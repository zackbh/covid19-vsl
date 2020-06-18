###############################################################################
# Alternative parameterizations ----
#
# Load libraries
library(squire)
library(parallel)
numCores <- detectCores() - 1
library(dplyr)
library(ggplot2)


age_ifr <- readr::read_csv(here::here("data/age-specific-parameters.csv")) %>%
  janitor::clean_names() %>%
  mutate(x2 = vapply(proportion_non_critical_dying * 2, min, numeric(1), .90),
         x4 = vapply(proportion_non_critical_dying * 4, min, numeric(1), .90),
         x6 = vapply(proportion_non_critical_dying * 6, min, numeric(1), .90),
         x10 = vapply(proportion_non_critical_dying * 10, min, numeric(1), .90)) %>%
  select(age_group, proportion_non_critical_dying, x2, x4, x6,x10) 


cc <- c("United States", "United Kingdom", "Nigeria", "Pakistan", "Bangladesh",
        "Mexico")
c1 <- c("United States", "United Kingdom", "Indonesia", "Mexico",
        "India", "Bangladesh")
c2 <- c("United States","South Africa", "Nigeria",
        "Botswana", "Pakistan", "Nepal")


# Ran if 2x before, let's try 3, 4, 5, and 10x
mortality_increase <- c(4L, 6L, 10L)
init <- readRDS(here::here("data/predicted-mortality.RDS")) 
test <- tidyr::crossing(init, mortality_increase)


tictoc::tic()
alternative_outcomes <- bind_rows(mcmapply(grid_search,
                                          country = test$country, strategy = test$strategy,
                                          mitigation_day = test$mitigation_day, mitigation_duration = test$mitigation_duration,
                                          increased_mortality_pr = test$mortality_increase,
                                          SIMPLIFY = FALSE, mc.cores = numCores))
tictoc::toc() 


df <- bind_rows(init, alternative_outcomes) %>%
  group_by(country) %>%
  mutate(across(.cols = c(vsl, vsl_extrapolated, pop, gdp), ~mean(.x, na.rm = T))) %>%
  ungroup() %>%
  mutate(value_deaths = vsl*total_deaths,
         rel_value_deaths = value_deaths/gdp)




df %>% 
  filter(country %in% c("Bangladesh", "India", "Pakistan", "Nigeria", "United States", "United Kingdom",
                        "Mexico", "Indonesia", "South Africa")) %>%
  mutate(increased_mortality_pr = factor(paste0("x", increased_mortality_pr), levels = c("x2", "x4", "x6", "x10"))) %>%
ggplot(.,
       aes(x = strategy, y = rel_value_deaths, color = factor(increased_mortality_pr), group = increased_mortality_pr)) +
  geom_point() +
  geom_line() +
  facet_wrap(~country) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "VSL/GDP",
       color = stringr::str_wrap("Increase in mortality rate without hospitalization", width = 25)) +
  theme_minimal() +
  guides(col = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = -90, hjust = 0, vjust = 2),
        legend.position = "bottom", legend.direction = "horizontal")

ggsave(here::here("fig/value-mortality-diff.pdf"), device = "pdf", width = 5, height = 6)


age_ifr %>%
  rename(x1 = proportion_non_critical_dying) %>%
  tidyr::pivot_longer(-age_group) %>%
  mutate(age_group = forcats::as_factor(age_group),
         name = forcats::as_factor(name)) %>%
  ggplot(.,
       aes(x = age_group, y = value, group = name, color = name)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = .9, linetype = 3) +
  scale_y_continuous(limits = c(0,1), expand = c(NA,0),
                     breaks = c(.1,.25, .5, .75, .9), 
                     labels = scales::percent_format()) +
  scale_x_discrete(breaks = age_ifr$age_group[c(TRUE, FALSE)]) +
  scale_color_hue() +
  labs(x = "Age Group", y = "Mortality probability without hospitalization",
       color = stringr::str_wrap("Increase in likelihood of death", width = 12 )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = -45, hjust = 0))

ggsave(here::here("fig/change-mortality-age.pdf"), device = "pdf", width = 6, height = 5)



df %>% group_by(country) %>% mutate(income_group = unique(na.omit(income_group))) %>% ungroup() %>%
  mutate(increased_mortality_pr = forcats::as_factor(paste0("x",increased_mortality_pr))) %>%
  filter(!is.na(rel_value_deaths)) %>%
  ggplot(., aes(x = rel_value_deaths, fill = strategy)) +
  geom_histogram(bins = 50, color = "black", alpha = .8) +
  facet_grid(income_group  ~ increased_mortality_pr, scales = "free_y") +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(breaks = c(10,20,30)) +
  scale_fill_viridis_d(option = "A", direction = -1) +
  labs(x = "VSL/GDP", y = "Number of Countries") +
  guides(fill = guide_legend(nrow = 2)) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank(),
        axis.text.x = element_text(angle = -45, hjust = 0, vjust = 0, size = 7),
        strip.text = element_text(size = 7))

ggsave(here::here("fig/change-mortality-income_group.pdf"), device = "pdf", width = 6, height = 6)


df %>% group_by(country) %>% mutate(income_group = unique(na.omit(income_group))) %>% ungroup() %>%
  mutate(increased_mortality_pr = forcats::as_factor(paste0("x",increased_mortality_pr))) %>%
  group_by(income_group, strategy, increased_mortality_pr) %>%
  summarize(avg_loss = weighted.mean(rel_value_deaths, w= pop, na.rm = T)) %>%
  mutate(diff = lag(avg_loss) - avg_loss) %>% ungroup() %>%
  arrange(diff) %>%
  head(20)

  View()
