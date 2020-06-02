###############################################################################
# Plot mortality ----
###############################################################################
# Load libraries ----
library(dplyr)
library(ggplot2)
  library(prismatic)
options(knitr.kable.NA = "--")

###############################################################################
# Load data ----

df <- readRDS(here::here("data/predicted-mortality.RDS"))
df_age <- readRDS(here::here("data/predicted-mortality-age-disaggregated.RDS"))

ifr <- readr::read_csv(here::here("data/age-specific-parameters.csv")) %>%
  tidyr::pivot_longer(cols = -age_group) %>%
  rename(var = name) %>%
  mutate(age_group = forcats::as_factor(age_group),
         age_group_lumped = forcats::fct_collapse(age_group,
                                                  "0-9" = c("0 to 4", "5 to 9"),
                                                  "10-19" = c("10 to 14", "15 to 19"),
                                                  "20-29" = c("20 to 24", "25 to 29"),
                                                  "30-39" = c("30 to 34", "35 to 39"),
                                                  "40-49" = c("40 to 44", "45 to 49"),
                                                  "50-59" = c("50 to 54", "55 to 59"),
                                                  "60-69" = c("60 to 64", "65 to 69"),
                                                  "70-79" = c("70 to 74", "75 to 79"),
                                                  "80+" = c("80+")),
          var_label = case_when(var == "proportion_infections_hospitalised" ~ "Proportion infections hospitalized",
                               var == "proportion_hospitalised_requiring_critical_care" ~ "Proportion hospitalized requiring critical care",
                               var == "proportion_non-critical_dying" ~ "Proportion hospitalized non-critical cases dying"),
         var_label = factor(var_label,
                            levels = c("Proportion infections hospitalized", "Proportion hospitalized non-critical cases dying", "Proportion hospitalized requiring critical care")))
         

###############################################################################

# Plot IFR -----
var_width = 35

ifr %>%
  mutate(var_label = stringr::str_wrap(var_label, width = var_width)) %>%
  mutate(var_label = factor(var_label, levels = c("Proportion infections hospitalized", "Proportion hospitalized non-\ncritical cases dying", "Proportion hospitalized requiring\ncritical care"))) %>%
  ggplot(., aes(x = age_group, y = value, group = var_label, color = var_label)) +
  geom_line(size = 1.1) +
  geom_point(alpha = .8, size = 1.5) +
  #ggrepel::geom_label_repel(data = filter(ifr, age_group == "40 to 44"), aes(label = var_label), nudge_y = -5) +
  facet_wrap(~var_label) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(breaks = levels(ifr$age_group)[c(TRUE, FALSE)], guide = guide_axis(n.dodge = 2)) +
  scale_color_hue() +
  labs(x = "Age Group") +
  theme_minimal() +
  theme(legend.position = "none", axis.title.y = element_blank(),
        axis.text.x = element_text(size = 8))

ggplot2::ggsave(here::here("fig/ifr.pdf"),
                device = "pdf", width = 8, height = 6)


# Mortality ----

c1 <- c("United States", "Japan", "Brazil",
        "India", "Bangladesh")

c2 <- c("United States", "South Africa", "Nigeria",
        "Indonesia", "Pakistan", "Nepal")


###############################################################################
# Mortality as fraction of population -----
###############################################################################
# Countries of interest
cc <- c("United States", "Japan", "Bangladesh", "Pakistan",
        "India", "Nigeria", "Mexico", "United Kingdom")

mortality_risk <- df %>% dplyr::filter(country %in% cc) %>%
  ggplot(., aes(x = strategy, y = total_deaths/pop, color = country, group = country, label = country)) +
  geom_point(size = 2, shape = 19) +
  geom_line(alpha = .75) +
  ggrepel::geom_label_repel(data = filter(df, country %in% cc & strategy == "Unmitigated"), force = 2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=5), labels = scales::percent_format(), position = "left") +
  scale_color_hue() +
  labs(#title = "How does COVID-19 mortality vary?",
    y = "Percent population lost") +
  theme_minimal() +
  theme(legend.position = "none", axis.title.x = element_blank())

ggplot2::ggsave(filename = here::here("fig/mortality-risk.pdf"),
                plot = mortality_risk, height = 5, width = 6, dpi = 1200)
ggplot2::ggsave(filename = here::here("fig/mortality-risk.png"),
                plot = mortality_risk, height = 5.625, width = 10, dpi = 900)


df_age %>% filter(country %in% c(cc, "Germany") & strategy == "Unmitigated") %>%
  ggplot(., aes(x = age_group, y = total_deaths/population, color = country, group = country, label = country)) +
  geom_point(size = 2, shape = 19) +
  geom_line(alpha = .75) +
  facet_wrap(~country) +
  scale_x_discrete(guide = guide_axis(n.dodge = 1)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=5), labels = scales::percent_format(), position = "left") +
  scale_color_hue() +
  labs(y = "Percent total population lost") +
  theme_minimal() +
  theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_text(size = 7, angle = 90, hjust = 1))


ggplot2::ggsave(filename = here::here("fig/mortality-risk-age.pdf"),
                plot = mortality_risk, height = 4.5, width = 4.5, dpi = 1200)


## Levels in a table
df %>% filter(Country %in% cc) %>% 
  mutate(percent_mortality = round((total_deaths/total_pop)*100, digits = 3)) %>%
  select(Country, strategy, percent_mortality) %>%
  tidyr::pivot_wider(names_from = "Country", values_from = "percent_mortality")

## Mortality perent by income block ----

inc_block <- df %>% 
  group_by(income_group, strategy) %>%
  summarize(total_deaths = sum(total_deaths),
            total_pop = sum(pop),
            percent_mortality = (total_deaths/total_pop))


ggplot(inc_block, aes(x = strategy, y = percent_mortality, color = income_group, group = income_group, label = income_group)) +
  geom_point(size = 2, shape = 19) +
  geom_line(alpha = .75) +
  ggrepel::geom_label_repel(data = filter(inc_block, strategy == "Unmitigated")) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_hue() +
  labs(y = "Percent population lost") +
  theme_minimal() +
  theme(legend.position = "none", axis.title.x = element_blank())

ggplot2::ggsave(here::here("fig/deaths-inc-bloc.pdf"), height = 5, width = 6, dpi = 1200)
ggplot2::ggsave(here::here("fig/deaths-inc-bloc.png"), height = 6, width = 6, dpi = 1200)  



