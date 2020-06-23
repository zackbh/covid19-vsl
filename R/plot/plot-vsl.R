################################################################################
# Plot VSL ----
## Differing measures between Masterman and Viscusi and Robinson et al.

################################################################################
# Load libraries ----
library(dplyr)
library(ggplot2)
library(prismatic)
options(knitr.kable.NA = "--")

###############################################################################
# Load data ----
df <- readRDS(here::here("data/predicted-mortality.RDS"))
df_age <- readRDS(here::here("data/predicted-mortality-age.RDS"))
vsl <- readr::read_csv(here::here("data/vsl.csv"))

cc <- c("United States", "United Kingdom", "Nigeria", "Pakistan", "Bangladesh", "Mexico")

c1 <- c("United States", "United Kingdom", "Indonesia", "Mexico",
        "India", "Bangladesh")

c2 <- c("United States","South Africa", "Nigeria",
        "Botswana", "Pakistan", "Nepal")


# Comparing VSL measures ----

vsl %>% filter(!is.na(vsl_extrapolated)) %>%
  mutate(label = ifelse(country %in% c(cc, c1, c2, "Turkey", "Argentina"),
                        country, "")) %>%
  mutate(bigger = if_else(vsl_extrapolated > vsl, TRUE, FALSE)) %>%
  ggplot(., aes(x = vsl, y = vsl_extrapolated, fill = bigger)) +
  geom_point(aes(color = after_scale(prismatic::clr_darken(fill, .45))), size = 2, shape = 21, alpha = .7) +
  geom_abline(slope = 1, linetype = 3) +
  ggrepel::geom_label_repel(aes(label = label), box.padding = .35, force = 2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n =5), labels = scales::dollar_format(), limits = c(0,3*1e6)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n =5), labels = scales::dollar_format(), limits = c(0,3*1e6)) +
  scale_color_hue() +
  labs(x = "VSL Estimates (Viscusi and Masterman 2017)", y = "VSL Estimates (Robinson et al. 2019)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(here::here("fig/vsl-comparison.pdf"), width = 6, height = 5)


###############################################################################
# VSL in levels for total deaths ----
###############################################################################


df %>% filter(country %in% cc) %>%
  ggplot(., aes(x = strategy, y = value_deaths/1e9, color = country, group = country, label = country)) +
  geom_point(size = 2, shape = 19) +
  geom_line(alpha = .8) +
  ggrepel::geom_label_repel(data = filter(df, country %in% cc & strategy == "Unmitigated"),
                            force = 5,
                            nudge_y = 1000) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(#title = "How does COVID-19 mortality vary?",
    y = "Total VSL Lost (billion USD)") +
  theme_minimal() +
  theme(legend.position = "none", axis.title.x = element_blank())

ggsave(here::here("fig/vsl-levels.pdf"), width = 5, height = 5, dpi = 1200)
ggplot2::ggsave(filename = here::here("fig/vsl-levels.png"),
                height = 5.625, width = 10, dpi = 900)

###############################################################################
# As fraction of GDP ----
###############################################################################


vsl_gdp_a <- df %>% filter(country %in% c1) %>%
  ggplot(., aes(x = strategy, y = value_deaths/gdp, color = country, group = country, label = country)) +
  geom_point(size = 2, shape = 19) +
  geom_line(alpha = .75) +
  ggrepel::geom_label_repel(data = filter(df, country %in% c1 & strategy == "Unmitigated"),
                            force = 2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(breaks = c(.5,1,1.5,2, 2.5), labels = scales::percent_format(), limits = c(.4,2.6)) +
  labs(y = "VSL Lost/GDP") +
  theme_minimal() +
  theme(legend.position = "none", axis.title.x = element_blank())

vsl_gdp_b <- df %>% filter(country %in% c2) %>%
  ggplot(., aes(x = strategy, y = value_deaths/gdp, color = country, group = country, label = country)) +
  geom_point(size = 2, shape = 19) +
  geom_line(alpha = .75) +
  ggrepel::geom_label_repel(data = filter(df, country %in% c2 & strategy == "Unmitigated"),
                            force = 3) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(breaks = c(.5,1,1.5,2, 2.5), labels = scales::percent_format(), limits = c(.4,2.6), position = "left") +
  labs(y = "VSL Lost/GDP") +
  theme_minimal() +
  theme(legend.position = "none", axis.title.x = element_blank())


ggsave(filename =  here::here("fig/vsl-gdp-a.pdf"),  plot = vsl_gdp_a, width = 5, height = 6)
ggplot2::ggsave(filename = here::here("fig/vsl-gdp-a.png"), plot = vsl_gdp_a,
                height = 5.625, width = 10, dpi = 900)

ggsave(filename = here::here("fig/vsl-gdp-b.pdf"), plot = vsl_gdp_b, width = 5, height = 6)
ggplot2::ggsave(filename = here::here("fig/vsl-gdp-b.png"), plot = vsl_gdp_b,
                height = 5.625, width = 10, dpi = 900)

# VSL by income bloc ---

income_bloc <- df %>%
  group_by(strategy, income_group) %>%
  summarize(losses = sum(value_deaths)/sum(gdp, na.rm = T))

ggplot(income_bloc, aes(x = strategy, y = losses, color = income_group, group = income_group, label = income_group)) +
  geom_point(size = 2, shape = 19) +
  geom_line(alpha = .75) +
  ggrepel::geom_label_repel(data=filter(income_bloc, strategy == "Unmitigated")) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_hue() + 
  labs(y = "VSL Lost/GDP") +
  theme_minimal() +
  theme(legend.position = "none", axis.title.x = element_blank())

ggsave(here::here("fig/vsl-income-block.pdf"), width = 5, height = 6, dpi = 1200)
ggsave(here::here("fig/vsl-income-block.png"), width = 6, height = 6, dpi = 1200)




# Alternative VSL ---

cc_alt <- c("Indonesia", "Mexico", "India", "Bangladesh", "South Africa",
            "Nigeria", "Botswana", "Pakistan", "Nepal")

df %>% filter(country %in% cc_alt) %>%
select(country, strategy, value_deaths, value_deaths_extrapolated, gdp) %>%
tidyr::pivot_longer(cols = c(value_deaths, value_deaths_extrapolated), names_to = "vsl") %>%
mutate(alpha_val = I(ifelse(vsl == "value_deaths_extrapolated", 1.0, .35))) %>%
  ggplot(., aes(x = strategy, y = value/gdp,
    color = vsl, alpha = alpha_val, group = vsl)) +
  geom_point(size = .9) +
  geom_line() +
  facet_wrap(~country) +
  scale_x_discrete(guide = guide_axis(n.dodge = 1)) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_hue(labels = c("Masterman and Viscusi 2017", "Robinson et al. 2019")) +
  labs(y = "VSL Lost/GDP") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

ggsave(filename =  here::here("fig/vsl-gdp-alt.pdf"),  width = 5, height = 6)



df %>% filter(!is.na(vsl_extrapolated)) %>%
  ggplot(., aes(y = strategy, x = value_deaths/gdp - value_deaths_extrapolated/gdp, fill = ..y..)) +
  ggridges::geom_density_ridges(stat = "binline", bins = 60, scale = 0.95, draw_baseline = FALSE) +
  geom_vline(xintercept = 0)+
  scale_x_continuous(limits = c(-1, 1), labels = scales::percent_format())+
  scale_y_discrete(guide = guide_axis(n.dodge = 1)) +
  labs(x = "Difference in relative value of interevention") +
  theme_minimal() +
  theme(axis.title.y = element_blank(), legend.position = "none")

ggsave(filename =  here::here("fig/vsl-diff-alt.pdf"),  width = 5, height = 6)
  

df %>% filter(!is.na(vsl_extrapolated)) %>% 
  mutate(diff = value_deaths/gdp - value_deaths_extrapolated/gdp)  %>%
  group_by(country, strategy) %>%
  summarize(avg = mean(diff)) %>%
  .[,3] %>%
  colMeans(., na.rm = T)

df %>% filter(!is.na(vsl_extrapolated)) %>%
  select(vsl, vsl_extrapolated) %>%
  summarize_all(mean) %>% ungroup() %>%
  .[,c(2,3)] %>% cor()
  


## Table of marginal value of each intervention ----
# Using dplyr 1.0.0

df %>% filter(country %in% c("Japan", "United Kingdom", "United States", "Mexico",
                             "Indonesia", "South Africa", "Bangladesh", "Nigeria")) %>%
  dplyr::mutate(marginal_value_gdp = (marginal_value/gdp)*100) %>%
  dplyr::select(country, strategy, marginal_value_gdp) %>%
  tidyr::pivot_wider(names_from = country, values_from = marginal_value_gdp) %>%
  dplyr::relocate(strategy, Japan, `United Kingdom`, `United States`, Mexico, Indonesia, `South Africa`) %>%
  knitr::kable(format = "latex", booktabs = T, digits = 0, label = "marginal-value-intervention",
               col.names = c("Strategy", "Japan", "UK", "US", "Brazil",
                             "Indonesia", "S. Africa", "Bangladesh","Nigeria"),
               caption = "Marginal Value of COVID-19 Interventions Relative to Unmitigated Scenario (total VSL/GDP)") %>%
  kableExtra::kable_styling(full_width = F) %>%
  kableExtra::add_header_above(c(" " = 1, "Upper Income" = 3, "Upper-Middle Income" = 2, "Lower-Middle Income" =3)) %>%
  cat(., file = here::here("tab/marginal-value-gdp.tex"))



# Get values for policy brief ----

df %>% filter(Country %in% c("United States", "Germany", "Nigeria", "Pakistan",
                             "Bangladesh", "India")) %>%
  filter(R0 == 3) %>%
  filter(strategy == "Social distancing") %>%
  mutate(marginal_value_gdp = marginal_value / gdp) %>%
  select(Country, marginal_value, gdp, marginal_value_gdp, Country, strategy) %>%
  kableExtra::kable()
