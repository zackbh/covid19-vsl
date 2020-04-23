# Plot VSL ----
# Load libraries ----
library(dplyr)
library(ggplot2)
library(prismatic)
options(knitr.kable.NA = "--")

###############################################################################
# Load data ----
df <- readRDS(here::here("data/combined-data.RDS")) %>%
  filter(R0 == 3)

cc <- c("United States", "Japan", "Nigeria", "Pakistan", "Bangladesh")

c1 <- c("United States", "Japan", "Brazil",
        "India", "Bangladesh")

c2 <- c("United States", "South Africa", "Nigeria",
        "Indonesia", "Pakistan", "Nepal")

###############################################################################
# VSL in levels ----
###############################################################################


df %>% filter(Country %in% cc) %>%
  ggplot(., aes(x = strategy, y = value_deaths/1e9, color = Country, group = Country, label = Country)) +
  geom_point(size = 2, shape = 19) +
  geom_line(alpha = .8) +
  ggrepel::geom_label_repel(data = filter(df, Country %in% cc & strategy == "Unmitigated"),
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


vsl_gdp_a <- df %>% filter(Country %in% c1) %>%
  ggplot(., aes(x = strategy, y = value_deaths/gdp, color = Country, group = Country, label = Country)) +
  geom_point(size = 2, shape = 19) +
  geom_line(alpha = .75) +
  ggrepel::geom_label_repel(data = filter(df, Country %in% c1 & strategy == "Unmitigated"),
                            force = 2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(#title = "How does COVID-19 mortality vary?",
    y = "VSL Lost/GDP") +
  theme_minimal() +
  theme(legend.position = "none", axis.title.x = element_blank())

vsl_gdp_b <- df %>% filter(Country %in% c2) %>%
  ggplot(., aes(x = strategy, y = value_deaths/gdp, color = Country, group = Country, label = Country)) +
  geom_point(size = 2, shape = 19) +
  geom_line(alpha = .75) +
  ggrepel::geom_label_repel(data = filter(df, Country %in% c2 & strategy == "Unmitigated"),
                            force = 3) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(labels = scales::percent_format(), position = "left") +
  labs(#title = "How does COVID-19 mortality vary?",
    y = "VSL Lost/GDP") +
  theme_minimal() +
  theme(legend.position = "none", axis.title.x = element_blank())


ggsave(filename =  here::here("fig/vsl-gdp-a.pdf"),  plot = vsl_gdp_a, width = 4.5, height = 4.5,)
ggplot2::ggsave(filename = here::here("fig/vsl-gdp-a.png"), plot = vsl_gdp_a,
                height = 5.625, width = 10, dpi = 900)



ggsave(filename = here::here("fig/vsl-gdp-b.pdf"), plot = vsl_gdp_b, width = 4.5, height = 4.5)
ggplot2::ggsave(filename = here::here("fig/vsl-gdp-b.png"), plot = vsl_gdp_b,
                height = 5.625, width = 10, dpi = 900)

# VSL by income bloc ---

# Plot VSL ----


income_bloc <- out %>% filter(!is.na(income_block)) %>%
  group_by(income_block, strategy) %>%
  summarize(avg_vsl = weighted.mean(vsl, w = pop, na.rm = T),
            deaths = sum(deaths),
            gdp = sum(gdp),
            avg_losses = (avg_vsl*deaths*1e6)/gdp)


ggplot(income_bloc, aes(x = strategy, y = avg_losses, color = income_block, group = income_block, label = income_block)) +
  geom_point(size = 2, shape = 19) +
  geom_line(alpha = .75) +
  ggrepel::geom_label_repel(data=filter(income_bloc, strategy == "Unmitigated")) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_hue() + 
  labs(y = "Avg VSL Lost/GDP") +
  theme_minimal() +
  theme(legend.position = "none", axis.title.x = element_blank())

ggsave(here::here("fig/vsl-income-block.pdf"), width = 5, height = 5, dpi = 1200)



## Table of marginal value of each intervention ----
# Using dplyr 1.0.0

df %>% filter(Country %in% c("Japan", "United Kingdom", "United States", "Brazil",
                             "Indonesia", "South Africa", "Bangladesh", "Nigeria")) %>%
  dplyr::mutate(marginal_value_gdp = (marginal_value/gdp)*100) %>%
  dplyr::select(Country, strategy, marginal_value_gdp) %>%
  tidyr::pivot_wider(names_from = Country, values_from = marginal_value_gdp) %>%
  dplyr::relocate(strategy, Japan, `United Kingdom`, `United States`, Brazil, Indonesia, `South Africa`) %>%
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
