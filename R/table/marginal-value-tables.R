# Tables
library(dplyr)
options(knitr.kable.NA = "--")

df <- readRDS(here::here("data/predicted-mortality.RDS"))
df_age <- readRDS(here::here("data/predicted-mortality-age.RDS"))

cc <- c("United States", "Mexico",
        "India", "Bangladesh", "South Africa", "Nigeria",
        "Pakistan", "Nepal", "United Kingdom")



df %>% filter(country %in% cc) %>%
  select(country, strategy, marginal_rel_value_deaths) %>%
  mutate(marginal_rel_value_deaths = round(marginal_rel_value_deaths * 100, digits = 1)) %>%
  tidyr::pivot_wider(names_from = country, values_from = marginal_rel_value_deaths) %>%
  dplyr::relocate(strategy,
                  `United Kingdom`, `United States`, Mexico, `South Africa`, India,
                  Bangladesh, Pakistan, Nigeria, Nepal) %>%
  knitr::kable(format = "latex", booktabs = T, digits = 1, label = "marginal-value-vsl",
               col.names = c("Strategy",
                             "UK", "US", "Mexico", "S. Africa", "India",
                             "Bangladesh", "Pakistan", "Nigeria", "Nepal"),
               caption = "Marginal Value of COVID-19 Interventions ($\\Delta$ VSL/GDP)") %>%
  kableExtra::kable_styling(full_width = F) %>%
  kableExtra::add_header_above(c(" " = 1, "High" = 2, "Upper-Middle" = 2, "Lower-Middle" = 4, "Low" = 1)) %>%
  kableExtra::add_header_above(c(" " = 1, "Income Group" = 9)) %>%
  cat(., file = here::here("tab/marginal-value-vsl.tex"))



df_age %>% filter(country %in% cc) %>%
  select(country, strategy, value_years, gdp) %>%
  group_by(country) %>% mutate(marginal_value = lag(value_years) - value_years,
                               marginal_rel_value = (marginal_value/gdp)*100) %>% ungroup() %>%
  select(country, strategy, marginal_rel_value) %>%
  tidyr::pivot_wider(names_from = country, values_from = marginal_rel_value) %>%
  dplyr::relocate(strategy,
                  `United Kingdom`, `United States`, Mexico, `South Africa`, India,
                  Bangladesh, Pakistan, Nigeria, Nepal) %>%
  knitr::kable(format = "latex", booktabs = T, digits = 1, label = "marginal-value-vsl",
               col.names = c("Strategy",
                             "UK", "US", "Mexico", "S. Africa", "India",
                             "Bangladesh", "Pakistan", "Nigeria", "Nepal"),
               caption = "Marginal Value of COVID-19 Interventions ($\\Delta$ VSLY/GDP)") %>%
  kableExtra::kable_styling(full_width = F) %>%
  kableExtra::add_header_above(c(" " = 1, "High" = 2, "Upper-Middle" = 2, "Lower-Middle" = 4, "Low" = 1)) %>%
  kableExtra::add_header_above(c(" " = 1, "Income Group" = 9)) %>%
  cat(., file = here::here("tab/marginal-value-vsly.tex"))



