# Figure out trigger points
# Compare SQUIRE vs Initial
library(squire)
library(parallel)
  numCores <- detectCores()-2
library(dplyr)

df <- readRDS(here::here("data/suppression-triggers.RDS")) %>%
  arrange(country, strategy) %>%
  filter(country != "China, Taiwan Province of China") %>%
  mutate(country = case_when(country == "Congo, Dem. Rep." ~ "Democratic Republic of Congo",
                             country == "Congo, Rep." ~ "Republic of the Congo",
                             country == "Iran (Islamic Republic of)" ~ "Iran",
                             country == "Korea, Dem. People’s Rep." ~ "North Korea",
                             country == "Korea, Rep." ~ "South Korea",
                             country == "Micronesia (Fed. States of)" ~ "Micronesia",
                             country == "Russian Federation" ~ "Russia",
                             country == "Syrian Arab Republic" ~ "Syria",
                             country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
                             TRUE ~ country ))





triggers <- left_join(df, outcome, by =  c("country", "strategy"))


lockdown_effects <- bind_rows(mcmapply(grid_search,
                                       country = triggers$country, strategy = "Suppression",
                                       mitigation_day = triggers$time, mitigation_duration = 90,
                             SIMPLIFY = FALSE, mc.cores = numCores))


foo <- left_join(select(triggers, -strategy),
                 lockdown_effects, by = c("country", "time" = "mitigation_day")) %>%
  group_by(country) %>%
  slice(which.min(deaths))

