# Get stringency data ----
## Oxford Covid-19 Government Response Tracker (OxCGRT)
## https://github.com/OxCGRT/covid-policy-tracker

library(dplyr)

github_url <- "https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"

df <- readr::read_csv(github_url) %>% janitor::clean_names() %>%
  mutate(date = lubridate::ymd(date)) %>%
  select(country_name, country_code, date, stringency_index, government_response_index, containment_health_index, economic_support_index)

saveRDS(df, here::here("data/stringency-index.RDS"))


