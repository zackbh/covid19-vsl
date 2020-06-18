###############################################################################
# Read lockdown data ----
library(dplyr)
library(lubridate)

# How long were lockdowns on average? -----
# Using Wikipedia data by country
## https://en.wikipedia.org/wiki/COVID-19_pandemic_lockdowns#Table_of_pandemic_lockdowns
## Current as of 2020-06-18


df <- readr::read_csv(here::here("data/raw-data/lockdown-duration.csv"),
                      col_names = c("country", "area", "start_date", "end_date", "level"), skip = 0) %>%
  filter(level == "National") %>%
  janitor::clean_names() %>%
  mutate(start_date = lubridate::ymd(stringr::str_sub(start_date, 1L, 10L)),
         end_date = lubridate::ymd(stringr::str_sub(end_date, 1L, 10L)),
         lockdown_duration = end_date - start_date)
  

# How long is the average and median lockdown? ----

mean(df$lockdown_duration, na.rm = T)
median(df$lockdown_duration, na.rm = T)

## 43 and 38 days, respectively

