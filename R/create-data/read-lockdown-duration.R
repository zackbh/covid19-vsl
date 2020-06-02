# Read lockdown data ----
library(dplyr)
library(lubridate)


df <- readr::read_csv(here::here("data/raw-data/lockdown-duration.csv"),
                      col_names = c("country", "area", "start_date", "end_date", "level"), skip = 1) %>%
  filter(level == "National") %>%
  janitor::clean_names() %>%
  mutate(start_date = lubridate::ymd(stringr::str_sub(start_date, 1L, 10L)),
         end_date = lubridate::ymd(stringr::str_sub(end_date, 1L, 10L)),
         lockdown_duration = end_date - start_date)
  



