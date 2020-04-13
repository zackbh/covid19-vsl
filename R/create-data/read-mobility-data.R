library(dplyr)
library(readxl)


df <- readxl::read_excel(path = here::here("data/raw-data/COVID_Community_Mobility_Reports_20200329.xlsx")) %>%
  janitor::clean_names() %>%
  mutate(country_code_iso3c = countrycode::countrycode(country_code, origin = "iso2c", destination = "wb"))

saveRDS(df, file = here::here("data/google-mobility.RDS"))