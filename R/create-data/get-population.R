# Get squire population ----

library(dplyr)

squire::population %>%
group_by(iso3c) %>%
summarize(pop = sum(n)) %>%
readr::write_csv(here::here("data/population.csv"))