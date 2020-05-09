# Get life tables -----

files <- list.files(here::here("data/raw-data/life-tables/"), pattern = "*.csv")

read_lifetables <- function(filename){

  years <- seq.int(2016, 2000, by = -1)
  
    
  df <- readr::read_csv(here::here(paste0("data/raw-data/life-tables/",filename)),
                        col_names = c("indicator", "age_group",
                                      paste0(rep(c("male_", "female_"), times = 5, each = 1), rep(years, each = 2))),
                        skip = 2,
                        col_types = paste(c(rep("c",2),rep("d", 34)), collapse= "") )
  
  out <- df %>% select(indicator, age_group, male_2016, female_2016) %>%
    filter(indicator == "ex - expectation of life at age x") %>%
    mutate(age_group = case_when(age_group %in% c("&lt;1 year", "<1 year", "1-4 years" )~ "0-4",
                                 age_group == "5-9 years" ~ "5-9",
                                 age_group == "10-14 years" ~ "10-14",
                                 age_group == "15-19  years" ~ "15-19",
                                 age_group == "20-24 years" ~ "20-24",
                                 age_group == "25-29 years" ~ "25-29",
                                 age_group == "30-34 years" ~ "30-34",
                                 age_group == "35-39 years" ~ "35-39",
                                 age_group == "40-44 years" ~ "40-44",
                                 age_group == "45-49 years" ~ "45-49",
                                 age_group == "50-54 years" ~ "50-54",
                                 age_group == "55-59 years" ~ "55-59",
                                 age_group == "60-64 years" ~ "60-64",
                                 age_group == "65-69 years" ~ "65-69",
                                 age_group == "70-74 years" ~ "70-74",
                                 age_group == "75-79 years" ~ "75-79",
                                 age_group == "80-84 years" ~ "80+",
                                 age_group == "85+ years" ~ "80+"),
           indicator = "life_expectancy") %>%
    rowwise() %>%
    mutate(avg_life_expectancy = mean(c(male_2016, female_2016))) %>%
    group_by(age_group) %>%
    summarize(life_expectancy = mean(avg_life_expectancy)) %>% ungroup() %>%
    mutate(country = stringr::str_split(filename, pattern = ".csv")[[1]][1],
           country_code = countrycode::countrycode(country, origin = "country.name", destination = "wb"),
           age_group = factor(age_group,
                              labels = c("0-4", "5-9", "10-14", "15-19",
                                         "20-24", "25-29", "30-34", "35-39",
                                         "40-44", "45-49", "50-54", "55-59",
                                         "60-64", "65-69", "70-74", "75-79",
                                         "80+")))
  
  
  return(out)
  
}

df <- purrr::map_dfr(.x = files, .f = read_lifetables)

saveRDS(df, file = here::here("data/life-tables.RDS"))
