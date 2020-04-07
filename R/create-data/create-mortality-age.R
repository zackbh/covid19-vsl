# Read in mortality by age data ----
library(dplyr)
library(readxl)


mortality_predictions <- readxl::read_excel(here::here("data/raw-data/Global_unmitigated_and_mitigated_epidemics_by_age.xlsx")) %>% 
  mutate(total_pop = total_pop * 1e3,
          country_code = countrycode::countrycode(Country, origin = "country.name", destination = "wb")) %>%
  left_join(x = ., y = readr::read_csv(here::here("data/vsl.csv")), by = "country_code")

# Check the data ----

c <- mortality_predictions %>%
  rowwise() %>%
  mutate(check_pop = sum(c_across(pop_0_5:pop_80_200)),
         check_deaths = 1e6*sum(c_across(deaths_0_5:deaths_80_200)),
         fraction_mortality = check_deaths/(total_pop*1e3))



boo <- left_join(df, select(c, Country, check_deaths, fraction_mortality), by = "Country")

plot(boo$total_deaths/boo$total_pop, boo$fraction_mortality)

hist(c$fraction_mortality)


(c$total_pop[1] - (c$pop_0_5[1] + c$pop_5_10[1] + c$pop_10_15[1] +
                     c$pop_15_20[1] + c$pop_20_25[1] + c$pop_25_30[1] + 
                    c$pop_30_35[1] + c$pop_35_40[1] + c$pop_40_45[1] +
                    c$pop_45_50[1] + c$pop_50_55[1] + c$pop_55_60[1] +
                    c$pop_60_65[1] + c$pop_65_70[1] + c$pop_70_75[1] +
                     c$pop_75_80[1] + c$pop_80_200[1]))




foo %>% rowwise()

