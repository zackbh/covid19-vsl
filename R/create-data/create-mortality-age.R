# Read in mortality by age data ----
library(dplyr)
library(readxl)
library(ggplot2)

# total_pop = total_pop * 1e3,
mortality_predictions <- readxl::read_excel(here::here("data/raw-data/Global_unmitigated_and_mitigated_epidemics_by_age.xlsx")) %>% 
  mutate(country_code = countrycode::countrycode(Country, origin = "country.name", destination = "wb")) %>%
  left_join(x = ., y = readr::read_csv(here::here("data/vsl.csv")), by = "country_code") %>%
  left_join(x = ., y = readr::read_csv(here::here("data/wb_data.csv")), by = c("country_code" = "iso3c")) %>%
  mutate(strategy = case_when(Strategy == "Unmitigated" ~ "Unmitigated",
                              Strategy == "Social distancing whole population" ~ "Social distancing",
                              Strategy == "Enhanced social distancing of elderly" ~ "Social distancing+"),
         strategy = factor(strategy,
                           levels = c("Unmitigated",
                                      "Social distancing", "Social distancing+"),
                           ordered = T))



df <- mortality_predictions %>%
  mutate(years_lost = ((life_expectancy - 2.5) * deaths_0_5) +
                      ((life_expectancy - 7.5) * deaths_5_10) +
                      ((life_expectancy - 12.5) * deaths_10_15) +
                      ((life_expectancy - 17.5) * deaths_15_20) +
                      ((life_expectancy - 22.5) * deaths_20_25) +
                      ((life_expectancy - 27.5) * deaths_25_30) +
                      ((life_expectancy - 32.5) * deaths_30_35) +
                      ((life_expectancy - 37.5) * deaths_35_40) +
                      ((life_expectancy - 42.5) * deaths_40_45) +
                      ((life_expectancy - 47.5) * deaths_45_50) +
                      (max(1, (life_expectancy - 52.5), na.rm = T) * deaths_50_55) +
                      (max(1, (life_expectancy - 57.5), na.rm = T) * deaths_55_60) +
                      (max(1, (life_expectancy - 62.5), na.rm = T) * deaths_60_65) +
                      (max(1, (life_expectancy - 67.5), na.rm = T) * deaths_65_70) +
                      (max(1, (life_expectancy - 72.5), na.rm = T) * deaths_70_75) +
                      (max(1, (life_expectancy - 77.5), na.rm = T) * deaths_75_80) +
                      (max(1, (life_expectancy - 80), na.rm = T) * deaths_80_200)) %>%
   mutate(years_lost = years_lost * 1e3,
          vsl_year = (vsl*1e6)/life_expectancy,
          value_years = years_lost * vsl_year,
          value_years_gdp = (value_years/gdp)) 




ggplot(dplyr::filter(df, Country %in% c("United States", "Japan", "Germany", "Bangladesh", "India")
                     & R0 == 3), 
       aes(x = strategy, y = value_years_gdp, group = Country, color = Country, label = Country)) +
  geom_point() +
  geom_line() +
  ggrepel::geom_label_repel(data = filter(df, strategy == "Unmitigated" & R0 == 3 & Country %in% c("United States", "Japan", "Germany", "Bangladesh", "India"))) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  labs(y = "Value of Statistical Life-Years/GDP") +
  theme(legend.position = "none", axis.title.x = element_blank())


ggplot(dplyr::filter(df, Country %in% c("United States", "Japan", "Germany", "Bangladesh", "India")
                     & R0 == 3), 
       aes(x = strategy, y = ((total_deaths * vsl * 1e6 *1e3)/gdp), group = Country, color = Country, label = Country)) +
  geom_point() +
  geom_line() +
  ggrepel::geom_label_repel(data = filter(df, strategy == "Unmitigated" & R0 == 3 & Country %in% c("United States", "Japan", "Germany", "Bangladesh", "India"))) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  labs(y = "Value of Statistical Life/GDP") +
  theme(legend.position = "none", axis.title.x = element_blank())


ggplot(dplyr::filter(df, Country %in% c("United States", "Japan", "Germany", "Bangladesh", "India")
                     & R0 == 3), 
       aes(x = strategy, y = (total_deaths * vsl * 1e6)/gdp, group = Country, color = Country, label = Country)) +
  geom_point() +
  geom_line() +
  ggrepel::geom_label_repel(data = filter(df, strategy == "Unmitigated" & R0 == 3 & Country %in% c("United States", "Japan", "Germany", "Bangladesh", "India"))) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()
  


%>%
   select(Country, years_lost, life_expectancy, vsl_year, value_years, value_years_gdp) %>%
   arrange(desc(value_years_gdp))
           
  
  
  
  mutate(paste("deaths",seq.int(0,80, by =5), sep = "_") = deaths_0_5:deaths_80_200)

df <- for (i in seq.int(1, length(string))){
  
  mortality_predictions[,low[i]] <- mortality_predictions[, string[i]] 
  
}

string <- c("deaths_0_5", "deaths_5_10", "deaths_10_15",
            "deaths_15_20", "deaths_20_25", "deaths_25_30", "deaths_30_35",
            "deaths_35_40", "death")

low <- paste("deaths",seq.int(0,80, by =5), sep = "_")






# Check the data ----

c <- mortality_predictions %>%
  rowwise() %>%
  mutate(check_pop = 1e3 * sum(c_across(pop_0_5:pop_80_200)),
         check_deaths = 1e3*sum(c_across(deaths_0_5:deaths_80_200)),
         fraction_mortality = check_deaths/(total_pop)) %>%
  select(Country, total_pop, check_pop, check_deaths, fraction_mortality)



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

