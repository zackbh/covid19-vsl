library(readxl)
library(dplyr)
library(countrycode)

cum.na <- function(x) { 
    x[which(is.na(x))] <- 0 
    return(cumsum(x)) 
} 

# Mitigation and suppression strategies on separate sheets of the Excel file ----
mitigation <- readxl::read_excel(here::here("data/raw-data/Imperial-College-COVID19-Global-unmitigated-mitigated-suppression-scenarios.xlsx"),
                                 sheet = "Mitigation") %>%
  mutate(strategy = case_when(Strategy == "Unmitigated" ~ "Unmitigated",
                              Strategy == "Social distancing whole population" ~ "Social distancing",
                              Strategy == "Enhanced social distancing of elderly" ~ "Social distancing+"))


suppression <- readxl::read_excel(here::here("data/raw-data/Imperial-College-COVID19-Global-unmitigated-mitigated-suppression-scenarios.xlsx"),
                                  sheet = "Suppression") %>%
  mutate(strategy = case_when(Strategy == "Unmitigated" ~ "Unmitigated",
                              Strategy == "0.2 deaths per 100,000 per week trigger" ~ "Early suppression",
                              Strategy == "1.6 deaths per 100,000 per week trigger" ~ "Late suppression")) %>%
  filter(Strategy != "Unmitigated")  # Remove duplicated mitigation estimates, social distancing parameter is incorrect here

# Bind into single mortality dataframe ----
mortality <- bind_rows(mitigation, suppression) %>%
  mutate(country_code = countrycode::countrycode(Country, origin = 'country.name', destination = 'wb'),
        region = countrycode::countrycode(country_code, origin = 'wb', destination = 'region'),
        continent = countrycode::countrycode(country_code, origin = 'wb', destination = 'continent'),
         R0 = factor(R0, levels = c(2.4, 2.7, 3, 3.3), ordered = T),
         strategy = factor(strategy,
                           levels = c("Unmitigated",
                                      "Social distancing", "Social distancing+", 
                                      "Late suppression", "Early suppression"),
                           ordered = T)) %>%
  filter(!is.na(country_code))


df <- left_join(mortality, readr::read_csv(here::here("data/vsl.csv")),
                by = "country_code") %>%
  left_join(., readr::read_csv(here::here("data/wb_data.csv")), by = c("country_code" = "iso3c")) %>%
  left_join(., readRDS(file = here::here("data/oecd-countries.RDS")), by = c("country_code" = "oecd_code")) %>%
  mutate(broad_region = case_when(region %in% c("Eastern Africa", "Middle Africa",
                                                "Southern Africa", "Western Africa") ~ "Sub-Saharan Africa",
                                  !is.na(oecd_name) ~ "OECD",
                                  (Country == "Mexico") | (region %in% c("Caribbean", "Central America",
                                                                      "South America")) ~ "LAC",
                                  TRUE ~ region)) %>%
  mutate(income_block = factor(income_block,
                               levels = c("Low income", "Lower-middle income", "Upper-middle income", "Upper income"),
                               ordered = T)) %>%
  mutate(risk = total_deaths/total_pop,
         value_deaths = vsl * total_deaths*1000000, # dollars
         relative_value_gdp = (value_deaths)/gdp,
         relative_value_gni = (value_deaths)/gni) %>%  # $ VSL/$ GDP
  arrange(Country, R0, strategy) %>%
  group_by(Country, R0) %>%
  mutate(marginal_value = dplyr::lag(value_deaths) - value_deaths,
         cumulative_value = cum.na(coalesce(marginal_value))) %>%
  ungroup()


# Create regional summaries


oecd <- filter(df, !is.na(oecd_name)) %>%
  group_by(strategy, R0) %>%
  summarize(Country = "OECD",
            region = "OECD",
            gdp = weighted.mean(gdp, w = total_pop),
            gni = weighted.mean(gni, w = total_pop),
            vsl = weighted.mean(vsl, w = total_pop),
            total_deaths = sum(total_deaths),
            total_pop = sum(total_pop),
            value_deaths = vsl * total_deaths * 1000000
            )

ssa <- filter(df, broad_region == "Sub-Saharan Africa") %>%
  group_by(strategy, R0) %>%
  summarize(Country = "Sub-Saharan Africa",
            region = "Sub-Saharan Africa",
            gdp = weighted.mean(gdp, w = total_pop),
            gni = weighted.mean(gni, w = total_pop),
            vsl = weighted.mean(vsl, w = total_pop),
            total_deaths = sum(total_deaths),
            total_pop = sum(total_pop),
            value_deaths = vsl * total_deaths * 1000000
  )

lac <- filter(df, broad_region == "LAC") %>%
  group_by(strategy, R0) %>%
  summarize(Country = "LAC",
            region = "LAC",
            gdp = weighted.mean(gdp, w = total_pop),
            gni = weighted.mean(gni, w = total_pop),
            vsl = weighted.mean(vsl, w = total_pop),
            total_deaths = sum(total_deaths),
            total_pop = sum(total_pop),
            value_deaths = vsl * total_deaths * 1000000
  )


df <- bind_rows(df, oecd, ssa, lac)

df <- left_join(df, readRDS(here::here("data/google-mobility.RDS")),
                by = c("country_code" = "country_code_iso3c"))

saveRDS(df, here::here("data/combined-data.RDS"))






