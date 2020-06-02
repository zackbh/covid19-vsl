# Compare SQUIRE vs Initial
# Load libraries-----
library(squire)
library(parallel)
  numCores <- detectCores() - 1
library(dplyr)
library(ggplot2)

source(here::here("R/create-data/squire-functions/squire-functions.R"))

vsl <- readr::read_csv(here::here("data/vsl.csv"))
wb <- readRDS(here::here("data/wb-data.RDS"))
life_tables <- readRDS(here::here("data/life-tables.RDS"))
df <- squire::population %>%  # Start with Squire built-in populations
  # Total population across all ages
  group_by(country) %>% summarize(pop = sum(n)) %>% ungroup() %>% 
  # Add in country code
  mutate(iso3c = countrycode::countrycode(country, origin = "country.name", destination = "wb")) %>%
  # Add in VSL and remove observations without VSL estimates
  left_join(select(vsl, country_code, vsl_gni_cap, vsl, vsl_160, vsl_100, vsl_extrapolated), by = c("iso3c" = "country_code")) %>%
  filter(!is.na(vsl)) %>%
  # Add World Bank data
  left_join(select(wb, iso3c, gdp, vulnerable_employment, income_group), by = "iso3c")

###############################################################################
# Calculate unmitigated deaths ----
unmitigated_outcomes <- bind_rows(mcmapply(grid_search, country = df$country, strategy = "Unmitigated", mitigation_day = 50,
                        SIMPLIFY = FALSE, mc.cores = numCores))

###############################################################################
# Find optimal timing for social distancing policies -----
###############################################################################

strategy <- c("Individual distancing", "Social distancing", "Social distancing+", "Suppression")
starting_dates <- seq.int(from = 35, to = 95, by = 3)  # Worked at 5 67 minutes at 3 days
# duration <- seq.int(from = 30, to = 45, by = 5)  # Takes too long to run
search_parameters <- tidyr::crossing(country = df$country, strategy, starting_dates) 

tictoc::tic()
distancing_outcomes <- bind_rows(mcmapply(grid_search,
                                country = search_parameters$country, strategy = search_parameters$strategy, mitigation_day = search_parameters$starting_dates,
                                increased_mortality_pr = 2,
                                SIMPLIFY = FALSE, mc.cores = numCores)) %>% 
  group_by(country, strategy) %>% 
  slice(which.min(total_deaths))
tictoc::toc()

###############################################################################
######### ----- Bind all outcomes together ------

out <- bind_rows(unmitigated_outcomes, distancing_outcomes) %>%
  left_join(df, by = "country") %>%
  mutate(strategy = factor(strategy,
                           levels = c("Unmitigated", "Individual distancing", "Social distancing", "Social distancing+", "Suppression"), ordered = T)) %>%
  arrange(country, strategy) %>%
  group_by(country) %>%
  mutate(value_deaths = total_deaths * vsl,
         value_deaths_extrapolated = if_else(is.na(vsl_extrapolated), total_deaths * vsl,
                                             total_deaths * vsl_extrapolated),
         rel_value_deaths = value_deaths/gdp,
         marginal_deaths = lag(total_deaths) - total_deaths,
         marginal_value_deaths = marginal_deaths * vsl,
         marginal_rel_value_deaths = marginal_value_deaths/gdp)

saveRDS(out, file = here::here("data/predicted-mortality.RDS"))

###############################################################################
# Let's get it by age now ----

distancing_outcomes_age <- bind_rows(mcmapply(FUN = grid_search,
        country = out$country, strategy = out$strategy, mitigation_day = out$mitigation_day, increased_mortality_pr = out$increased_mortality_pr,
        reduce_age = FALSE,
        SIMPLIFY = FALSE, mc.cores = numCores))

out <- distancing_outcomes_age %>%
  mutate(country_code = countrycode::countrycode(country, origin = "country.name", destination = "wb")) %>%
  left_join(select(life_tables, -country),
            by = c("country_code", "age_group")) %>%
  left_join(select(vsl, country_code, vsl_gni_cap, vsl, vsl_160, vsl_100, vsl_extrapolated), by = c("country_code")) %>%
  filter(!is.na(vsl)) %>%
  left_join(select(wb, iso3c, gdp, vulnerable_employment, income_group), by = c("country_code" = "iso3c")) %>%
  mutate(years_lost = total_deaths * life_expectancy,
         vsly = vsl/life_exp_working,
         value_years = years_lost * vsly,
         value_lives = total_deaths * vsl) %>%
  group_by(country, strategy) %>%
  summarize(total_years = sum(years_lost),
            total_deaths = sum(total_deaths),
            increased_mortality_pr = mean(increased_mortality_pr),
            vsl = mean(vsl),
            vsl_extrapolated = mean(vsl_extrapolated),
            value_years = sum(value_years),
            value_lives = sum(value_lives),
            gdp = mean(gdp),
            income_block = unique(income_group)) %>%
   mutate(rel_value = value_years/gdp,
          rel_value_vsl = value_lives/gdp,
         marginal_value = lag(rel_value) - rel_value,
         marginal_deaths = lag(total_deaths) - total_deaths) %>%
  left_join(get_population("country"), by = "country")


saveRDS(out, file = here::here("data/predicted-mortality-age.RDS"))

distancing_outcomes_age %>%
  mutate(country_code = countrycode::countrycode(country, origin = "country.name", destination = "wb")) %>%
  left_join(select(life_tables, -country),
            by = c("country_code", "age_group")) %>%
  left_join(select(vsl, country_code, vsl_gni_cap, vsl, vsl_160, vsl_100, vsl_extrapolated), by = c("country_code")) %>%
  filter(!is.na(vsl)) %>%
  left_join(select(wb, iso3c, gdp, vulnerable_employment, income_group), by = c("country_code" = "iso3c")) %>%
  mutate(vsly = vsl/life_exp_working,
         value_years = total_deaths * life_expectancy * vsly,
         value_lives = total_deaths * vsl)  %>%
  left_join(get_population("country"), by = "country") %>%
  saveRDS(., file = here::here("data/predicted-mortality-age-disaggregated.RDS"))






###############################################################################
# Suppression effects -----


df_suppression <- readRDS(here::here("data/suppression-triggers.RDS")) %>%
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
                             TRUE ~ country ),
         country = as.character(country))

# Conditional on triggers provided in previous study --
# Which day to implement a 90% lockdown

suppression_timing <- bind_rows(
                              mcmapply(FUN = get_trigger,
                              country = df_suppression$country, strategy = df_suppression$strategy, deaths_threshold = df_suppression$deaths_threshold,
                              SIMPLIFY = FALSE, mc.cores = numCores))

# Use those timings to estimate outcomes

lockdown_outcomes <- bind_rows(
                        mcmapply(grid_search,
                        country = suppression_timing$country, strategy = "Suppression",
                        mitigation_day = suppression_timing$time, mitigation_duration = 90,
                        SIMPLIFY = FALSE, mc.cores = numCores)) %>%
  group_by(country) %>% 
  slice(which.min(deaths))













cc <- c("South Africa", "Nigeria",
        "Indonesia", "Pakistan", "Nepal")
#c("United States", "Japan", "Nigeria", "Pakistan", "Bangladesh")

optimal_timing %>% filter(country %in% cc) %>%
  ggplot(., aes(x = strategy, y = rel_value, color = country, group = country, label = country)) +
  geom_point(size = 2, shape = 19) +
  geom_line(alpha = .8) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(#title = "How does COVID-19 mortality vary?",
    y = "Total VSL Lost (billion USD)") +
  theme_minimal() +
  theme(legend.position = "bottom", axis.title.x = element_blank())
  
  
  
c1 <- c("United States", "Japan", "Brazil",
        "India", "Bangladesh")

c2 <- c("South Africa", "Nigeria",
        "Indonesia", "Pakistan", "Nepal")



df %>% filter(Country %in% c1) %>%
  ggplot(., aes(x = strategy, y = (new_deaths * vsl * 1e6)/gdp, color = Country, group = Country, label = Country)) +
  geom_point(size = 2, shape = 19) +
  geom_line(alpha = .75) +
  ggrepel::geom_label_repel(data = filter(df, Country %in% c1 & strategy == "Unmitigated"),
                            force = 2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(#title = "How does COVID-19 mortality vary?",
    y = "VSL Lost/GDP") +
  theme_minimal() +
  theme(legend.position = "none", axis.title.x = element_blank())
  
df %>% filter(Country %in% c2) %>%
  ggplot(., aes(x = strategy, y = (new_deaths * vsl * 1e6)/gdp, color = Country, group = Country, label = Country)) +
  geom_point(size = 2, shape = 19) +
  geom_line(alpha = .75) +
  ggrepel::geom_label_repel(data = filter(df, Country %in% c2 & strategy == "Unmitigated"),
                            force = 3) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(labels = scales::percent_format(), position = "left") +
  labs(#title = "How does COVID-19 mortality vary?",
    y = "VSL Lost/GDP") +
  theme_minimal() +
  theme(legend.position = "none", axis.title.x = element_blank())






ggplot(df, aes(x = strategy, y = (total_deaths*vsl*1e6)/gdp, color = Country, group = Country)) +
geom_point() +
geom_line() +
scale_color_hue() +
theme_minimal() +
theme(legend.position = "bottom")



foo <- squire::format_output(out[[1]], c("deaths", "infections", "hospital_occupancy",          "ICU_occupancy", "hospital_demand", "ICU_demand"))
bar <- squire::format_output(out[[2]], c("deaths", "infections", "hospital_occupancy",          "ICU_occupancy", "hospital_demand", "ICU_demand"))
baz <- squire::format_output(out[[3]], c("deaths", "infections", "hospital_occupancy",          "ICU_occupancy", "hospital_demand", "ICU_demand"))


bar %>% group_by(replicate, compartment) %>% summarize_all(sum) %>% group_by(compartment) %>% summarize_all(mean)

foo %>% group_by(replicate, compartment) %>% summarize_all(sum) %>% group_by(compartment) %>% summarize_all(mean)

baz %>% group_by(replicate, compartment) %>% summarize_all(sum) %>% group_by(compartment) %>% summarize_all(mean)