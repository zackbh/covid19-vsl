###############################################################################
# Load libraries ----
library(tabulizer)
library(dplyr)
library(countrycode)
###############################################################################

###############################################################################
# Robinson, Hammitt, and O’Keeffe 
# "Valuing Mortality Risk Reductions in Global Benefit-Cost Analysis"
# https://doi.org/10.1017/bca.2018.26
# VSL estimates for low- or middle-income countries 
## categorize by the World Bank, based on their 2015 income levels. 
## in 2015 international dollars based on purchasing power parity
###############################################################################

vsl_robinson <- readxl::read_excel(here::here("data/raw-data/vsl-robinson-2019.xlsx")) %>%
  mutate(country_code = countrycode::countrycode(country, origin = "country.name", destination = "wb"))


###############################################################################
# Extract tables Viscusi and Masterman (2017) 
## "Income Elasticities and Global Values of a Statistical Life"
## Takes in .pdf article => outputs country-specific VSL 
###############################################################################
###############################################################################

# Read in tables ----
vsl_raw <- tabulizer::extract_tables(file = here::here("data/raw-data/viscusi-2017.pdf"),
                          pages = 20:22) # VSL tables

# Bind together ----
vsl <- 
  rbind(vsl_raw[[1]][-c(1:2),1:3],  # Omit header rows
        vsl_raw[[1]][-c(1:2), 4:6],  # Omit header rows
        vsl_raw[[2]][,1:3],
        vsl_raw[[2]][,4:6],
        vsl_raw[[3]][,1:3],
        vsl_raw[[3]][,4:6]) %>%
  tibble::as_tibble(., .name_repair = "universal") %>%
  dplyr::transmute(country = ...1,
            vsl_gni_cap = as.numeric(...2)*1e3,  # USD
            vsl = as.numeric(...3)*1e6) %>%  # USD
 filter(country != "") %>%
  mutate(country_code = countrycode::countrycode(country, origin = 'country.name', destination = 'wb'))


# Join VM 2017 with RHO 2019 and save ----

df <- left_join(vsl, vsl_robinson,
                by = c("country_code", "country"))


readr::write_csv(df, here::here("data/vsl.csv"))