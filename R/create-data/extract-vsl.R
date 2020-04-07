###############################################################################
# Extract tables Viscusi and Masterman (2017) 
## "Income Elasticities and Global Values of a Statistical Life"
## Takes in .pdf article => outputs country-specific VSL 
###############################################################################
###############################################################################
# Load libraries ----
library(tabulizer)
library(dplyr)
library(countrycode)
###############################################################################

# Read in tables ----
vsl_raw <- tabulizer::extract_tables(file = here::here("reading/viscusi-2017.pdf"),
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
            vsl_gni_cap = as.numeric(...2),  # USD, thousands
            vsl = as.numeric(...3)) %>%  # USD, millions
 filter(country != "") %>%
  mutate(country_code = countrycode::countrycode(country, origin = 'country.name', destination = 'wb'))
  
readr::write_csv(vsl, here::here("data/vsl.csv"))