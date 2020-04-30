# Get World Bank data ----

library(wbstats)
library(dplyr)

indicators <- c("NY.GDP.MKTP.CD",
                "NY.GNP.MKTP.CD",
                "NY.GNP.PCAP.CD",
                "NY.ADJ.NNTY.PC.CD", # Adjusted net national income per capita (current US$)
                "SP.POP.65UP.TO.ZS", # Population ages 65 and above (% of total population)
                "SL.EMP.SELF.ZS",    # Self-employed, total (% of total employment) (modeled ILO estimate)
                "SL.EMP.VULN.ZS",    # Vulnerable Employment, Total (% Of Total Employment) (Modeled ILO Estimate)
                "GC.TAX.TOTL.GD.ZS", # Tax revenue, % GDP 
                "GC.TAX.YPKG.ZS",    # Take on income, % total taxes
                "SP.DYN.LE00.IN",    # # Life expectancy at birth
                "SH.MED.BEDS.ZS", # Hospital beds per 1,000 people
                "SH.MED.PHYS.ZS" # Physiciains per 1,000 people
                )    

wb_data <- purrr::map_dfr(indicators,
                          .f = function(x) wbstats::wb(country = "countries_only", 
                                                       indicator = x, 
                                                       mrv = 1, # Get the most recent value of each indicator 
                                                       return_wide = F)) %>%
  dplyr::select(iso3c, value, indicatorID) %>%
  tidyr::pivot_wider(id_cols = iso3c, values_from = value, names_from = indicatorID) %>%
  rename(gdp = NY.GDP.MKTP.CD,
         gni = NY.GNP.MKTP.CD,
         gni_cap = NY.GNP.PCAP.CD,
         income = NY.ADJ.NNTY.PC.CD,
         self_employment = SL.EMP.SELF.ZS,
         vulnerable_employment = SL.EMP.VULN.ZS,
         tax_gdp = GC.TAX.TOTL.GD.ZS,
         tax_inc = GC.TAX.YPKG.ZS,
         life_expectancy = SP.DYN.LE00.IN,
         hospital_beds = SH.MED.BEDS.ZS,
         doctors = SH.MED.PHYS.ZS) %>%
  # Income classification: https://blogs.worldbank.org/opendata/new-country-classifications-income-level-2019-2020
  mutate(income_block = case_when(gni_cap < 1026 ~ "Low income",
                                  between(gni_cap, 1026, 3995) ~ "Lower-middle income",
                                  between(gni_cap, 3996, 12375) ~ "Upper-middle income",
                                  gni_cap > 12375 ~ "Upper income"))





#calorie_deficit <- readr::read_csv(here::here("data/raw-data/SN_ITK_DFCT.csv")) %>%
 # janitor::clean_names() %>%
  #dplyr::select(country_code, x2016) %>%
  #rename(calorie_deficit = x2016)

readr::write_csv(wb_data, path = here::here("data/wb-data.csv"))

# Save the data ----
#readr::write_csv(dplyr::left_join(wb_data, readRDS(here::here("data/fies.RDS")), by = c("iso3c" = "country_code")),
 #                path = here::here("data/wb_data.csv"))

