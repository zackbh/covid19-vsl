# Get World Bank data ----
# Uses wbstats package to pull in indicators

library(wbstats)
library(dplyr)

indicators <- c("NY.GDP.MKTP.CD",
                "NY.GNP.MKTP.CD",
                "NY.GNP.PCAP.CD",
              #  "SP.POP.TOTL",       # Total population
                "SP.POP.65UP.TO.ZS", # Population ages 65 and above (% of total population)
                "SL.EMP.SELF.ZS",    # Self-employed, total (% of total employment) (modeled ILO estimate)
                "SL.EMP.VULN.ZS",    # Vulnerable Employment, Total (% Of Total Employment) (Modeled ILO Estimate)
                "GC.TAX.TOTL.GD.ZS", # Tax revenue, % GDP 
                "GC.TAX.YPKG.ZS",    # Take on income, % total taxes
                "SP.DYN.LE00.IN",    # # Life expectancy at birth
                "SH.MED.BEDS.ZS",    # Hospital beds per 1,000 people
                "SH.MED.PHYS.ZS"     # Physicians per 1,000 people
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
         self_employment = SL.EMP.SELF.ZS,
         vulnerable_employment = SL.EMP.VULN.ZS,
         tax_gdp = GC.TAX.TOTL.GD.ZS,
         tax_inc = GC.TAX.YPKG.ZS,
         life_expectancy_birth = SP.DYN.LE00.IN,
         hospital_beds = SH.MED.BEDS.ZS,
         doctors = SH.MED.PHYS.ZS) %>%
  # Income classification: https://blogs.worldbank.org/opendata/new-country-classifications-income-level-2019-2020
  mutate(income_group_handcoded = case_when(gni_cap < 1026 ~ "Low income",
                                  between(gni_cap, 1026, 3995) ~ "Lower-middle income",
                                  between(gni_cap, 3996, 12375) ~ "Upper-middle income",
                                  gni_cap > 12375 ~ "Upper income"))


income_group <- readxl::read_excel(here::here("data/raw-data/CLASS.xls"),
                                   range = "C7:F224",
                                   col_names = c("country", "country_code",
                                                 "region", "income_group"),
                                   col_types = rep("text", 4))

df <- left_join(wb_data, income_group, by = c("iso3c" = "country_code")) %>%
  relocate(iso3c, country, region, income_group) %>%
  mutate(income_group = factor(income_group, levels = c("High income", "Upper middle income",
                                                        "Lower middle income", "Low income"),
                               ordered = T))


saveRDS(df, here::here("data/wb-data.RDS"))



