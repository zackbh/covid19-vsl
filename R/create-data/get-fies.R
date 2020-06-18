# Read FIES data ----
## Stored as a bunch of .Rdata files, unfortunately
###############################################################################
library(purrr)
library(data.table)
library(dtplyr)


get_country_names <- function(string){
  
  country <- stringr::str_sub(string = string, start = 1, end = 3)
  
  return(country)
}

read_fies <- function(Rdata){
  
  load(Rdata)
  
  df <- data
  
  data.table::setDT(df)
  
  return(df)
  
}


###############################################################################


files <- list.files(path = here::here("data/raw-data/fies"), full.names = T)
file_names <- list.files(here::here("data/raw-data/fies"))



fies <- purrr::map(files, .f = read_fies)

country_names <- purrr::map(file_names, get_country_names)


for (i in seq.int(1, length(fies))){
  
  set(fies[[i]], j = "country_code", value = country_names[i])
  
}

df <- data.table::rbindlist(fies)

# Convert year to integer
df[, year := as.integer(as.character(year))]

df[,WORRIED := as.integer(WORRIED)][,
    HEALTHY := as.integer(HEALTHY)][,
    FEWFOOD := as.integer(FEWFOOD)][,
    SKIPPED := as.integer(SKIPPED)][,
    ATELESS := as.integer(ATELESS)][,
    RUNOUT := as.integer(RUNOUT)][,
    HUNGRY := as.integer(HUNGRY)][,
    WHLDAY := as.integer(WHLDAY)][,
    N_adults := as.integer(N_adults)][,
    N_child := as.integer(N_child)]

# Select only most recent observations ----
df <- df[df[, .I[year == max(year)], by=country_code]$V1]


#dplyr::group_by(lazy_dt(df), country_code) %>%
# dplyr::summarize(score = sum(wt * Prob_Mod_Sev, na.rm = T))


#foo <- df[, .(score = sum(wt * Prob_Mod_Sev, na.rm = T)), keyby = country_code]


output <- df[, .(avg_food_insecurity = weighted.mean(Prob_sev, wt, na.rm = ..T)), keyby = .(country_code)]

saveRDS(output, file = here::here("data/fies.RDS"))

