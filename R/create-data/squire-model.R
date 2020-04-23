library(squire)
library(purrr)
library(furrr)
plan(multiprocess)
library(dplyr)


calc_deaths <- function(squire_output){
  
  output <- squire::format_output(squire_output, var_select = "deaths")
  
  deaths <- output %>% dplyr::group_by(replicate) %>%
    dplyr::summarize(total_deaths = sum(y))
  
  return(mean(deaths$total_deaths))
  
}

run_squire <- function(mitigation_day = 15, mitigation_duration = 45, mitigation_level = .5,
                       country = "Bangladesh"){
  
  contact_matrix <- squire::get_mixing_matrix(country)
  
  output <- squire::run_explicit_SEEIR_model(country = country,
                                             tt_contact_matrix = c(0, mitigation_day, mitigation_day+mitigation_duration),
                                             contact_matrix_set = list(contact_matrix,
                                                                       contact_matrix * mitigation_level,
                                                                       contact_matrix))
  return(output)
}

# Run updated COVID-19 model ----





countries <- c("United States", "Japan", "Brazil", "India", "Bangladesh",
				"South Africa", "Nigeria", "Indonesia", "Pakistan", "Nepal")


unmitigated <- furrr::future_map_dbl(.x = countries, .f = function(x) calc_deaths(squire::run_explicit_SEEIR_model(country = x)), .progress = T)

outcomes <- tibble::tibble(countries, unmitigated)

saveRDS(outcomes, file = here::here("data/squire/unmitigated.RDS"))

social_distancing <- 1- c(0.4481994, 0.4220294, 0.4482054, 0.4468343,
						  0.4260916, 0.4486710, 0.4481856, 0.4491362,
						  0.4467822, 0.4384045)

storage <- vector(mode = "list", length = length(countries))

for (i in seq_along(countries)){

test <- cross_df(tibble::tibble(day = seq.int(15, 45, length.out = 4),
                                reduction = seq(.3,.8, length.out = 4)))
test$duration = 45                                              


foo <- furrr::future_pmap(.l = list(country = countries[i],
                                    mitigation_day = test$day, mitigation_duration = test$duration,
                                    mitigation_level = test$reduction),
                          .f = run_squire)

test$deaths <- purrr::map_dbl(.x = foo, .f = calc_deaths)
test$country = countries[i]

storage[[i]] <- test
}



df <- bind_rows(storage)



