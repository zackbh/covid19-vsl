
calc_deaths <- function(squire_output, reduce_age = TRUE){
  
  output <- squire::format_output(squire_output, var_select = "deaths", reduce_age = reduce_age)
  
  
  if(reduce_age){
    
    deaths <- output %>% dplyr::group_by(replicate) %>%
      dplyr::summarize(total_deaths = sum(y)) %>%
      ungroup() %>% summarize(total_deaths = mean(total_deaths))

  }
  
  if(!reduce_age){
    
  deaths <- output %>% dplyr::group_by(age_group, replicate) %>%
    dplyr::summarize(total_deaths = sum(y)) %>%
    ungroup() %>% group_by(age_group) %>%
    summarize(total_deaths = mean(total_deaths)) %>% ungroup() %>%
    mutate(age_group = factor(age_group,
                              levels = seq.int(1,17),
                              labels = c("0-4", "5-9", "10-14", "15-19",
                                         "20-24", "25-29", "30-34", "35-39",
                                         "40-44", "45-49", "50-54", "55-59",
                                         "60-64", "65-69", "70-74", "75-79",
                                         "80+")))
  }
  
  return(deaths)
  
}

#' Calculate hospital demand and occupancy from Squire SEEIR model
#'
#' @param squire_output Takes an explicit SEEIR Squire model object as input
#'
#' @return Returns a t x 9 tibble with rows for hospital and ICU demand, occupancy, and capacity, and their delta
#' @export
#'
#' @examples 
calc_hospital_demand <- function(squire_output){
  
  results <- format_output(squire_output,
                          var_select = c("hospital_occupancy", "hospital_demand",
                                         "ICU_occupancy", "ICU_demand"))
  output <- results %>%
    dplyr::group_by(compartment, t) %>%
    dplyr::summarize(y = mean(y)) %>%
    tidyr::pivot_wider(id_cols = t, values_from = y, names_from = compartment) %>%
    mutate(hospital_capacity = max(hospital_occupancy),
           ICU_capacity = max(ICU_occupancy)) %>%
    rowwise() %>%
    mutate(hospital_unmet = if_else(hospital_demand - hospital_capacity > 0, hospital_demand - hospital_capacity, 0),
           icu_unmet = if_else(ICU_demand - ICU_capacity > 0, ICU_demand - ICU_capacity, 0)) %>%
    ungroup()
  
  return(output)
  
}

run_squire <- function(country = "Bangladesh",
                       mitigation_day = 80, mitigation_duration = 40, 
                       social_distancing = 1, increased_mortality_pr = 1,
                       time_period = 365){
  
  base_R0 <- 3.0
  base_pr_death_no_treatment <- c(0.01257020, 0.01257020, 0.01257020, 0.01257020,
                                  0.01257020, 0.01257020, 0.01257020, 0.01336115,
                                  0.01510469, 0.01916412, 0.02747752, 0.04176211,
                                  0.06853166, 0.10530232, 0.14930573, 0.20349534,
                                  0.58043120)
  
  output <- squire::run_explicit_SEEIR_model(country = country,
                                             R0 = c(base_R0, base_R0 * social_distancing, base_R0),
                                             tt_R0 = c(0, mitigation_day, mitigation_day + mitigation_duration),
                                             time_period = time_period  ,
                                             prob_non_severe_death_no_treatment = vapply(base_pr_death_no_treatment * increased_mortality_pr, min, numeric(1), .90)
  )
  
  

  return(output)
}

get_social_distance <- function(strategy){
  
  #assertthat::assert_that(assertthat::is.string(strategy))
  
  distance <- dplyr::case_when(strategy == "Unmitigated" ~ 1,
                   strategy == "Individual distancing" ~ .8,
                   strategy == "Social distancing" ~ .5,
                   strategy == "Social distancing+" ~ (1/3),
                   strategy == "Suppression" ~ .2,
                   TRUE ~ NA_real_)
  
  assertthat::assert_that(assertthat::noNA(distance))
  
  return(distance)
  
}

get_population <- function(identifier = "code"){
  
  assertthat::assert_that(assertthat::is.string(identifier))
  
  if (identifier == "code"){
    squire::population %>% 
      group_by(iso3c) %>% 
      summarize(population = sum(n, na.rm = T)) %>%
      ungroup()
  }
  else{
    squire::population %>% 
      group_by(country) %>% 
      summarize(population = sum(n, na.rm = T)) %>%
      ungroup()
  }
  

  
}

get_hospital_demand <- function(country = "Bangladesh", strategy = "Unmitigated", mitigation_day = 10, mitigation_duration = 40, increased_mortality_pr = 2){
  
  social_distancing <- get_social_distance(strategy)
  
  output <- run_squire(country = country, mitigation_day = mitigation_day, mitigation_duration = mitigation_duration,
                       social_distancing = social_distancing, increased_mortality_pr = increased_mortality_pr)
  
  vals <- calc_hospital_demand(output) %>% 
    dplyr::mutate(country = country,
                  strategy = strategy)
  
  return(vals)
  
    
}


grid_search <- function(country = "Bangladesh", strategy = "Unmitigated", mitigation_day = NULL, mitigation_duration = 40, increased_mortality_pr = 2, reduce_age = TRUE){
  
  #' Title
  #'
  #' @param country Country name
  #' @param strategy Unmitigated, social distancing, social distancing+, or suppression
  #' @param mitigation_day When to begin mitigation or suppression measures
  #' @param mitigation_duration How long to pursue mitigation or suppression
  #'
  #' @return tibble if country, strategy, social distancing,etc.
  #' @export
  #'
  #' @examples
  
  
  social_distancing <- get_social_distance(strategy)
  
  output <- run_squire(country = country, mitigation_day = mitigation_day, mitigation_duration = mitigation_duration,
                       social_distancing = social_distancing, increased_mortality_pr = increased_mortality_pr)
  
  deaths <- calc_deaths(output, reduce_age = reduce_age) %>%
    dplyr::mutate(country = country,
           strategy = strategy,
           mitigation_day = mitigation_day,
           mitigation_duration = mitigation_duration,
           increased_mortality_pr = increased_mortality_pr)
  
  return(deaths)
  
}


get_trigger <- function(country = "Bangladesh", strategy = "Late suppression", deaths_threshold = NULL){
  
  assertthat::is.number(deaths_threshold)
  
  output <- squire::run_explicit_SEEIR_model(country = country)
  
  results <- squire::format_output(output, var_select = "deaths") %>%
    dplyr::group_by(t) %>%
    dplyr::summarize(deaths = mean(y)) %>%
    dplyr::mutate(weekly_deaths = zoo::rollsum(deaths, k = 70, fill = NA),
                  trigger = ifelse(weekly_deaths > deaths_threshold, TRUE, FALSE))  
  
  time <- results[min(which(results$trigger == TRUE)),]$t
  
  return(tibble::tibble("country" = country, "strategy" = strategy, "time" = time))
  
}

