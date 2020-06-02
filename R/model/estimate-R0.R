# Calibrate Swedish and Brazil data ---
library(squire)
library(dplyr)


#' Prepare data for use with the particle filter.  This function
#' exists to make explicit how time changes through the model
#' relative to the data and to odin's internal clock.
#' @title Prepare particle filter data
#'
#' @param data A data.frame of observed data.  There must be a column
#'   \code{date}, containing dates (or ISO-formatted strings for
#'   conversion with \code{\link{as.Date}}.
#'
#' @param start_date The date to start the simulation from.  Must be
#'   earlier than the first date in \code{data}.
#'
#' @param steps_per_day The number of steps per day
#'
particle_filter_data <- function(data, start_date, steps_per_day) {
  if (!inherits(data, "data.frame")) {
    stop("Expected a data.frame for 'data'")
  }
  if (!("date" %in% names(data))) {
    stop("Expected a column 'date' within 'data'")
  }
  data$date <- as.Date(data$date)
  if (any(diff(data$date) <= 0)) {
    stop("'date' must be strictly increasing")
  }
  start_date <- as.Date(start_date)
  if (start_date >= as.Date(data$date[1], "%Y-%m-%d")) {
    stop("'start_date' must be less than the first date in data")
  }
  
  ## Then for each timestep we work out the start and end date
  ret <- data
  ret$day_start <- as.integer(data$date - start_date - 1L)
  if (nrow(ret) == 1) {
    ret$day_end <- as.integer(ret$day_start[nrow(ret)] + 1L)
  } else {
    ret$day_end <- as.integer(c(ret$day_start[2:nrow(ret)], ret$day_start[nrow(ret)] + 1L))
  }
  
  d0 <- ret[1, ]
  d0[] <- NA
  d0$date <- start_date
  d0$day_start <- 0
  d0$day_end <- ret$day_start[[1]]
  ret <- rbind(d0, ret)
  rownames(ret) <- NULL
  
  ret$step_start <- ret$day_start * steps_per_day
  ret$step_end <- ret$day_end * steps_per_day
  
  class(ret) <- c("particle_filter_data", "data.frame")
  attr(ret, "steps_per_day") <- steps_per_day
  
  ret
}

data <- readRDS(here::here("data/COVID-19-up-to-date.rds"))

swe <- filter(data, Countries.and.territories == "Sweden") %>%
  select(date = DateRep, deaths = Deaths, cases = Cases) %>%
  mutate(date = lubridate::dmy(date)) %>%
  filter(date > lubridate::ymd("2020-01-31")) %>%
  particle_filter_data(., start_date = "2020-02-31", steps_per_day = 2)




future::plan(future::multiprocess())

get <- squire::calibrate(
  data = swe,
  R0_min = 2,
  R0_max = 4,
  R0_step = 0.5,
  first_start_date = "2020-01-25",
  last_start_date = "2020-01-29",
  day_step = 4,
  replicates = 10,
  n_particles = 20,
  country = "Sweden"
)