t1 <- calibrate(country = "Bangladesh",
                deaths = 84,
                reporting_fraction = 1,
                dt = 0.2,
                time_period = 200,
                replicates = 25,
                R0_scan = c(2,3,4,5))


# create our projections
p2 <- projections(r = t1, contact_matrix_set_change = c(1,0.5, 0.9), tt_contact_matrix = c(0,5,45))

calc_deaths(t1) - calc_deaths(p2)


plot(t1, var_select = "deaths")


ggproj <- projection_plotting(r_list = list(t1,p2),
                              scenarios = c("Unmitigated","Mitigation"),
                              var_select = c("deaths"),
                              add_parms_to_scenarios = F,ci = FALSE,summarise = TRUE)

# and lets add in the ICU capacity
ggproj 



+ ggplot2::geom_hline(yintercept = t1$parameters$ICU_bed_capacity)