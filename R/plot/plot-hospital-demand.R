library(squire)
library(dplyr)
library(ggplot2)
library(patchwork)
library(parallel)
  numCores <- detectCores()-1
source(here::here("R/create-data/squire-functions/squire-functions.R"))

width <- 4.5; height <- 4.5; dpi <- 1200


df <- readRDS(here::here("data/predicted-mortality.RDS")) %>%
  dplyr::select(country, strategy, mitigation_day, mitigation_duration, total_deaths) %>%
  dplyr::filter(country %in% c("Bangladesh", "United States")) %>%
  filter(strategy %in% c("Unmitigated", "Suppression"))

out <- bind_rows(parallel::mcmapply(get_hospital_demand, country = df$country, strategy = df$strategy,
                        mitigation_day = df$mitigation_day, mitigation_duration = df$mitigation_duration,
                        mc.cores = numCores, SIMPLIFY = F))

hosp <- out %>% filter(between(t, 40,300)) %>%
ggplot(., aes(x = t, color = strategy, y = hospital_demand/hospital_capacity))+
  geom_line(aes(linetype = strategy), size = 1.35) +
  geom_hline(aes(yintercept = 1), linetype = "dotted") +
  facet_wrap(~country) +
  scale_y_continuous(labels = scales::percent_format()) +
  ggplot2::scale_color_viridis_d(option = "A", begin = 0, end = .8) +
    #scale_color_hue() +
  scale_linetype_manual(values=c("solid", "dotted"))+
  labs(title = "Hospital Bed Demand",
       x = "Day", y = "Percent demand of existing hospital bed capacity") +
  theme_minimal() +
  theme(legend.position = c(.8,.8), legend.title = element_blank(), plot.caption = element_text(size = 7))


icu <- out %>% filter(between(t, 40,300)) %>%
  ggplot(., aes(x = t, color = strategy, y = ICU_demand/ICU_capacity))+
  geom_line(aes(linetype = strategy), size = 1.35) +
  geom_hline(aes(yintercept = 1), linetype = "dotted") +
  facet_wrap(~country) +
  ggplot2::scale_color_viridis_d(option = "A", begin = 0, end = .8) +
  scale_y_continuous(labels = scales::percent_format(), position = "right") +
  scale_linetype_manual(values=c("solid", "dotted"))+
  labs(title = "ICU Bed Demand",
       x = "Day", y = "Percent demand of existing ICU bed capacity") +
  theme_minimal() +
  theme(legend.position = c(.8,.8), legend.direction = "vertical", legend.title = element_blank(),
        plot.caption = element_text(size = 7))



ggsave(here::here("fig/hospital-demand.pdf"), device = "pdf", plot = hosp, width = width, height = height, dpi = dpi)
ggsave(here::here("fig/fig5a-hospital-demand.eps"), device = cairo_ps, plot = hosp, width = width, height = height, dpi = dpi)

ggsave(here::here("fig/icu-demand.pdf"), device = "pdf", plot = icu, width = width, height = height, dpi = dpi)
ggsave(here::here("fig/fig5b-icu-demand.eps"), device = cairo_ps, plot = icu, width = width, height = height, dpi = dpi)



hosp + icu +
  plot_annotation(title = "Figure 5: Medical System Demand in Bangladesh and the U.S.",
                  caption = stringr::str_wrap("Solid line represents daily demand under the unmitigated scenario, while the dotted line represents daily demand under full suppression. The horizontal line shows capacity in each country, with the portion of the curve above that line representing unmet demand and excess mortality. The second wave of the disease is clearly visible in the suppression scenario.", 120),
                  theme = theme(plot.caption = element_text(size = 7), axis.text.x = element_text(size = 7)))
ggsave(here::here("fig/fig5-medical-demand.eps"), width = 7, height = 7, device = cairo_ps)



# Get values ----

out %>% mutate(rel_hospital_demand = hospital_demand/hospital_capacity,
               rel_ICU_demand = ICU_demand/ICU_capacity)  %>%
  select(country, strategy, rel_hospital_demand, rel_ICU_demand) %>%
  tidyr::pivot_longer(cols = c(rel_hospital_demand, rel_ICU_demand)) %>%
  group_by(country, name, strategy) %>%
  slice(which.max(value))



