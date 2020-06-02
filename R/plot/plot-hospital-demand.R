library(squire)
library(dplyr)
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
  geom_line(aes(linetype = strategy), size = 1.25) +
  geom_hline(aes(yintercept = 1), linetype = "longdash") +
  facet_wrap(~country) +
  ggplot2::scale_color_viridis_d(option = "E", begin = 0, end = .8) +
  scale_y_continuous(labels = scales::percent_format()) +
  #scale_color_hue() +
  scale_linetype_manual(values=c("solid", "dotted"))+
  labs(x = "Day", y = "Percent demand of existing hospital bed capacity") +
  theme_minimal() +
  theme(legend.position = c(.8,.8), legend.title = element_blank())


icu <- out %>% filter(between(t, 40,300)) %>%
  ggplot(., aes(x = t, color = strategy, y = ICU_demand/ICU_capacity))+
  geom_line(aes(linetype = strategy), size = 1.25) +
  geom_hline(aes(yintercept = 1), linetype = "longdash") +
  facet_wrap(~country) +
  ggplot2::scale_color_viridis_d(option = "E", begin = 0, end = .8) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_linetype_manual(values=c("solid", "dotted"))+
  labs(x = "Day", y = "Percent demand of existing ICU bed capacity") +
  theme_minimal() +
  theme(legend.position = c(.8,.8), legend.direction = "vertical", legend.title = element_blank())


ggsave(here::here("fig/hospital-demand.pdf"), device = "pdf", plot = hosp, width = width, height = height, dpi = dpi)

ggsave(here::here("fig/icu-demand.pdf"), device = "pdf", plot = icu, width = width, height = height, dpi = dpi)

# Get values ----

out %>% mutate(rel_hospital_demand = hospital_demand/hospital_capacity,
               rel_ICU_demand = ICU_demand/ICU_capacity)  %>%
  select(country, strategy, rel_hospital_demand, rel_ICU_demand) %>%
  tidyr::pivot_longer(cols = c(rel_hospital_demand, rel_ICU_demand)) %>%
  group_by(country, name, strategy) %>%
  slice(which.max(value))



