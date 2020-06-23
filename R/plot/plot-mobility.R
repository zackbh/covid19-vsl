################################################################################
# Plot change in movement ----

# Load libraries
library(ggplot2)
library(dplyr)
library(covdata)

wb <- readRDS(here::here("data/wb-data.RDS")) %>%
	select(iso3c, country, region, income_group)
pop <- readr::read_csv(here::here("data/population.csv"))
stringency <- readRDS(here::here("data/stringency-index.RDS"))

cc <- c("United States", "United Kingdom", "Nigeria", "Pakistan", "Bangladesh", "Mexico")

c1 <- c("United States", "United Kingdom", "Indonesia", "Mexico",
        "India", "Bangladesh")

c2 <- c("United States","South Africa", "Nigeria",
        "Botswana", "Pakistan", "Nepal")

# Mobility data by Apple and Google

google_mobility %>% 
filter(type != "parks") %>%
group_by(country_region, type, date) %>%
summarize(avg_pct_diff = mean(pct_diff, na.rm = T)) %>%
mutate(type = stringr::str_to_title(type)) %>%
filter(country_region %in% cc) %>%
ggplot(., aes(x = date, y = avg_pct_diff/100, color= type, group = type)) +
geom_point(alpha = .55) +
geom_smooth(se = F) +
geom_hline(yintercept = 0) +
facet_wrap(~country_region) +
scale_y_continuous(labels = scales::percent_format()) +
labs(y = "Avg. % Change Mobility") +
#guides(color = FALSE) +
theme_minimal() +
theme(legend.position = "bottom", legend.title = element_blank(),
	axis.title.x = element_blank())

ggsave(here::here("fig/google-mobility.png"))

## What about Eid?

google_mobility %>%
select(country_region, date, type, pct_diff) %>%
  filter(country_region %in% c("Pakistan", "Bangladesh", "Indonesia", "Iran", "Turkey")) %>%
  #filter(type != "parks") %>%
  filter(date > as.Date("2020-05-10")) %>% # Closer to Eid
  mutate(type = stringr::str_to_title(type),
		 date = date - 1)  %>%  # Fix dates being recorded for day after
  group_by(country_region, type, date) %>%
  summarize(pct_diff = mean(pct_diff, na.rm = T)) %>%
  ggplot(., aes(x = date, y = pct_diff/100, color = type)) +
  geom_point(alpha = .4) +
  geom_smooth(se = F, span = .5) +
  #geom_rect(aes(xmin=as.Date("2020-05-23"), xmax=as.Date("2020-05-24"), ymin=-Inf, ymax=Inf), alpha = .01, fill = "green", color = NA) +
  annotate("rect", xmin=as.Date("2020-05-23"), xmax=as.Date("2020-05-24"), ymin=-Inf, ymax=Inf, alpha=0.2, fill="red") +
  facet_wrap(~country_region, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "% Change Mobility") +
  theme_minimal() +
  theme(axis.title.x = element_blank(), legend.position = "bottom",
        legend.title = element_blank())


## What about sub-national mobility? 

### Pakistan

google_mobility %>% filter(country_region == "Pakistan") %>%
filter(!is.na(sub_region_1)) %>% # Need to wrap
filter(type != "parks") %>%
mutate(type = stringr::str_to_title(type))  %>%
ggplot(., aes(x = date, y = pct_diff/100, color = type)) +
geom_point(alpha = .55, shape = 20) +
geom_smooth(se = F, span = .7) +
geom_hline(yintercept = 0, linetype = 3, alpha = .5) +
#geom_rect(aes(xmin=as.Date("2020-05-23"), xmax=as.Date("2020-05-24"), ymin=-Inf, ymax=Inf), color = "grey", alpha = .25) +
facet_wrap(~sub_region_1) +
scale_y_continuous(labels = scales::percent_format()) +
labs(y = "% Change Mobility") +
theme_minimal() +
theme(legend.position = "bottom", legend.title = element_blank(),
	axis.title.x = element_blank())

ggsave(here::here("fig/pakistan-mobility.png"))


### India


google_mobility %>% filter(country_region == "India") %>%
#filter(!is.na(sub_region_1)) %>% # Need to wrap
filter(sub_region_1 %in% c("West Bengal", "Rajasthan")) %>%
filter(type != "parks") %>%
filter(date > as.Date("2020-05-10")) %>%
mutate(type = stringr::str_to_title(type))  %>%
ggplot(., aes(x = date, y = pct_diff/100, color = type)) +
geom_point(alpha = .55, shape = 20) +
#geom_line() +
geom_smooth(se = F, span = .4) +
geom_hline(yintercept = 0, linetype = 3, alpha = .5) +
annotate("rect", xmin=as.Date("2020-05-23"), xmax=as.Date("2020-05-24"), ymin=-Inf, ymax=Inf, alpha=0.2, fill="red") +
facet_wrap(~sub_region_1, scales = "fixed") +
scale_y_continuous(labels = scales::percent_format()) +
labs(y = "% Change Mobility") +
theme_minimal() +
theme(legend.position = "bottom", legend.title = element_blank(),
	axis.title.x = element_blank())


# Google Mobility by Income Group ----

foo <- google_mobility %>% 
filter(type != "parks") %>%  # Irrelevant
group_by(country_region, type, date) %>%
summarize(avg_pct_diff = mean(pct_diff, na.rm = T)) %>%
filter(country_region != "Réunion") %>%  # Never matches
filter(country_region != "Taiwan") %>% # Not in WB
mutate(type = stringr::str_to_title(type),
	   iso3c = countrycode::countrycode(country_region, "country.name", "wb")) %>%
left_join(., wb, by = "iso3c") %>%
left_join(., pop, by = "iso3c") %>% ungroup()

foo %>%
group_by(income_group, type, date) %>%
summarize(obs = n(),
			a = mean(avg_pct_diff, na.rm = T),
			b = Hmisc::wtd.mean(avg_pct_diff, pop, na.rm = T)) %>%
ggplot(., aes(x = date, y = b, color = type)) +
geom_point(alpha = .5) +
geom_smooth(se = F) +
facet_wrap(~income_group) +
labs(y = "Population Avg % Change from Median") +
theme_minimal() +
theme(legend.position = "bottom", legend.title = element_blank(),
      axis.title.x = element_blank())




summarize(avg_pct_diff = weighted.mean(avg_pct_diff, w = pop, na.rm = T),
		  other = mean(avg_pct_diff, na.rm = T),
		  boo = min(avg_pct_diff, na.rm = T)) %>%
glimpse()




apple_mobility %>%
filter(region == "India") %>%
ggplot(., aes(x = date, y = index/100)) +
facet_wrap(~transportation_type) +
geom_point() +
geom_smooth(span = .5, se = F, color = "black") +
geom_hline(yintercept = 1) +
scale_y_continuous(labels = scales::percent_format())


google_mobility %>% 
filter(sub_region_1 == "West Bengal") %>%
select(date, type, pct_diff) %>%
mutate(type = stringr::str_to_upper(type)) %>%
ggplot(., aes(x = date, y = pct_diff/100, color= type)) +
geom_point(alpha = .55) +
geom_smooth(method = "gam", se = F, color = "black") +
geom_hline(yintercept = 0) +
facet_wrap(~type) +
scale_y_continuous(labels = scales::percent_format()) +
theme_minimal() +
labs(y = "Percent change from baseline") +
guides(color = FALSE) +
theme(axis.title.x = element_blank())

ggsave(here::here("fig/w-bengal-mobility.png"))



