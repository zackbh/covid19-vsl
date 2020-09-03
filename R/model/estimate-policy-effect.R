# Relationship between policies and mobility

library(dplyr)
library(data.table)
library(dtplyr)
library(covdata)


stringency <- data.table::setDT(readRDS(here::here("data/stringency-index.RDS")))
setkey(stringency, country_name, date)
wb <- readRDS(here::here("data/wb-data.RDS"))

pop <- readr::read_csv(here::here("data/population.csv"))

gm <- data.table::setDT(covdata::google_mobility)
gm[, census_fips_code := NULL][, ]
gm[,iso3c := countrycode::countrycode(country_region_code, origin = "iso2c", destination = "iso3c")]
setkey(gm, country_region, date)


df <- gm %>%
  left_join(., stringency, by = c("country_region" = "country_name", "date")) %>%
  left_join(., wb, by = c("iso3c")) %>%
  left_join(., pop, by = c("iso3c"))


cc <- c("United States", "India", "Bangladesh")

ggplot(df[type == "workplaces" & country %in% c("United States", "India")],
       aes(x = date)) +
  geom_point(aes(y = stringency_index)) +
  geom_point(aes(y = pct_diff))

ggplot[df[]]


ggplot(df[!is.na(income_group) & type == "workplaces" & pct_diff < 10],
       aes(y = pct_diff, x = stringency_index, color=income_group, group = income_group)) +
  stat_summary()


  geom_point(position = "jitter", alpha = .3)


mod <- mgcv::gam(pct_diff ~ s(stringency_index, by = income_group, id = 1) + income_group,
                 data = df[type == "workplaces"]  )



plot(mod, shade = TRUE, pages = 1, scale = 0)


country_level <- df[!is.na(stringency_index)][
  !is.na(income_group)][
  type %in% c("residential", "transit", "workplaces")][
  , .(pct_diff = mean(pct_diff, na.rm = T), 
       stringency_index = mean(stringency_index, na.rm = T),
       income_group = unique(income_group)), by = .(country_region, date, type)]

ggplot(country_level[country_region %in% c("United States", "India", "Bangladesh")],
       aes(x = date, color = type)) +
  geom_point(aes(y = pct_diff/100), alpha = .5) +
  geom_step(aes(y = stringency_index/100), color = "black") +
  stat_smooth(aes(y = pct_diff/100), se = F) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_viridis_d() +
  facet_wrap(~country_region) +
  theme(axis.title.x = element_blank(), legend.position = "bottom", legend.title = element_blank())


# What was the effect of policies on mobility?
df %>% 
  #filter(type == "workplaces") %>%
  group_by(country_region, date, type) %>%
  summarize(pct_diff = mean(pct_diff, na.rm = T),
            stringency_index = mean(stringency_index, na.rm = T),
            income_group = unique(income_group)) %>% ungroup() %>%
  group_by(income_group, date, type) %>%
  summarize(pct_diff = Hmisc::wtd.mean(pct_diff, pop, na.rm = T),
            stringency_index = Hmisc::wtd.mean(stringency_index, pop, na.rm = T)) %>%
  ggplot(., aes(x = date, color = income_group)) +
  geom_line(aes(y = pct_diff/100)) +
  geom_line(aes(y = stringency_index/100)) +
  facet_wrap(~type)


ggplot(foo) +
  geom_histogram(aes(x = stringency_index, y = ..density..)) +
  facet_wrap(~income_group)

mod <- lme4::lmer(pct_diff ~ date + stringency_index + (stringency_index | income_group) + (1|region), data = foo[type == "transit"] )