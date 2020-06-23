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
setkey(gm, country_region, date)

df <- left_join(gm, stringency, by = c("country_region" = "country_name", "date")) %>%
  left_join(., wb, by = c("country_region" = "country")) %>%
  left_join(., pop, by = c("iso3c"))


mod <- mgcv::gam(pct_diff ~ s(stringency_index) + s(log(gdp)), data = df[type == "workplaces" & income_group %in% c("High income", "Upper middle income")]  )

xb <- predict(mod, newdata = df[type == "workplaces" & income_group %in% c("Lower middle income", "Low income")])


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