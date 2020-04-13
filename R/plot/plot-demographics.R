###############################################################################
# Plot World Bank employment and age distribution data
###############################################################################
# Load libraries and data ---
library(dplyr)
library(ggplot2)
library(prismatic)
df <- readRDS(here::here("data/combined-data.RDS")) %>%
  filter(R0 == 3) %>%
  filter(!is.na(income_block))
###############################################################################

###############################################################################
# Fraction self- or informally employed ----
###############################################################################

df %>% filter(!is.na(vulnerable_employment)) %>%
  group_by(Country) %>%
  summarize(vulnerable_employment = mean(vulnerable_employment),
            income_block = unique(income_block)) %>%
  ggplot(., aes(x  = vulnerable_employment/100, fill = income_block)) +
  geom_histogram(bins = 35, aes(color = after_scale(prismatic::clr_darken(fill, .45)))) +
  facet_wrap(~income_block, ncol = 1) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(position = "right") +
  scale_fill_hue() +
  labs(title = "",
       y = "Number of countries", x = "Percent workforce self- or informally-employed") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(here::here("fig/vulnerable-employment.pdf"), width = 5, height = 5, dpi = 1200)

###############################################################################
# Age distribution by income block ----
###############################################################################

df %>% filter(!is.na(income_block)) %>%
  group_by(Country) %>%
  summarize(SP.POP.65UP.TO.ZS = mean(SP.POP.65UP.TO.ZS),
            income_block = unique(income_block))  %>%
  ggplot(., aes(x = SP.POP.65UP.TO.ZS/100, fill = income_block)) +
  scale_fill_hue() +
  geom_histogram(bins = 30, aes(color = after_scale(prismatic::clr_darken(fill, .45)))) +
  facet_wrap(~income_block, ncol = 1) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(position = "right") +
  labs(y = "Number of countries", x = "Percent population over the age 65") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(here::here("fig/age-dist.pdf"), width = 5, height = 5, dpi = 1200)

df %>% group_by(income_block) %>%
  summarize(avg_perc_old = weighted.mean(SP.POP.65UP.TO.ZS/100, total_pop))


# Mobility data ----

df %>% filter(!is.na(income_block)) %>%
  group_by(Country) %>%
  summarize(residential = mean(residential),
            workplaces = mean(workplaces),
            income_block = unique(income_block)) %>%
  ggplot(., aes(x = workplaces, fill = income_block)) +
  scale_fill_hue() +
  geom_histogram(bins = 30, aes(color = after_scale(prismatic::clr_darken(fill, .45)))) +
  geom_vline(xintercept = 0, linetype = 2) +
  facet_wrap(~income_block, ncol = 1) +
  scale_x_continuous(labels = scales::percent_format(), breaks = scales::pretty_breaks()) +
  scale_y_continuous(position = "left", breaks = scales::pretty_breaks(n = 3)) +
  labs(y = "Number of countries", x = "Percent change mobility trends for workplaces") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(color = "black"))

ggsave(here::here("fig/workplace-mobility.pdf"), width = 5, height = 5, dpi = 1200)


df %>% filter(!is.na(income_block)) %>%
  group_by(Country) %>%
  summarize(retail_recreation = retail_recreation,
            income_block = unique(income_block)) %>%
  ggplot(., aes(x = retail_recreation, fill = income_block)) +
  scale_fill_hue() +
  geom_histogram(bins = 30, aes(color = after_scale(prismatic::clr_darken(fill, .45)))) +
  geom_vline(xintercept = 0, linetype = 2) +
  facet_wrap(~income_block, ncol = 1) +
  scale_x_continuous(labels = scales::percent_format(), breaks = scales::pretty_breaks()) +
  scale_y_continuous(position = "right", breaks = scales::pretty_breaks(n = 3)) +
  labs(y = "Number of countries", x = "Percent change mobility trends for retail and recreation") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(color = "black"))

ggsave(here::here("fig/retail-mobility.pdf"), width = 5, height = 5, dpi = 1200)



# Food insecurity ----

foo <- df %>% group_by(Country) %>% summarize(avg_food_insecurity = mean(avg_food_insecurity, na.rm = T), income_block = unique(income_block)) %>% filter(!is.na(income_block) & !is.na(avg_food_insecurity)) 



ggplot(data = foo, aes(x = avg_food_insecurity, fill = income_block)) + 
  geom_histogram(data = foo[,-3], fill = "grey", alpha = .5, bins = 45) +
  geom_histogram(bins = 45, aes(color = after_scale(prismatic::clr_darken(fill, .45)))) + 
  scale_fill_hue() +
  facet_wrap(~income_block, ncol = 1, scales = "free_y") +
  scale_x_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  labs(y = "Number of countries", x = "Likelihood of a person being food insecure") +
  theme(legend.position = "none")





###############################################################################
# Tax revenue ----
###############################################################################
## Too sparse to be useful


df %>% dplyr::filter(!is.na(income_block)) %>%
  ggplot(., aes(x = tax_gdp/100, fill = income_block)) +
  scale_fill_hue() +
  geom_histogram(bins = 50, aes(color = after_scale(prismatic::clr_darken(fill, .45)))) +
  facet_wrap(~income_block, ncol = 1) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(position = "left", breaks = scales::pretty_breaks()) +
  labs(title = "",
       y = "Number of countries", x = "Tax revenue (% GDP)") +
  theme_minimal() +
  theme(legend.position = "none")


df %>% dplyr::filter(!is.na(income_block)) %>%
  ggplot(., aes(x = tax_inc/100, fill = income_block)) +
  scale_fill_hue() +
  geom_histogram(bins = 50, aes(color = after_scale(prismatic::clr_darken(fill, .45)))) +
  facet_wrap(~income_block, ncol = 1, scales = "fixed") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(position = "left", breaks = scales::pretty_breaks()) +
  labs(title = "",
       y = "Number of countries", x = "Income tax revenue (% total taxes)") +
  theme_minimal() +
  theme(legend.position = "none")


df %>% dplyr::filter(!is.na(income_block)) %>%
  ggplot(., aes(x = calorie_deficit, fill = income_block)) +
  scale_fill_hue() +
  geom_histogram(bins = 50, aes(color = after_scale(prismatic::clr_darken(fill, .45)))) +
  facet_wrap(~income_block, ncol = 1, scales = "fixed") +
  scale_x_continuous(labels = scales::comma_format(), breaks = scales::pretty_breaks()) +
  scale_y_continuous(position = "left", breaks = scales::pretty_breaks()) +
  labs(title = "",
       y = "Number of countries", x = "Depth of Food Deficit (kilocalories per person per day)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(here::here("fig/caloric-deficit.pdf"), width = 5, height = 5, dpi = 1200)

