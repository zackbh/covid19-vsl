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
  ggplot(., aes(x  = vulnerable_employment/100, fill = income_block)) +
  geom_histogram(bins = 50, aes(color = after_scale(prismatic::clr_darken(fill, .45)))) +
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
  ggplot(., aes(x = SP.POP.65UP.TO.ZS/100, fill = income_block)) +
  scale_fill_hue() +
  geom_histogram(bins = 50, aes(color = after_scale(prismatic::clr_darken(fill, .45)))) +
  facet_wrap(~income_block, ncol = 1) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(position = "right") +
  labs(y = "Number of countries", x = "Percent population over the age 65") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(here::here("fig/age-dist.pdf"), width = 5, height = 5, dpi = 1200)

df %>% group_by(income_block) %>%
  summarize(avg_perc_old = weighted.mean(SP.POP.65UP.TO.ZS/100, total_pop))

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


