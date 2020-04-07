###############################################################################
# Plot mortality and the VSL ----
###############################################################################
# Load libraries ----
library(dplyr)
library(ggplot2)
  library(prismatic)
options(knitr.kable.NA = "--")

###############################################################################
# Load data ----
df <- readRDS(here::here("data/combined-data.RDS")) %>%
  filter(R0 == 3)
###############################################################################

# Mortality ----

c1 <- c("United States", "Japan", "Brazil",
        "India", "Bangladesh")

c2 <- c("United States", "South Africa", "Nigeria",
        "Indonesia", "Pakistan", "Nepal")

# Mortality in levels ----

df %>% filter(Country %in% c1) %>%
  ggplot(., aes(x = strategy, y = total_deaths,
                color = Country, group = Country, label = Country)) +
  geom_point(size = 2, shape = 19) +
  geom_line(alpha = .75) +
  ggrepel::geom_label_repel(data = filter(df, Country %in% c1 & strategy == "Unmitigated"), force = 2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(labels = scales::comma_format(), breaks = scales::pretty_breaks(), position = "left") +
  scale_color_hue() +
  labs(y = "Total population lost") +
  theme_minimal() +
  theme(legend.position = "none", axis.title.x = element_blank())

ggsave(here::here("fig/deaths-country-a.pdf"), width = 5, height = 5, dpi = 1200)



df %>% filter(Country %in% c2) %>%
  ggplot(., aes(x = strategy, y = total_deaths,
                color = Country, group = Country, label = Country)) +
  geom_point(size = 2, shape = 19) +
  geom_line(alpha = .75) +
  ggrepel::geom_label_repel(data = filter(df, Country %in% c2 & strategy == "Unmitigated"), force = 2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(labels = scales::comma_format(), breaks = scales::pretty_breaks(), position = "left") +
  scale_color_hue() +
  labs(y = "Total population lost") +
  theme_minimal() +
  theme(legend.position = "none", axis.title.x = element_blank())

ggsave(here::here("fig/deaths-country-b.pdf"), width = 5, height = 5, dpi = 1200)


###############################################################################
# Mortality as fraction of population -----
###############################################################################
# Countries of interest
cc <- c("United States", "OECD", "China",
        "Sub-Saharan Africa","Bangladesh")

mortality_risk <- df %>% dplyr::filter(Country %in% cc) %>%
ggplot(., aes(x = strategy, y = total_deaths/total_pop, color = Country, group = Country, label = Country)) +
  geom_point(size = 2, shape = 19) +
  geom_line(alpha = .75) +
  ggrepel::geom_label_repel(data = filter(df, Country %in% cc & strategy == "Unmitigated"), force = 2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(labels = scales::percent_format(), position = "left") +
  scale_color_hue() +
  labs(#title = "How does COVID-19 mortality vary?",
    y = "Percent population lost") +
  theme_minimal() +
  theme(legend.position = "none", axis.title.x = element_blank())

ggplot2::ggsave(filename = here::here("fig/mortality_risk.pdf"),
                plot = mortality_risk, height = 5, width = 5, dpi = 1200)

## Levels in a table
df %>% filter(Country %in% cc) %>% 
  mutate(percent_mortality = round((total_deaths/total_pop)*100, digits = 3)) %>%
  select(Country, strategy, percent_mortality) %>%
  tidyr::pivot_wider(names_from = "Country", values_from = "percent_mortality")

## Mortality perent by income block ----

inc_block <- df %>% 
  filter(!is.na(income_block)) %>% 
  group_by(income_block, strategy) %>%
  summarize(total_deaths = sum(total_deaths),
            total_pop = sum(total_pop),
            percent_mortality = (total_deaths/total_pop))


ggplot(inc_block, aes(x = strategy, y = percent_mortality, color = income_block, group = income_block, label = income_block)) +
  geom_point(size = 2, shape = 19) +
  geom_line(alpha = .75) +
  ggrepel::geom_label_repel(data = filter(inc_block, strategy == "Unmitigated")) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_hue() +
  labs(y = "Percent population lost") +
  theme_minimal() +
  theme(legend.position = "none", axis.title.x = element_blank())

ggplot2::ggsave(here::here("fig/deaths-inc-bloc.pdf"), height = 5, width = 5, dpi = 1200)
  



