# Plot VSL ----
# Load libraries ----
library(dplyr)
library(ggplot2)
  library(patchwork)
  library(prismatic)
options(knitr.kable.NA = "--")

###############################################################################
# Load data ----
df_age <- readRDS(here::here("data/predicted-mortality-age.RDS"))
vsl <- readr::read_csv(here::here("data/vsl.csv"))

cc <- c("United States", "United Kingdom", "Nigeria", "Pakistan", "Bangladesh", "Mexico")

c1 <- c("United States", "United Kingdom", "Indonesia", "Mexico",
        "India", "Bangladesh")

c2 <- c("United States", "South Africa", "Nigeria",
        "Botswana", "Pakistan", "Nepal")


###############################################################################
# VSLY estimates ----- ########################################################


vsly_a <- df_age %>% filter(country %in% c1) %>%
  ggplot(., aes(x = strategy, y = value_years/gdp, color = country, group = country, label = country)) +
  geom_point(size = 2, shape = 19) +
  geom_line(alpha = .75) +
  ggrepel::geom_label_repel(data = filter(df_age, country %in% c1 & strategy == "Unmitigated"),
                            force = 2) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(breaks = c(.2,.4,.6,.8,1), limits = c(.2,1), labels = scales::percent_format()) +
  labs(y = "VSLY Lost/GDP") +
  theme_minimal() +
  theme(legend.position = "none", axis.title.x = element_blank())

vsly_b <- df_age %>% filter(country %in% c2) %>%
  ggplot(., aes(x = strategy, y = value_years/gdp, color = country, group = country, label = country)) +
  geom_point(size = 2, shape = 19) +
  geom_line(alpha = .75) +
  ggrepel::geom_label_repel(data = filter(df_age, country %in% c2 & strategy == "Unmitigated"),
                            force = 3) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(breaks = c(.2,.4,.6,.8,1),limits = c(.2,1), labels = scales::percent_format(), position = "right") +
  labs(y = "VSLY Lost/GDP") +
  theme_minimal() +
  theme(legend.position = "none", axis.title.x = element_blank())

ggsave(plot = vsly_a, filename = here::here("fig/vsly-gdp-a.pdf"), device = "pdf", width = 5, height = 6)
ggsave(plot = vsly_b, filename = here::here("fig/vsly-gdp-b.pdf"), device = "pdf", width = 5, height = 6)


vsly_a + vsly_b +
  plot_annotation(title = "Figure 10: What is the value of social distancing for each country?",
                  caption = stringr::str_wrap("Point estimates of the total VSLY of predicted mortality in each country over that country's GDP in each scenario. A value of 100% represents a loss equal to 100% of that country's yearly GDP", 120),
                  theme = theme(plot.caption = element_text(size = 7), axis.text.x = element_text(size = 7)))
ggsave(here::here("fig/fig10-vsly-gdp.eps"), width = 8, height = 8, device = cairo_ps)



# Difference from VSL ----

df_age %>% filter(country %in% c2) %>%
ggplot(., aes(x = strategy, y = (value_years - value_lives)/gdp, color = country, group = country, label = country)) +
  geom_point(size = 2, shape = 19) +
  geom_line() +
  ggrepel::geom_label_repel(data = filter(df_age, country %in% c2 & strategy == "Unmitigated"),
                            force = 3) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(labels = scales::percent_format(), position = "left") +
  labs(y = "Change in relative value moving from VSL to VSLY over GDP") +
  theme_minimal() +
  theme(legend.position = "none", axis.title.x = element_blank())





# By income bloc ----


income_bloc <- df_age %>%
  group_by(strategy, income_block) %>%
  summarize(total_losses = sum(value_years, na.rm = T)/sum(gdp, na.rm = T),
            avg_losses = weighted.mean(value_years, w = population, na.rm = T)/weighted.mean(gdp, w = population, na.rm = T))


ggplot(income_bloc, aes(x = strategy, y = total_losses, color = income_block, group = income_block, label = income_block)) +
  geom_point(size = 2, shape = 19) +
  geom_line(alpha = .75) +
  ggrepel::geom_label_repel(data=filter(income_bloc, strategy == "Unmitigated")) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_hue() + 
  labs(title = "Figure 11: VSLY by Income Group",
       caption = stringr::str_wrap("Total VSLY over GDP in each income group. A value of 100% indicates a welfare loss equal to 100% of that income group's yearly GDP", 100),
      y = "Total VSLY Lost/Total GDP") +
  theme_minimal() +
  theme(legend.position = "none", axis.title.x = element_blank(), plot.caption = element_text(size=7))

ggsave(here::here("fig/fig11-vsly-income-group.eps"), device = cairo_ps, height = 6, width = 6)
ggsave(here::here("fig/vsly-income-group.pdf"), device = "pdf", height = 4.5, width = 4.5)

## Relative to VSL

ggplot(filter(df_age, country %in% c1),
       aes(x = strategy, color = country, group = country)) +
  geom_point(aes(y = value_years/gdp)) +
  geom_point(aes(y = (total_deaths * vsl)/gdp)) +
  geom_line(aes(y = value_years/gdp)) +
  geom_line(aes(y = (total_deaths * vsl)/gdp)) +
  facet_wrap(~country)
