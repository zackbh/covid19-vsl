# Powerpoint Presentation Plots ----

df %>% filter(!is.na(income_block)) %>%
  group_by(Country) %>%
  summarize(SP.POP.65UP.TO.ZS = mean(SP.POP.65UP.TO.ZS),
            income_block = unique(income_block))  %>%
  ggplot(., aes(x = SP.POP.65UP.TO.ZS/100, fill = income_block)) +
  scale_fill_hue() +
  geom_histogram(bins = 30, aes(color = after_scale(prismatic::clr_darken(fill, .45)))) +
  facet_wrap(~income_block, ncol = 1) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(position = "left") +
  labs(y = "Number of countries", x = "Percent population over the age 65") +
  theme_minimal() +
  theme(legend.position = "none")


ggsave(here::here("fig/age-dist.png"),width=14.11, height=14.11, units="cm",,dpi=600)



df %>% filter(!is.na(vulnerable_employment)) %>%
  group_by(Country) %>%
  summarize(vulnerable_employment = mean(vulnerable_employment),
            income_block = unique(income_block)) %>%
  ggplot(., aes(x  = vulnerable_employment/100, fill = income_block)) +
  geom_histogram(bins = 35, aes(color = after_scale(prismatic::clr_darken(fill, .45)))) +
  facet_wrap(~income_block, ncol = 1) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(position = "left", breaks = scales::pretty_breaks(n = 3)) +
  scale_fill_hue() +
  labs(title = "",
       y = "Number of countries", x = "Percent workforce self- or informally-employed") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(here::here("fig/vulnerable-employment.png"),width=10, height=7.5, units="cm",dpi=600)
