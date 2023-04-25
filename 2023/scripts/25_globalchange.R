library(tidyverse)
library(showtext)
library(maps)
library(RColorBrewer)
library(ggtext)

font_add_google("Cabin")
font_add_google("Cabin Condensed")
showtext_auto()

# Manually download data source from OWID:
# Water Use and Stress Data: https://ourworldindata.org/water-use-stress

waterUse <- read_csv(here::here("2023/data/25_globalchange.csv")) %>%
  filter(Year %in% c(2000, 2017)) %>%
  mutate(Entity = case_when(Entity == "United States"~"USA",
                            Entity == "United Kingdom"~"UK",
                            .default = Entity)) %>%
  rename(water_stress_perc = `6.4.2 - Level of water stress: freshwater withdrawal as a proportion of available freshwater resources (%) - ER_H2O_STRESS - No breakdown`)

world <- map_data("world") %>%
  left_join(waterUse, by = c("region" = "Entity"), copy = TRUE) %>%
  mutate(stress = case_when(water_stress_perc <= 25~"No Stress",
                            water_stress_perc > 25 & water_stress_perc <= 50~"Low Stress",
                            water_stress_perc > 50 & water_stress_perc <= 75~"Medium Stress",
                            water_stress_perc > 75 & water_stress_perc <= 100~"High Stress",
                            water_stress_perc > 100~"Critical Stress",
                            is.na(water_stress_perc)==TRUE~"No Data")) %>%
  mutate(stress = factor(stress, 
                         levels = c("No Data", "No Stress", "Low Stress", "Medium Stress", "High Stress", "Critical Stress")))

plot_25 <- ggplot() +
  geom_polygon(data = world %>% filter(!is.na(Year)), aes(x = long, y = lat, group = group, fill = stress), colour = "grey50", linewidth = 0.25) +
  facet_wrap(~Year, nrow = 2, strip.position = "left") +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = "Global Water Stress",
       subtitle = "Global water stress 'is measured based on freshwater withdrawls as a share of internal resources'. This is shown for 2000 and 2017.
       Saudi Arabia and Libya had the highest levels of water stress with withdrawl rates exceeding internal (renewable) resources by approximately 800% and 900% respectively.
       Whereas, countries in Norther Europe, Latin America and Oceania have much lower withdrawal rates and generally do not exceed 25% of internal resources.",
       caption = "Source: OWID | #30DayChartChallenge | Day 25 | Global Change") +
  theme_void() +
  theme(text = element_text(family = "Cabin Condensed", size = 42),
        plot.background = element_rect(fill = "grey95", colour = "grey95"),
        panel.background = element_rect(fill = "grey95", colour = "grey95"),
        plot.title = element_text(family = "Cabin", face = "bold", colour = "grey5", hjust = 0.5, size = 60, margin = margin(t = 10)),
        plot.subtitle = element_textbox_simple(colour = "grey5", hjust = 0.5, halign = 0.5, lineheight = 0.25, size = 40, margin = margin(t = 10, l = -30)),
        plot.caption.position = "plot",
        plot.caption = element_text(colour = "grey5", margin = margin(t = 5, b = 5), size = 28),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.margin = margin(r = 10, l = 10),
        strip.placement = "inside",
        strip.text = element_text(face = "bold", size = 40)) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE, reverse = FALSE, title = "",
                             label.position = "bottom",
                             keyheight = 1,
                             keywidth = 8, label.vjust = 5))

ggsave(plot = plot_25, path = here::here("2023/charts/"),
       "25_globalchange.png", dpi = 320, height = 8, width = 11, unit = "in")
