# Day 11 - Mobile-Friendly, 11 April 2023

# Libraries, Fonts, Data
## Libraries
library(tidyverse)
library(showtext)
library(ggdist)

## Fonts
font_add_google("Barlow")
font_add_google("Barlow Condensed")
showtext_auto()

## Data
aiData <- read_csv(here::here("2024/data/11_mobilefriendly.csv"))

# Data Exploration
aiData_pivot_categories <- aiData %>%
  pivot_longer(cols = starts_with("Patent applications granted")) %>%
  group_by(name) %>%
  summarise(n = sum(value)) %>%
  arrange(desc(n)) %>%
  slice_head(n = 5)

aiData_pivot <- aiData %>%
  pivot_longer(cols = starts_with("Patent applications granted")) %>%
  filter(name %in% c(aiData_pivot_categories$name)) %>%
  mutate(name = case_when(name == "Patent applications granted - Field: Personal devices and computing" ~ "Personal Devices & Conmputing",
                          name == "Patent applications granted - Field: Telecommunications" ~ "Telecommunications",
                          name == "Patent applications granted - Field: Business" ~ "Business",
                          name == "Patent applications granted - Field: Life sciences" ~ "Life Sciences",
                          name == "Patent applications granted - Field: Transportation" ~ "Transportation",
                          .default = "Other")) %>%
  group_by(name, Year) %>%
  summarise(n = sum(value))

# Data Visualization
plot11 <- aiData_pivot %>%
  ggplot(aes(x = Year, y = n, fill = name)) +
  geom_area(show.legend = FALSE) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018, 2020, 2022),
                     labels = c(2010, 2012, 2014, 2016, 2018, 2020, 2022)) +
  scale_y_continuous(limits = c(0,30000), labels = seq(0, 30000, 5000), breaks = seq(0, 30000, 5000)) +
  scale_fill_brewer(palette = "Dark2", n = 5) +
  labs(x = "", y = "Number of Patent Applications Granted",
       title = "Artificial Intelligence Patent Applications",
       subtitle = "Number of AI patent applications granted, by industry, worldwide",
       caption = "Source: OWID.org | #30DayChartChallenge | Day 11: mobile-friendly | @hdailey") +
  facet_wrap(~name, nrow = 5, ncol = 1) +
  theme_minimal() +
  theme(text = element_text(family = "Barlow Condensed", size = 48),
        plot.title = element_text(family = "Barlow", hjust = 0.5, face = "bold", size = 64),
        plot.title.position = "plot",
        plot.subtitle = element_text(hjust = 0.5, size = 48),
        plot.caption = element_text(hjust = 0.5, margin = margin(b = 2), size = 32),
        plot.caption.position = "plot",
        strip.text = element_text(face = "bold", size = 48),
        plot.margin = margin(10, 15, 0, 15),
        panel.grid.major.y = element_line(linetype = "dashed", colour = "grey75", linewidth = 0.1),
        panel.grid.minor.y = element_line(linetype = "dashed", colour = "grey75", linewidth = 0.1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"))


# Save
ggsave(plot = plot11, path = here::here("2024/charts/"), "11_mobilefriendly.png", 
       height = 6, width = 3, units = "in", dpi = 640)
