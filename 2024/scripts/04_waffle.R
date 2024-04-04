# Day 04 - waffle, 04 April 2023

# Libraries, Fonts, Data
## Libraries
library(tidyverse)
library(showtext)
library(waffle)

## Fonts
font_add_google("Barlow")
font_add_google("Barlow Condensed")
showtext_auto()

## Data
olympics <- read_csv(here::here("2024/data/04_waffle.csv"))

# Data Exploration
olympicsGrouped <- olympics %>%
  filter(Type != "youthgames") %>%
  mutate(Type = case_when(Type == "summergames" ~ "Summer",
                          Type == "wintergames" ~ "Winter",
                          .default = Type)) %>%
  mutate(Country = case_when(Country == "United States of America" ~ "United States",
                             .default = Country)) %>%
  group_by(Country) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  slice_head(n = 5)

# Data Visualization
plot04 <- olympicsGrouped %>%
  ggplot() +
  geom_waffle(aes(values = n, fill = Country), n_rows = 4, flip = TRUE, 
              show.legend = FALSE, size = 2, colour = "#FFFFFF", radius = unit(0.4, "cm")) +
  geom_text(data = olympicsGrouped %>% filter(Country == "Canada"),
            aes(0.5, 3, label = Country), hjust = 0, family = "Barlow Condensed", fontface = "bold", size = 20) +
  geom_text(data = olympicsGrouped %>% filter(Country == "France"),
            aes(0.5, 3, label = Country), hjust = 0, family = "Barlow Condensed", fontface = "bold", size = 20) +
  geom_text(data = olympicsGrouped %>% filter(Country == "Italy"),
            aes(0.5, 2, label = Country), hjust = 0, family = "Barlow Condensed", fontface = "bold", size = 20) +
  geom_text(data = olympicsGrouped %>% filter(Country == "Japan"),
            aes(0.5, 2, label = Country), hjust = 0, family = "Barlow Condensed", fontface = "bold", size = 20) +
  geom_text(data = olympicsGrouped %>% filter(Country == "United States"),
            aes(0.5, 4, label = Country), hjust = 0, family = "Barlow Condensed", fontface = "bold", size = 20) +
  scale_fill_manual(values = c("#0081C8", "#FCB131", "#00A651", "#EE334E", "#000000")) +
  labs(title = "Top Olympic Hosts (by Country, 1896-2028)",
       caption = "Source: @Pietrfm via Kaggle.com | #30DayChartChallenge | Day 04: waffle | @hdailey") +
  coord_fixed(clip = "off") +
  facet_wrap(vars(Country), nrow = 3, ncol = 2) +
  theme_void() +
  theme(text = element_text(family = "Barlow Condensed", size = 36),
        strip.text = element_blank(),
        plot.title = element_text(family = "Barlow Condensed", face = "bold", hjust = 0.5, size = 64),
        plot.caption = element_text(colour = "grey55", hjust = 0.5),
        plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
        panel.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"))

# Save
ggsave(plot = plot04, path = here::here("2024/charts/"), "04_waffle.png", dpi = 640, width = 3, height = 4, units = "in")
