# Day 12 - Theme Day: Reuters, 12 April 2023

# Libraries, Fonts, Data
## Libraries
library(tidyverse)
library(showtext)

## Fonts
font_add_google("Merriweather")
font_add_google("Noto Sans")
showtext_auto()

## Data
senate <- readxl::read_xlsx(here::here("2024/data/tmp.xlsx"))

# Data Exploration
 senateGrouped <- senate %>%
   select(c(`Year at start of Congress`, `Number of substantive and significant bills became law`, `Number of substantive bills became law`,
            `Number of commemorative bills became law`)) %>%
   rename("Year" = "Year at start of Congress") %>%
   group_by(Year) %>%
   summarise(totalBillsToLaw = sum(`Number of substantive bills became law`,
                                   `Number of commemorative bills became law`, 
                                   `Number of substantive and significant bills became law`))

# Data Visualization
plot12 <- senateGrouped %>%
  ggplot(aes(x = Year, y = totalBillsToLaw)) +
  annotate("label", x = 1977.8, y = 500, label = "laws passed", colour = "grey20", fill = "#FFFFFF", 
           label.size = NA, family = "Noto Sans", hjust = 1, size = 20) +
  geom_step(colour = "#77B2B2", linewidth = 1) +

  
  geom_rect(aes(xmin = Year, xmax = lead(Year),
                ymin = 0, ymax = totalBillsToLaw), fill = "#77B2B2", alpha = 0.5) +
  geom_point(data = senateGrouped %>% filter(Year == min(Year)), size = 3, colour = "#77B2B2") +
  geom_point(data = senateGrouped %>% filter(Year == max(Year)), size = 3, colour = "#77B2B2") +
  annotate("text", x = 1972.1, y = 428, label = "428", fontface = "bold", colour = "#77B2B2", size = 20, family = "Noto Sans") +
  annotate("text", x = 2007, y = 335, label = "319", fontface = "bold", colour = "#77B2B2", size = 20, family = "Noto Sans") +

  scale_x_continuous(breaks = seq(1973, 2007, 5),
                     labels = seq(1973, 2007, 5),
                     expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(1973, max(senateGrouped$Year)), clip = "off") +
  labs(x = "",
       y = "",
       title = "Congress is passing fewer laws",
       subtitle = "(1973-2008)",
       caption = "Source: Center for Effective Lawmaking | #30DayChartChallenge | Day 12: theme day - reuters | @hdailey \nInspiration: https://www.reuters.com/graphics/USA-CONGRESS/PRODUCTIVITY/egpbabmkwvq/") +
  theme_minimal() +
  theme(text = element_text(family = "Noto Sans", colour = "grey20", size = 64),
        plot.title = element_text(face = "bold", margin = margin(t = 10, b = 15, l = 10), family = "Merriweather", size = 72),
        plot.title.position = "plot",
        plot.subtitle = element_text(margin = margin(t = -10, b = 15, l = 10), size = 64),
        plot.caption = element_text(size = 42, colour = "grey55", lineheight = 0.25, margin = margin(b = 10)),
        plot.margin = margin(r = 10),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line.x = element_line(colour = "#000000", linewidth = 0.5),
        plot.background = element_rect(colour = "#FFFFFF", fill = "#FFFFFF"))


# Save
ggsave(plot = plot12, path = here::here("2024/charts/"), "12_reuters.png", dpi = 640)
