library(tidyverse)
library(showtext)
library(rnaturalearth)
library(ggtext)

font_add_google("Cabin")
font_add_google("Cabin Condensed")
showtext_auto()

# Tidy Tuesday 2023, Week 16 Data - https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-04-18/readme.md
tuesData <- tidytuesdayR::tt_load(2023, week = 16)

crops <- tuesData$founder_crops %>%
  filter(!is.na(edibility),
         source == "ORIGINS") %>%
  group_by(site_name) %>%
  slice_max(prop, n = 1) %>%
  ungroup()

map <- ne_countries(continent = c("africa", "asia"), returnclass = "sf")

plot_03 <- ggplot() +
  geom_sf(data = map, colour = "black", fill = "grey95") +
  geom_point(data = crops,
             mapping = aes(x  = longitude, y = latitude, shape = category,
                           colour = category)) +
  labs(x = "",
       y = "",
       title = "Neolithic Founder Crops",
       subtitle = "Eight **founder crops** — emmer wheat, einkorn wheat, barley, lentil, pea, chickpea, bitter vetch, and flax — have long been thought to have been the bedrock of Neolithic economies. <br>
       The map below shows the site locations noted within the Origins of Agriculture database.",
       caption = "Source: Orgins of Agriculture | #30DayChartChallenge | Day 3 | Fauna Flora") +
  theme_void() +
  theme(text = element_text(size = 24, family = "Cabin Condensed"),
        plot.title = element_text(family = "Cabin", face = "bold",
                                  margin = margin(b = 10)),
        plot.subtitle = element_textbox_simple(lineheight = 0.5, hjust = 0, margin = margin(b = 10)),
        legend.position = "top",
        legend.title = element_blank())
  
        