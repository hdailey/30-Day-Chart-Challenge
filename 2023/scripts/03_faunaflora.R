library(tidyverse)
library(showtext)
library(rnaturalearth)

font_add_google("Roboto")
showtext_auto()

# Tidy Tuesday 2023, Week 16 Data - https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-04-18/readme.md
tuesData <- tidytuesdayR::tt_load(2023, week = 16)

crops <- tuesData$founder_crops %>%
  filter(!is.na(edibility),
         source == "ORIGINS") %>%
  group_by(site_name) %>%
  slice_max(prop, n = 1) %>%
  ungroup()

map <- ne_countries(returnclass = "sf")

plot_03 <- ggplot() +
  geom_sf(data = map) +
  geom_rect(data = data.frame(),
            mapping = aes(xmin = 30.0, xmax = 48.6,
                          ymin = 29.8, ymax = 38.5)) +
  geom_point(data = crops,
             mapping = aes(x  = longitude, y = latitude, colour = category))


  