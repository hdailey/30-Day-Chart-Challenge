library(tidyverse)
library(maps)
library(showtext)
library(ggtext)

font_add_google("Cabin")
font_add_google("Cabin Condensed")
showtext_auto()

# Manually download data source from OWID:
# Migration Data: https://ourworldindata.org/migration

humans <- read_csv(here::here("2023/data/08_humans.csv")) %>%
  select(c(Year, Country, `Net migration rate`)) %>%
  filter(Year == 2020) %>%
  na.omit() %>%
  distinct()

humans$Country <- recode(humans$Country,
                         "United States" = "USA",
                         "United Kingdom" = "UK")

humans <- humans %>%
  mutate(flag = ifelse(`Net migration rate` > 0, "Migrating To", "Migrating From"))

world <- map_data("world") %>%
  left_join(humans, by = c("region" = "Country")) %>%
  mutate(flag = replace_na(flag, "Data Unknown"))

plot_08 <- ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group,
                                 fill = flag), colour = "black", 
               linewidth = 0.25, show.legend = FALSE) +
  scale_fill_manual(values = c("grey95", "#981D26", "#3B75A9")) +
  theme_void() +
  labs(title = "Global Migration in 2020",
       subtitle = "This visual explores the net migration rate for each country in 2020. 
       Net <span style='color:#981D26'>**migration away**</span> and net <span style='color:#3B75A9'>**migration to**</span> are shown in their representative colors. 
       Countries without migration data are shown without color.",
       caption = "Source: OWID | #30DayChartChallenge | Day 8 | Humans") +
  theme(text = element_text(family = "Cabin Condensed", size = 36),
        plot.background = element_rect(fill = "grey95", colour = "grey95"),
        panel.background = element_rect(fill = "grey95", colour = "grey95"),
        plot.title = element_text(family = "Cabin", hjust = 0.5, size = 54, face = "bold",
                                  margin = margin(5, 10, 5, 10)),
        plot.subtitle = element_textbox_simple(halign = 0.5, size = 42, lineheight = 0.5,
                                               margin = margin(5, 10, 5, 10)),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0.5, margin = margin(b = 10)))

ggsave(plot = plot_08, path = here::here("2023/charts/"),
       "08_humans.png", dpi = 320, height = 8, width = 11, unit = "in")
