library(tidyverse)
library(showtext)
library(ggridges)
library(MetBrewer)
library(lubridate)

font_add_google("Cabin")
font_add_google("Cabin Condensed")
showtext_auto()

# Manually download data source from USGS:
# Avalanche occurrence records along the Going-to-the-Sun Road, Glacier National Park, Montana Data: https://doi.org/10.5066/P9BO1LHQ

avyData <- read_csv(here::here("2023/data/07_hazards.csv")) %>%
  mutate(Date = mdy(Date),
         Month = month(Date, label = TRUE, abbr = FALSE),
         Year = year(Date)) %>%
  filter(Month %in% c("March", "April", "May", "June")) %>%
  select(c(ID, Date, Year, Month, AvalancheType, SizeDestructiveForce)) %>%
  na.omit() %>%
  group_by(Year, Month) %>%
  summarise(n = n(), sizeAvg = mean(SizeDestructiveForce))

plot_07 <- avyData %>%
  ggplot(aes(x = n, y = Month, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 1) +
  scale_fill_met_c("Hokusai2", direction = -1) +
  labs(x = "",
       y = "",
       title = "Avalanche Occurences along Going-to-the-Sun Road in Glacier National Park (2003-2022)",
       subtitle = "Starting in 2003, USGS and NPS started collecting avalanche observations along Going-to-the-Sun Road during Spring road clearing in order to communicate the potential avalanche hazard while road clearing operations are in progress.
       Overall, avalanches are mostly reported during the month of May whereas the least number of avalanches are reported in March.",
       caption = "Source: USGS | #30DayChartChallenge | Day 7 | Hazards") +
  theme_void() +
  theme(text = element_text(family = "Cabin Condensed", size = 48),
        plot.background = element_rect(color = "grey95", fill = "grey95"),
        panel.background = element_rect(color = "grey95", fill = "grey95"),
        plot.margin = margin(r = 10, l = 10),
        plot.title = element_text(family = "Cabin", size = 64, face = "bold", hjust = 0.5,
                                  margin = margin(t = 10)),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_textbox_simple(lineheight = 0.25, hjust = 0.5, linewidth = 100,
                                                       margin = margin(t = 10, r = 10, 
                                                                       b = 10)),
        legend.position = "top",
        legend.justification = "center",
        plot.caption = element_text(size = 32, margin = margin(r = 10, b = 10)),
        axis.text.y = element_text()) +
  guides(fill = guide_colorbar(title = "Number of Avalanche Occurrences", 
                               title.justification = "center", 
                               title.position = "top", 
                               ticks = TRUE, 
                               frame.colour = "black", 
                               barheight = 1.5, barwidth = 15,
                               title.vjust = -5,
                               title.hjust = 0.5,
                               
                               label.vjust = 5))

ggsave(plot = plot_07, path = here::here("2023/charts/"), "07_hazards.png", dpi = 320, height = 8, width = 11, unit = "in")

