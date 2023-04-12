library(tidyverse)
library(lubridate)
library(showtext)
library(ggtext)
library(ggrepel)
library(grid)

font_add_google("IBM Plex Sans Condensed")
showtext_auto()

# Manually download data source from OWID:
# Drinking water Data: https://ourworldindata.org/water-access#how-many-people-do-not-have-access-to-safe-drinking-water
# Population Data: https://ourworldindata.org/grapher/population-past-future

# inspiration: https://www.economist.com/graphic-detail/2023/03/27/retirement-has-become-much-longer-across-the-rich-world

wqGlobal <- read_csv(here::here("2023/data/12-economist_WQ.csv")) %>%
  filter(Entity %in% c("Afghanistan", "United States", 
                       "Ethiopia", "Brazil", "Bangladesh"))

popGlobal <- read_csv(here::here("2023/data/12-economist_Pop.csv")) %>%
  filter(Entity %in% c("Afghanistan", "United States", 
                       "Ethiopia", "Brazil", "Bangladesh")) %>%
  filter(Year <= 2020,
         Year >= 2000) %>%
  select(-c(`Population (future projections)`))

mergeGlobal <- left_join(popGlobal, wqGlobal, by = join_by(Entity, Year)) %>%
  rename(Population = `Population (historical estimates)`) %>%
  select(c(Entity, Year, Population, wat_sm_number_without)) %>%
  mutate(percPopWQ = wat_sm_number_without/Population)

data_ends <- mergeGlobal %>%
  filter(Year == 2020)

plot_12 <- mergeGlobal %>%
  ggplot(aes(Year, percPopWQ, fill = Entity, colour = Entity)) +
  
  geom_line(aes(group = Entity), linewidth = 1) +
  geom_text_repel(data = data_ends, aes(label = Entity), nudge_y = -0.05, size = 6,
                  min.segment.length = unit(0, "in")) +
  geom_point(size = 3) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%"), position = "right") +
  scale_colour_manual(values = c("Afghanistan" = "#CE7E00", "United States" = "#6A329F", 
                                 "Ethiopia" = "#CC0000", "Brazil" = "#38761D", "Bangladesh" = "#003060")) +
  labs(x = "",
       y = "",
       title = "Percentage of the Population Without Access to Safe Drinking Water ",
       subtitle = "Safe drinking water is defined as 'an improved source located on premises, available when needed, and free from microbiological and priority contamination'. 
       Shown is the percentage of the population in <span style='color:#CE7E00'>**Afghanistan**</span>, <span style='color:#003060'>**Bangladesh**</span>, 
       <span style='color:#38761D'>**Brazil**</span>, <span style='color:#CC0000'>**Ethiopia**</span> and the <span style='color:#6A329F'>**United States**</span> without access to clean safe drinking water from 2000 through 2020.",
       caption = "Source: Our World in Data | #30DayChartChallenge | Day 12 | Theme: The Economist.") +
  theme_minimal(base_family = "IBM Plex Sans Condensed") +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.25, color = "#DAD9D9"),
        plot.background = element_rect(colour = "white", fill = "white"),
        text = element_text(size = 18),
        axis.line.x = element_line(colour = "black", linewidth = 0.25),
        axis.ticks.x = element_line(colour = "black", linewidth = 0.25),
        legend.position = "none",
        plot.title = element_markdown(face = "bold", margin = margin(t = 20, b = 4)),
        plot.title.position = "plot",
        plot.subtitle = element_textbox_simple(padding = margin(5.5, 5.5, 5.5, 5.5),
                                               margin = margin(0, 0, 1, 0), lineheight = 0.75),
        plot.caption = element_text(hjust = 0, size = 12),
        plot.caption.position = "plot")

plot_12
grid.lines(x = c(0, 1), y = 1, gp = gpar(col = "#990000", lwd = 6)) 
grid.rect(x = 0, y = 1, width = 0.2, height = 0.05, gp = gpar(fill = "#990000", col = NA))


            