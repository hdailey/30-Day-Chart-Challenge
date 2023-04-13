library(tidyverse)
library(lubridate)
library(extrafont)
library(ggrepel)
library(bbplot)

# Manually download data source from OWID:
# Drinking water Data: https://ourworldindata.org/water-access#how-many-people-do-not-have-access-to-safe-drinking-water
# Population Data: https://ourworldindata.org/grapher/population-past-future

wqGlobal <- read_csv(here::here("2023/data/12_BBC_WQ.csv")) %>%
  filter(Entity %in% c("Afghanistan", "United States", 
                       "Ethiopia", "Brazil", "Bangladesh"))

popGlobal <- read_csv(here::here("2023/data/12_BBC_Pop.csv")) %>%
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
  geom_text_repel(data = data_ends, aes(label = Entity), nudge_y = -0.05, size = 7,
                  min.segment.length = unit(0, "in")) +
  geom_point(size = 3) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%"), position = "left") +
  scale_colour_manual(values = c("Afghanistan" = "#CE7E00", "United States" = "#6A329F", 
                                 "Ethiopia" = "#CC0000", "Brazil" = "#38761D", "Bangladesh" = "#003060")) +
  labs(x = "",
       y = "",
       title = "Safe Drinking Water Availability and Population",
       subtitle = "Percentage of the population in various countries without safe drinking water.") +
  bbc_style() +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.25, color = "#DAD9D9"),
        plot.background = element_rect(colour = "white", fill = "white"),
        text = element_text(size = 28),
        axis.line.x = element_line(colour = "black"),
        axis.ticks.x = element_line(colour = "black"),
        axis.text = element_text(size = 32),
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 48),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 32, margin = margin(t = -7)),
        plot.caption.position = "plot")

finalise_plot(plot_name = plot_12, 
              source_name = "Source: Our World in Data | #30DayChartChallenge | Day 12 | Theme: BBC",
              save_filepath = here::here("2023/charts/12_BBC.png"))

            