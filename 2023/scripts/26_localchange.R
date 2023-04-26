library(tidyverse)
library(sf)
library(tigris)
library(lubridate)
library(grid)
library(showtext)
library(cowplot)

# Manually Download Data from https://cdec.water.ca.gov/

font_add_google("Cabin")
font_add_google("Cabin Condensed")
showtext_auto()

dataSac <- readxl::read_xlsx(here::here("2023/data/26_localchange.xlsx"), sheet = "SacRiver") %>%
  rename("ID" = "Station ID",
         "IncrementalPPT" = "PPT INC INCHES",
         "AccumulatedPPT" = "RAIN INCHES") %>%
  mutate(DATE = mdy(DATE))

dataSJR <- readxl::read_xlsx(here::here("2023/data/26_localchange.xlsx"), sheet = "SanJoaquinRiver") %>%
  rename("ID" = "Station ID",
         "IncrementalPPT" = "PPT INC INCHES",
         "AccumulatedPPT" = "RAIN INCHES")

dataTLB <- readxl::read_xlsx(here::here("2023/data/26_localchange.xlsx"), sheet = "TulareLake") %>%
  rename("ID" = "Station ID",
         "IncrementalPPT" = "PPT INC INCHES",
         "AccumulatedPPT" = "RAIN INCHES")

dataCV <- rbind(dataSac, dataSJR, dataTLB) %>%
  mutate(watShed = ifelse(ID %in% c("WHI", "SHA", "SMF", "FLD", "CSU"), watShed <- "SAC",
                          ifelse(ID %in% c("SLP", "TCP", "NMS", "FRT", "CHM"), watShed <- "SJR", watShed <- "TLB")))

coordCV <- st_as_sf(dataCV, coords = c("Longitude", "Latitude")) %>%
  st_set_crs(4269)

caMap <- counties(state = 06) %>%
  st_as_sf()

CA <- ggplot() +
  geom_sf(data = caMap) +
  geom_sf(data = coordCV, aes(group = ID, colour = ID), size = 4) +
  scale_colour_manual(values = c("dodgerblue4", "blue", "darkturquoise", "cadetblue", "cyan",
                                 "red", "lightpink", "brown", "coral2", "chocolate1",
                                 "seagreen4", "forestgreen", "darkgreen", "chartreuse"),
                      breaks = c("SHA", "WHI", "SMF", "FLD", "CSU", 
                                 "SLP", "TCP", "NMS", "CHM", "FRT", 
                                 "CRL", "SPA", "QUA", "BCH")) +
  theme_void() +
  theme(text = element_text(family = "Cabin Condensed"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.justification = "center")+
  guides(colour = guide_legend(title = "Station ID", nrow = 2, byrow = FALSE))

sacPlot <- dataCV %>%
  filter(watShed == "SAC") %>%
  ggplot() +
  geom_line(aes(x = DATE, y = AccumulatedPPT, group = ID, colour = ID)) +
  scale_colour_manual(values = c("cyan", "cadetblue", "dodgerblue4", "darkturquoise", "blue")) +
  ylab("Accumulated Precipitation (in)") +
  xlab("") +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Cabin Condensed", size = 34))

sjrPlot <- dataCV %>%
  filter(watShed == "SJR") %>%
  ggplot() +
  geom_line(aes(x = DATE, y = AccumulatedPPT, group = ID, colour = ID)) +
  scale_colour_manual(values = c("coral2", "chocolate1", "brown", "red", "lightpink")) +

  ylab("Accumulated Precipitation (in)") +
  xlab("") +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Cabin Condensed", size = 34))


tlbPlot <- dataCV %>%
  filter(watShed == "TLB") %>%
  ggplot() +
  geom_line(aes(x = DATE, y = AccumulatedPPT, group = ID, colour = ID)) +
  scale_colour_manual(values = c("chartreuse", "seagreen4", "darkgreen", "forestgreen")) +
  ylab("Accumulated Precipitation (in)") +
  xlab("") +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Cabin Condensed", size = 34))

plot_26 <- CA + 
  annotation_custom(ggplotGrob(sacPlot), ymin = 39, ymax = 42, xmin = -124.8, xmax = -130) +
  annotation_custom(ggplotGrob(sjrPlot), ymin = 38, ymax = 41, xmin = -113.8, xmax = -119) +
  annotation_custom(ggplotGrob(tlbPlot), ymin = 33, ymax = 36, xmin = -121.8, xmax = -127) +
  labs(title = "Accumulated Precipitation Across the Central Valley",
       subtitle = "December 4, 2022 - January 2, 2023",
       caption = "Source: CA DWR - California Data Exchange Center | #30DayChartChallenge | Day 26 | Local Change") +
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour = "white"),
        text = element_text(family = "Cabin Condensed", size = 32),
        plot.title = element_text(family = "Cabin", size = 64, hjust = 0.5),
        plot.subtitle = element_text(size = 54, hjust = 0.5),
        plot.caption = element_text(size = 24, hjust = 0.5),
        plot.caption.position = "plot",
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

ggsave(plot = plot_26, path = here::here("2023/charts/"),
       filename = "26_localchange.png", width = 11, height = 8, unit = "in")
