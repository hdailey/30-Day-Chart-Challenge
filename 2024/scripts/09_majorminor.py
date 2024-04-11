# -*- coding: utf-8 -*-
"""
30 Day Chart Challenge - Day 09 major/minor
11 April 2024
"""
# Libraries, Fonts, Data
## Libraries
import pandas as pd
import numpy as np
from matplotlib import *
from plotnine import *
import textwrap
from mizani.formatters import percent_format

## Fonts
rc("font", **{"family":"sans-serif", "sans-serif":["Cambria"]})

## Data
olympicAthletes = pd.read_csv("C:/Users/hdailey/OneDrive - Water Boards/Reference/DataViz/30-Day-Chart-Challenge/2024/data/09_majorminor.csv")

# Data Exploration and Cleaning
olympicAthletesGrouped = olympicAthletes.groupby(by = ["Year", "Season"]).count().reset_index()
olympicAthletesGrouped["Category"] = "All Athletes"
olympicAthletesGrouped = olympicAthletesGrouped[["Year", "ID"]]

olympicAthletesMinor = olympicAthletes.query("Age < 18")
olympicAthletesMinor = olympicAthletesMinor.dropna()
olympicAthletesMinor = olympicAthletesMinor.groupby(by = ["Year", "Season"]).count().reset_index()
olympicAthletesMinor["Category"] = "Minors (Under 18)"
olympicAthletesMinor = olympicAthletesMinor[["Year", "ID", "Category", "Season"]]
olympicAthletesMinor = olympicAthletesMinor.merge(olympicAthletesGrouped, left_on = "Year", right_on = "Year")
olympicAthletesMinor["PercMinor"] = olympicAthletesMinor["ID_x"]/olympicAthletesMinor["ID_y"]

# Data Visualization
plot09 = (ggplot()
          + geom_col(olympicAthletesMinor, aes(x = "Year", y = "PercMinor", fill = "Season"), show_legend = True,
                     position = position_dodge2(preserve = "single"), width = 6)
          + scale_y_continuous(labels = percent_format())
          + labs(x = "", y = "", fill = "",
                 title = "Proportion of Olympic Athletes Under 18",
                 subtitle = textwrap.fill("In general, Olympic Athletes that are under 18 make up less than 5% of the total athletes at the games.", width = 50),
                 caption = "Source: @rgriffin via kaggle.com | #30DayChartChallenge | Day 09: major/minor | @hdailey")
          + scale_fill_manual(values = ["#EE334E", "#0081C8"])
          + coord_flip()
          + theme_minimal()
          + theme(text = element_text(family = "Cambria", colour = "#000000"),
                  plot_title = element_text(hjust = 0.5, size = 15, face = "bold"),
                  plot_subtitle = element_text(hjust = 0.5, linespacing = 1.5, size = 8),
                  plot_caption = element_text(size = 4, hjust = 0.5),
                  panel_grid = element_blank(),
                  plot_background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
                  panel_background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
                  legend_position = "top",
                  legend_direction = "horizontal",
                  panel_grid_major_x = element_line(linetype = "dashed", size = 0.25, colour = "#B0AFAE"))
          + guides(fill = guide_legend(keywidth = 25, keyheight = 5, label_position = "top"))
          )

# Save
plot09.save("C:/Users/hdailey/OneDrive - Water Boards/Reference/DataViz/30-Day-Chart-Challenge/2024/charts/09_majorminor.png", dpi = 1200, width = 4.5, height = 5, units = "in")