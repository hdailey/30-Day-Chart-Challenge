# -*- coding: utf-8 -*-
"""
30 Day Chart Challenge - Day 10 physical
10 April 2024
"""
# Libraries, Fonts, Data
## Libraries
import pandas as pd
import numpy as np
from matplotlib import *
from plotnine import *
import textwrap

## Fonts
rc("font", **{"family":"sans-serif", "sans-serif":["Calibri"]})

## Data
olympicAthletes = pd.read_csv("10_physical.csv")

# Data Exploration and Cleaning
olympicAthletesGrouped = olympicAthletes.dropna()
olympicAthletesGrouped = olympicAthletesGrouped.groupby(["Year", "Sex"]).mean(["Weight"]).reset_index()
olympicAthletesGrouped = olympicAthletesGrouped.query("Year >= 1920")

# Data Visualization
plot10 = (ggplot()
          + geom_point(olympicAthletesGrouped, aes(x = "Year", y = "Weight", colour = "Sex"), size = 2, show_legend = False)
          + geom_line(olympicAthletesGrouped, aes(x = "Year", y = "Weight", colour = "Sex"), size = 1, show_legend = False)
          + geom_text(aes(x = 1920, y = 55, label = "1920"), size = 10, colour = "#FFFFFF", fontweight = "bold")
          + geom_text(aes(x = 1970, y = 55, label = "1970"), size = 10, colour = "#FFFFFF", fontweight = "bold")
          + geom_text(aes(x = 2016, y = 55, label = "2016"), size = 10, colour = "#FFFFFF", fontweight = "bold")
          + geom_text(aes(x = 1970, y = 80, label = "'Men'"), size = 10, colour = "#FCB131", fontweight = "bold")
          + geom_text(aes(x = 1970, y = 65, label = "'Women'"), size = 10, colour = "#EE334E", fontweight = "bold")
          + scale_y_continuous(breaks = (60, 65, 70, 75, 80))
          + scale_colour_manual(values = (["#EE334E", "#FCB131"]))
          + coord_fixed(ratio = 4)
          + labs(x = "", y = "",
                 title = "Olympic Athlete Weights (in Kg, 1920-2016)",
                 caption = textwrap.fill("Source: @rgriffin via Kaggle.com | #30DayChartChallenge | Day 10: physical | @hdailey inspired by @gkaramanis", width = 150))
          + theme_minimal()
          + theme(text = element_text(family = "Calibri", colour = "#FFFFFF"),
                  plot_title = element_text(face = "bold", size = 15, hjust = 0.5),
                  plot_caption = element_text(size = 5, hjust = 0.5, lineheight = 1.5),
                  panel_grid = element_blank(),
                  panel_background = element_rect(fill = "#00A651"),
                  plot_background = element_rect(fill = "#00A651"),
                  axis_text_x = element_blank(),
                  axis_ticks_major_x = element_blank(),
                  axis_text_y = element_text(size = 10, face = "bold"))
          )

plot10
# Save
plot10.save("10_physical.png", dpi = 1200, width = 4.2, height = 5, units = "in")
