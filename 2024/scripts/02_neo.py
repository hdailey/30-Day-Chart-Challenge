# -*- coding: utf-8 -*-
"""
30 Day Chart Challenge - Day 02 neo
03 April 2024
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
waste = pd.read_csv("02_OECD_Waste.csv")

# Data Exploration and Cleaning
wasteALL = waste.loc[waste["Unit of measure"] == "Percentage of treated waste"]
wasteALL = waste.loc[waste["TIME_PERIOD"] == 2016]
wasteALL["REF_AREA"] = wasteALL["REF_AREA"].astype("category")
wasteALL["Reference area"] = wasteALL["Reference area"].astype("category")

# Data Visualization
plot02 = (ggplot(wasteALL, aes(x = "Reference area", y = "OBS_VALUE", fill = "Measure"))
          + geom_col(stat = "identity", position = "fill", width = 0.5)
          + scale_fill_brewer(type = "qual", palette = 2)
          + labs(x = "", y = "Percentage", fill = "",
                 title = "G7 Municipal Waste Recovery Options",
                 subtitle = textwrap.fill("Today we look at waste recovery options via the OECD dataset. While countries like Japan continue utilizing methods such as incineration for waste recovery, European countries have a greater proportion of waste recovery utiling composting than their other G7 counterparts in 2016.", width = 85),
                 caption = "#30DayChartChallenge | Day 2: neo | @hdailey\nSource: OECD Annual Quality Assurance (AQA) questionnaire(jointly collected with Eurostat) for OECD countries.UNSD, Country Files from the UNSD/UNEP data collection on environment\nstatistics (available at: https://unstats.un.org/unsd/envstats/country_files) for non-OECD countries.")
          + coord_flip()
          + theme_classic()
          + theme(text = element_text(family = "Calibri"),
                  plot_title = element_text(face = "bold"),
                  plot_caption = element_text(size = 4, linespacing = 1, ha = "left"),
                  legend_position = "top",
                  legend_direction = "horizontal"))
# Save
plot02.save("02_neo.png", dpi = 1200)