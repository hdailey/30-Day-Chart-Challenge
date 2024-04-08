# -*- coding: utf-8 -*-
"""
30 Day Chart Challenge - Day 08 circular
08 April 2024
"""
# Libraries, Fonts, Data
## Libraries
import pandas as pd
import numpy as np
from matplotlib import *
import matplotlib.pyplot as plt
import circlify
import textwrap

## Fonts
rc("font", **{"family":"sans-serif", "sans-serif":["Calibri"]})

## Data
olympics = pd.read_csv("C:/Users/hdailey/OneDrive - Water Boards/Reference/DataViz/30-Day-Chart-Challenge/2024/data/08_circular.csv")


# Data Exploration and Cleaning
olympicsFiltered = olympics.query("Type == 'summergames'")
olympicsFiltered = olympicsFiltered.groupby("Country").count().reset_index()
olympicsFiltered["n"] = olympicsFiltered["Year"]
olympicsFiltered = olympicsFiltered[["Country", "n"]]
olympicsFiltered = olympicsFiltered.sort_values(by = ["n"], ascending = False)
olympicsFiltered = olympicsFiltered.query("n >= 2")

olympicsFilteredList = [{"id": "Japan", 'datum': 2},
                        {"id": "Greece", 'datum': 2},
                        {"id": "Germany", 'datum': 2},
                        {"id": "Great Britain", 'datum': 3},
                        {"id": "France", 'datum': 3},
                        {"id": "United States", 'datum': 5}]

olympicsCircles = circlify.circlify(olympicsFilteredList,
                                    show_enclosure = False,
                                    target_enclosure = circlify.Circle(x = 0, y = 0, r = 1)
                                    )
## Reverse order
olympicsCircles = olympicsCircles[::-1]

# Data Visualization
## Create Figure
fig, ax = plt.subplots(figsize = (10, 10))

## No Axis
ax.axis("off")

## Add Title
ax.set_title("Top Summer Olympics Game Hosts (1896-2028)", size = 24, weight = "bold")

## Add Caption
plt.figtext(0.25, 0.1, "Source: @Pietrfm via Kaggle.com | #30DayChartChallenge | Day 08: circular | @hdailey", fontsize = 10)

## List of Circle Labels & Count Labels
labels = olympicsFiltered["Country"].tolist()
counts = olympicsFiltered["n"].tolist()
colors = ["#0081C8", "#FCB131", "#FCB131", "#00A651", "#EE334E", "#EE334E"]

## Find Axis Limits
lim = max(
    max(abs(circle.x) + circle.r,
              abs(circle.y) + circle.r,
          )
    for circle in olympicsCircles
    )
plt.xlim(-lim-0.1, lim+0.1)
plt.ylim(-lim, lim)

## Plot Circles
for circle, label, count, color in zip(olympicsCircles, labels, counts, colors):
    x, y , r = circle
    ax.add_patch(plt.Circle((x, y), r, linewidth = 2, color = color, alpha = 0.8))
    plt.annotate(label,
                 (x, y),
                 va = "center",
                 ha = "center",
                 size = 14,
                 weight = "bold")
    plt.annotate(count,
                 (x, y - 0.05),
                 va = "center",
                 ha = "center")


# # Save
plt.savefig("C:/Users/hdailey/OneDrive - Water Boards/Reference/DataViz/30-Day-Chart-Challenge/2024/charts/08_circular.png", dpi = 600)