library(tidyverse)
library(ggstraw)

# Data is from Tidy Tuesday, 2021 Week 31: https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-07-27/

olyData <- tidytuesdayR::tt_load(2021, week = 31)

olympics <- olyData$olympics %>%
  filter(noc == "USA") %>%
  na.omit()

