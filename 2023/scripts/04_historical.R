library(tidyverse)
library(showtext)

font_add_google("Cabin")
font_add_google("Cabin Condensed")
showtext_auto()

# Data is from #TidyTuesday 2020 - June 16, 2020: https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-06-16/
tuesdata <- tidytuesdayR::tt_load(2020, week = 25)

routes <- tuesdata$slave_routes
