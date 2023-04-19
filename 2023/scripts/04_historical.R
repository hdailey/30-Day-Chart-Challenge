library(tidyverse)
library(showtext)

font_add_google("Cabin")
font_add_google("Cabin Condensed")
showtext_auto()

tuesdata <- tidytuesdayR::tt_load(2020, week = 25)

routes <- tuesdata$slave_routes
