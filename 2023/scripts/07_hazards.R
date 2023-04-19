library(tidyverse)
library(showtext)
library(ggridges)
library(MetBrewer)
library(lubridate)

font_add_google("Cabin")
font_add_google("Cabin Condensed")
showtext_auto()

# Manually download data source from USGS:
# Avalanche occurrence records along the Going-to-the-Sun Road, Glacier National Park, Montana Data: https://doi.org/10.5066/P9BO1LHQ

avyData <- read_csv(here::here("2023/data/07_hazards.csv")) %>%
  mutate(Date = mdy(Date),
         Month = month(Date),
         Year = year(Date)) %>%
  select(c(ID, Date, Year, Month, AvalancheType))
