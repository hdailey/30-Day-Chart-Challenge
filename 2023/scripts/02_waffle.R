library(tidyverse)
library(waffle)
library(showtext)

font_add_google("Roboto")
showtext_auto()

# Manually download data source from Kaggle:
# North American Ski Resort Data via Aryan Juyal: https://www.kaggle.com/datasets/aryanjuyal/comparison-of-north-american-ski-resorts


skiResort <- read_csv(here::here("2023/data/02_waffle.csv")) %>%
  mutate(`Less Than $50 (USD)` = ifelse(`Lift ticket (USD)` <= 50, 1, 0),
         `Less Than $100 (USD)` = ifelse(`Lift ticket (USD)` <= 100 & `Lift ticket (USD)` > 50, 1, 0),
         `Less Than $150 (USD)` = ifelse(`Lift ticket (USD)` <= 150 & `Lift ticket (USD)` > 100, 1, 0),
         `Greater Than $150 (USD)` = ifelse(`Lift ticket (USD)` > 150, 1, 0)) %>%
  pivot_longer(cols = `Less Than $50 (USD)`:`Greater Than $150 (USD)`,
               names_to = "Price") %>%
  filter(value == 1) %>%
  select(Resort, Price) %>%
  na.omit() %>%
  group_by(Price) %>%
  summarise(n = n())

plot_02 <- waffle(parts = skiResort$Price, rows = 8)
                         