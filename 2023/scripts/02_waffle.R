library(tidyverse)
library(showtext)

font_add_google("Roboto")
font_add_google("Roboto Condensed")
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

plot_02 <- skiResort %>%
  ggplot(aes(fill = Price, values = n)) + 
  waffle::geom_waffle(n_rows = 22, colour = "white", make_proportional = FALSE, na.rm = TRUE) +
  MetBrewer::scale_fill_met_d("Hokusai2") +
  coord_equal() +
  labs(title = "Lift Ticket Prices (in USD) at North American Ski Resorts",
       subtitle = str_wrap("There are over 460 ski resorts in North America. Of these, approximately 40% have lift ticket prices that are less than $50 per day. The median price of a lift ticket in the dataset is $60."),
       caption = "Source: Aryan Juyal via Kaggle | #30DayChartChallenge | Day 2 | Waffle") +
  theme_void() +
  theme(text = element_text(family = "Roboto Condensed", size = 36),
        plot.background = element_rect(fill = "grey95", colour = "grey95"),
        panel.background = element_rect(fill = "grey95", colour = "grey95"),
        plot.title = element_text(family = "Roboto", size = 64, face = "bold", hjust = 0.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 48, hjust = 0.5, lineheight = 0.25),
        plot.caption = element_text(size = 12, hjust = 0.5, margin = margin(b = 10))) +
  guides(fill = guide_legend(nrow = 2))

ggsave(plot = plot_02, path = here::here("2023/charts/"), "02_waffle.png", 
       dpi = 320, height = 8, width = 11, unit = "in")
