library(tidyverse)
library(showtext)
library(ggforce)

font_add_google("Cabin")
font_add_google("Cabin Condensed")
showtext_auto()

# Data is from #TidyTuesday 2020 - June 16, 2020: https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-06-16/
tuesdata <- tidytuesdayR::tt_load(2020, week = 25)

routes <- tuesdata$slave_routes %>%
  filter(!is.na(n_slaves_arrived)) %>%
  select(c(voyage_id, year_arrival, n_slaves_arrived)) %>%
  mutate(decade_arrival = (year_arrival - (year_arrival%%10))) %>%
  group_by(decade_arrival) %>%
  summarise(n = sum(n_slaves_arrived)) %>%
  mutate(n = -n)

plot_04 <- routes %>%
  ggplot(aes(x = decade_arrival, y = n)) +
  geom_col(fill = "grey80") +
  annotate("text", x = 1600, y = -125000, family = "Cabin", 
           label = "TRANS-ATLANTIC SLAVE TRADE",
           hjust = 0.5, size = 18, colour = "red") + 
  annotate("text", x = 1600, y = -150000, family = "Cabin Condensed",
           label = str_wrap("Over a span of more than 400 years, more than 12 million enslaved Africans were forced across the Atlantic Ocean by European colonizers. This graphic explores the number of slaves transported by decade (~5 million people), as documented in the records of the more than 36,000 voyages between 1514 and 1866.", 85), hjust = 0.5, vjust = 1, size = 14, lineheight = 0.5, colour = "grey80") +
  scale_y_continuous(position = "right", 
                     label = c("500,000", "400,000", "300,000", "200,000", "100,000", "0")) +
  scale_x_continuous(position = "top",
                     labels = c(1510, 1600, 1700, 1800, 1860)) +
  labs(caption = "Source: US Census, Slave Voyages and Black Past | #30DayChartChallenge | Day 4 | Historical | Inspired by @gkaramanis") +
  theme_void() +
  theme(text = element_text(family = "Cabin Condensed"),
        panel.background = element_rect(color = "grey10", fill = "grey10"),
        plot.background  = element_rect(color = "grey10", fill = "grey10"),
        axis.text.x.top = element_text(color = "red", face = "bold", size = 36, margin = margin(t = 20, b = -15)), 
        panel.grid.major.y = element_line(colour = "grey40", linewidth = 0.25),
        panel.grid.minor.y = element_line(colour = "grey40", linewidth = 0.1),
        axis.text.y = element_text(colour = "red", hjust = 0, face = "bold", size = 36, margin = margin(0, 0, 0, 5)),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 28, colour = "grey80"),
        plot.margin = margin(25, 25, 25, 25))

ggsave(plot = plot_04, path = here::here("2023/charts/"), "04_historical.png", dpi = 320, height = 8, width = 11, unit = "in")
