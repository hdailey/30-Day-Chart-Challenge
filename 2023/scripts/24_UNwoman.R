library(tidyverse)
library(showtext)
library(ggtext)
library(cowplot)

font_add_google("Cabin")
font_add_google("Cabin Condensed")
showtext_auto()

# Manually download data from https://population.un.org/
# Population 1950-2100, all scenarios

pop <- read_csv(here::here("2023/data/24_UNwoman.csv")) %>%
  filter(ISO2_code == "US") %>%
  mutate(percFemale = (PopFemale/PopTotal)*100) %>%
  filter(Time <= 2021) 

plot_24 <- ggplot(data = pop, aes(x = Time, y = (PopFemale/10000))) +
  geom_col(aes(x = Time, y = (PopFemale/10000)), fill = "#F7A9A9") +
  ggrepel::geom_label_repel(data = pop %>% filter(Time %in% c(1950, 1975, 2000, 2021)),
                            aes(label = glue::glue("{Time}: ", "{format(percFemale, digits = 3)}", "%")),
                            nudge_y = 1, family = "Cabin Condensed", size = 12) +
  scale_y_continuous(position = "left", 
                     label = c("0", "5K", "10K", "15K", "20K")) +
  labs(x = "",
       y = "Number of Individuals",
       title = "Population (Female) in the United States 1950-2021",
       subtitle = "Data are presented in the ten thousands (K)",
       caption = "Source: Population Prospects via UN | #30DayChartChallenge | Day 24 | Theme Day: UN Women") +
  theme_minimal_grid() +
  theme(text = element_text(family = "Cabin Condensed", size = 36),
        panel.background = element_rect(fill = "grey95", colour = "grey95"),
        plot.background = element_rect(fill = "grey95", colour = "grey95"),
        plot.title = element_textbox_simple(family = "Cabin", face = "bold", hjust = 0, size = 64),
        plot.subtitle = element_textbox_simple(size = 48),
        axis.text = element_text(size = 36),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 5, b = 5), size = 28),
        plot.caption.position = "plot")

ggsave(plot = plot_24, path = here::here("2023/charts/"), "24_UNwomen.png", dpi = 320, height = 8, width = 11, unit = "in")
