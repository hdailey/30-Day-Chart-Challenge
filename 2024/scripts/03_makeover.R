# Day 03 - Makeover, 04 April 2023

# Libraries, Fonts, Data
## Libraries
library(tidyverse)
library(showtext)

## Fonts
font_add_google("Barlow")
font_add_google("Barlow Condensed")
showtext_auto()

## Data
waste <- read_csv(here::here("2024/data/02_OECD_Waste.csv"))

# Data Exploration
wasteALL <- waste %>%
  filter(`Unit of measure` == "Percentage of treated waste",
         TIME_PERIOD == 2016) %>%
  arrange(desc(OBS_VALUE)) %>%
  mutate(`Reference area` = fct_inorder(`Reference area`))

# Data Visualization
plot03 <- wasteALL %>%
  ggplot(aes(x = `Reference area`, y = OBS_VALUE, fill = Measure)) +
  geom_col(position = "fill", width = 0.5) +
  labs(x = "", 
       y = "",
       fill = "",
       title = "G7 Municipal Waste Recovery Options",
       subtitle = glue::glue("Today we look at waste recovery options via the OECD dataset. While countries like Japan ", 
                             "continue utilizing methods such as incineration for waste recovery, European countries have ", 
                             "a greater proportion of waste recovery utiling composting than their other G7 counterparts in 2016."),
       caption = glue::glue("#30DayChartChallenge | Day 3: makeover | @hdailey",
                            "<br>",
                            "Source: OECD Annual Quality Assurance (AQA) questionnaire(jointly collected with Eurostat) for OECD countries. ",
                            "UNSD, Country Files from the UNSD/UNEP data collection on environment statistics")) +
  scale_fill_manual(values = wesanderson::wes_palette("Darjeeling1", n = 4)) +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_flip() +
  theme_minimal() +
  guides(fill = guide_legend(byrow = TRUE, keywidth = 2, keyheight = 1)) +
  theme(text = element_text(family = "Barlow Condensed", size = 48, colour = "grey5"),
        plot.title = element_text(face = "bold", family = "Barlow", size = 64),
        plot.subtitle = ggtext::element_textbox_simple(lineheight = 0.25, size = 56, margin = margin(b = 10)),
        plot.title.position = "plot",
        plot.caption = ggtext::element_textbox_simple(lineheight = 0.25, colour = "grey55"),
        plot.caption.position = "plot",
        axis.text.y = element_text(face = "bold"),
        axis.text.x = element_text(colour = "grey5"),
        legend.direction = "horizontal",
        legend.position = "top",
        legend.margin = margin(l = -100),
        legend.text = element_text(size = 52, margin = margin(l = -20)),
        panel.grid.major = element_line(linetype = "dashed", colour = "grey5", linewidth = 0.1),
        panel.background = element_rect(fill = "grey95", colour = "grey95"),
        plot.background = element_rect(fill = "grey95", colour = "grey95"))


# Save
ggsave(plot = plot03, path = here::here("2024/charts/"), "03_makeover.png", dpi = 640)
