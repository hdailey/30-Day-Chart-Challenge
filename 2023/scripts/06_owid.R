library(tidyverse)
library(showtext)

font_add_google("Cabin")
font_add_google("Cabin Condensed")
showtext_auto()

# Data is from OWID Greenhouse Gas Emissions: https://ourworldindata.org/co2-and-greenhouse-gas-emissions

co2OWID <- read_csv(here::here("2023/data/06_owid.csv")) %>%
  filter(Year >= 1950,
         Entity %in% c("North America", "South America", "Asia",
                       "Europe", "Oceania"))

plot_06 <- ggplot(data = co2OWID, aes(x = Year, y = `Annual CO₂ emissions (zero filled)`)) +
  geom_area(aes(group = Entity, colour = Entity, fill = Entity)) +
  scale_y_continuous(position = "left", 
                     label = c("0", "10 Billion", "20 Billion", "30 Billion", "40 Billion")) +
  ggrepel::geom_label_repel(data = co2OWID %>% filter(Year == 2020), aes(label = glue::glue("{Entity}: ", 
                                                                  "{format(`Annual CO₂ emissions (zero filled)`,
                                                                  digits = 9, big.mark = ',')}",
                                                                  " tons"),
                                                                  colour = Entity),
                            nudge_x = -50, alpha = 0.9, min.segment.length = 100,
                            hjust = 0, size = 16, family = "Cabin Condensed") +
  scale_fill_manual(values = c("North America" = "#00AB66", "South America" = "#BD35BD", "Asia" = "#BD3455", 
                               "Europe" = "#5779C1", "Oceania" = "#B67E11")) +
  scale_colour_manual(values = c("North America" = "#00AB66", "South America" = "#BD35BD", "Asia" = "#BD3455", 
                                 "Europe" = "#5779C1", "Oceania" = "#B67E11")) +
  labs(x = "",
       y = "Annual CO₂ emissions (tons)",
       title = "Annual CO₂ emissions by World Region",
       subtitle = "This visualization explores fossil fuel and industry emissions from 1950 through 2020 for the major world regions.
       Shown in the labels are the major world region with the total carbon dioxide emissions from 2020.",
       caption = "Source: OWID | #30DayChartChallenge | Day 6 | Data Day: OWID") +
  cowplot::theme_minimal_hgrid()+
  theme(text = element_text(family = "Cabin Condensed", size = 34, colour = "grey5"),
        panel.background = element_rect(fill = "grey95", colour = "grey95"),
        plot.background = element_rect(fill = "grey95", colour = "grey95"),
        plot.title = element_text(family = "Cabin", size = 48, face = "bold"),
        plot.subtitle = ggtext::element_textbox_simple(size = 40, lineheight = 0.25),
        axis.text = element_text(size = 34),
        axis.line.x = element_line(colour = "grey10", linewidth = 0.1),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 28),
        panel.border = element_rect(colour = "grey10"),
        legend.position = "none")

ggsave(plot = plot_06, path = here::here("2023/charts/"), "06_owid.png", dpi = 320, height = 8, width = 11, unit = "in")

