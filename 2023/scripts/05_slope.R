library(tidyverse)
library(showtext)
library(MetBrewer)

font_add_google("Cabin")
font_add_google("Cabin Condensed")
showtext_auto()

# Data From OWID: https://ourworldindata.org/sanitation

sanitation <- read_csv(here::here("2023/data/05_slope.csv")) %>%
  filter(!is.na(san_sm),
         Year %in% c(2000, 2020)) %>%
  select(c(Entity, Year, san_sm)) %>%
  pivot_wider(names_from = Year, values_from = san_sm) %>%
  na.omit() %>%
  mutate(changeSan = `2020` - `2000`) %>%
  filter(changeSan >= 0) %>%
  pivot_longer(cols = -c('Entity', 'changeSan'),
               names_to = "Year",
               values_to = "san_sm") %>%
  arrange(Entity)

sanitationContinents <- sanitation %>%
  filter(Entity %in% c("World", "North America and Europe", "Central and Southern Asia",
                       "Eastern and South-Eastern Asia", "Western Asia and Northern Africa",
                       "Sub-Saharan Africa", "Australia and New Zealand"))

plot_05 <- sanitation %>%
  ggplot(aes(Year, san_sm, group = Entity)) +
  geom_line(data = sanitation, 
            aes(colour = changeSan), linewidth = 0.5, alpha = 0.15) +
  geom_line(data = sanitationContinents,
            aes(colour = changeSan), linewidth = 1) +
  geom_point(data = sanitation, aes(colour = changeSan), size = 1, alpha = 0.15) +
  geom_point(data = sanitationContinents, 
             fill = "white", colour = "black",
             shape = 21, size = 2, stroke = 1.5) +
  ggrepel::geom_text_repel(data = sanitationContinents %>% filter(Year == 2020),
                           aes(label = glue::glue("{Entity}: ", "{format(abs(changeSan), digits = 3)}%")),
                           hjust = 0, size = 12, family = "Cabin Condensed",
                           direction = "y", force = 0.5, min.segment.length = 0,
                           segment.size = 0.5, lineheight = 0.25) +
  scale_color_met_c(name = "Paquin", direction = -1) + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = "",
       y = "",
       title = "Percentage of the Population with Access to Safely Managed Sanitation",
       subtitle = "OWID has defined safely managed sanitation as *improved facilities which are not shared with other households and where
excreta are safely disposed in situ or transported and treated off-site*. Shown here is the percentage of the global population with access to safely managed sanitation. Highlighted are the major continental areas with approximately 25-percent of the global population having access to safely managed sanitation.",
       caption = "Source: Our World in Data | #30DayChartChallenge | Day 5 | Slope") +
  theme_minimal() +
  theme(text = element_text(family = "Cabin Condensed", size = 40),
        plot.background = element_rect(fill = "grey95", colour = "grey95"),
        panel.background = element_rect(fill = "grey95", colour = "grey95"),
        plot.title = element_text(family = "Cabin", size = 48, face = "bold", hjust = 0.5),
        plot.subtitle = ggtext::element_textbox_simple(lineheight = 0.5, size = 42, hjust = 0, margin = margin(t = 10, r = 20, b = 10)),
        plot.caption.position = "plot",
        plot.caption = element_text(margin = margin(b = 10)),
        legend.position = "top",
        legend.justification = "center",
        ) +
  guides(colour = guide_colorbar(title = str_wrap("Change in access to safe sanitation (%)", width = 50), 
                                 title.position = "top", 
                                 title.lineheight = 0.5,
                                 title.hjust = 0.5, title.vjust = 1,
                                 title.justification = "center",
                                 ticks = TRUE, 
                                 frame.colour = "black", 
                                 barheight = 1.5, barwidth = 15,
                                 label.vjust = 2.5))

ggsave(plot = plot_05, path = here::here("2023/charts/"), "05_slope.png", dpi = 320, height = 11, width = 8, unit = "in")
