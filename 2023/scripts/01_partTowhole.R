library(tidyverse)
library(MetBrewer)
library(showtext)

font_add_google("Barlow")
font_add_google("Barlow Condensed")
showtext_auto()

# Manually download data source from OWID:
# Agricultural Production Data: https://ourworldindata.org/agricultural-production#explore-data-on-agricultural-production

cropData <- read_csv(here::here("2023/data/01_partOfWhole_crops.csv")) %>%
  filter(Country %in% c("Africa (FAO)", "Asia (FAO)", "Americas (FAO)",
                        "Europe (FAO)", "Oceania (FAO)")) %>%
  mutate(Country = case_when(Country == "Africa (FAO)"~"Africa", .default = Country),
         Country = case_when(Country == "Asia (FAO)"~"Asia", .default = Country),
         Country = case_when(Country == "Americas (FAO)"~"Americas", .default = Country),
         Country = case_when(Country == "Europe (FAO)"~"Europe", .default = Country), 
         Country = case_when(Country == "Oceania (FAO)"~"Oceania", .default = Country)) %>%
  filter(Year == 2020) %>%
  select(c(Product, Country, Year, `Production (t)`)) %>%
  pivot_wider(names_from = "Product",
               values_from = "Production (t)") %>%
  group_by(Country, Year) %>%
  rowwise() %>%
  mutate(productionTotal = sum(c_across(Cereals:Vegetables)),
         cerealPerc = (Cereals/productionTotal)*100,
         rtPerc = (`Roots and Tubers`/productionTotal)*100,
         pulsesPerc = (Pulses/productionTotal)*100,
         fruitsPerc = (Fruits/productionTotal)*100,
         vegPerc = (Vegetables/productionTotal)*100) %>%
  select(c(Country, Year, cerealPerc:vegPerc)) %>%
  pivot_longer(cols = cerealPerc:vegPerc,
               names_to = "crops") %>%
  mutate(crops = case_when(crops == "cerealPerc"~"Cereals", .default = crops),
         crops = case_when(crops == "rtPerc"~"Roots and Tubers", .default = crops),
         crops = case_when(crops == "pulsesPerc"~"Pulses", .default = crops),
         crops = case_when(crops == "fruitsPerc"~"Fruits", .default = crops),
         crops = case_when(crops == "vegPerc"~"Vegetables", .default = crops))

plot_01 <- cropData %>%
  ggplot(aes(x = value, y = Country, fill = crops, label = paste0(round(value), "%"))) +
  geom_col(width = 0.7, position = "stack") +
  geom_text(position = position_stack(vjust = 0.5),
                           data = cropData[cropData$value > 1,],
            colour = "#F9F9F9",
            fontface = "bold",
            size = 8)+
  labs(x = "",
       y = "",
       title = "Global Crop Production by Continent, 2020",
       subtitle = "Global crop production for cereals, fruits, pulses, roots/tubers and vegetables. Crop production is measured in tons.",
       caption = "Source: Our World in Data | #30DayChartChallenge | Day 1 | Part-to-Whole") +
  scale_fill_met_d("Wissing", direction = -1) +
  cowplot::theme_minimal_hgrid() +
  theme(text = element_text(family = "Barlow Condensed", colour = "grey15", size = 32),
        plot.background = element_rect(color = "#F9F9F9", fill = "#F9F9F9"),
        panel.background = element_rect(color = "#F9F9F9", fill = "#F9F9F9"),
        plot.title = element_text(face = "bold", family = "Barlow", size = 72, colour = "grey15",
                                  margin = margin(t = 10, b = 4)),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 48, family = "Barlow Condensed", colour = "grey15"),
        plot.caption = element_text(hjust = 1, size = 26, family = "Barlow Condensed", 
                                    colour = "grey15"),
        plot.caption.position = "plot",
        axis.text = element_text(family = "Barlow Condensed", colour = "grey15", size = 32),
        legend.position = "top",
        legend.justification = "left",
        legend.title = element_blank(),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(1, "cm"),
        legend.margin = margin(b = -25),
        legend.text = element_text(size = 26, family = "Barlow Condensed", color = "grey15"))

ggsave(plot = plot_01, path = here::here("2023/charts/"), "01_partTowhole.png", dpi = 320, height = 8, width = 11, unit = "in")
