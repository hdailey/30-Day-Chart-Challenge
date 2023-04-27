library(tidyverse)
library(showtext)
library(ggtext)

# Manually download data for Sacramento County, California - PRISM OSU Group: https://prism.oregonstate.edu/explorer/

font_add_google("Cabin")
font_add_google("Cabin Condensed")
showtext_auto()

tempData <- read_csv(here::here("2023/data/28_trend.csv")) %>%
  select(-c("PPT_INC"))

plot_28 <- tempData %>%
  ggplot() +
  geom_ribbon(mapping = aes(x = Date, ymin = TMIN_DEGF, ymax = TMAX_DEGF),
              alpha = 0.4, fill = "grey95") +
  geom_line(mapping = aes(x = Date, y = TMIN_DEGF), colour = "#57d3f5") +
  geom_line(mapping = aes(x = Date, y = TMEAN_DEGF, colour = TMEAN_DEGF), linewidth = 1) +
  geom_line(mapping = aes(x = Date, y = TMAX_DEGF), colour = "#f44336") +
  scale_colour_distiller(palette = "Greens") +
  coord_polar() +
  labs(x = "",
       y = "",
       title = "Annual Temperature (in °F) for Sacramento County, California",
       subtitle = glue::glue("Parameter-elevation Regressions on Independent Slopes Model (PRISM) is a set of temporal gridded data products of mean, min and max temperatures,
       mean precipitation, and dewpoints for the United States. PRISM uses a weighted regression scheme to account for complex climate regimes associated with orography, slope aspect,
       coastal proximity and other factors.",
                             "\n\n",
                             "Shown below is the annual 
                             <span style='color:#57d3f5'>**minimum**</span>,
                             <span style='color:#f44336'>**maximum**</span> and 
                             <span style='color:seagreen'>**average**</span> temperature for Sacramento County, California from 1895 through 2022. 
                             Overall, annual maximum and average temperatures have increased during the reported time period. Maximum temperatures have increased from 70.8 to 76.7 and average temperatures have increased from 59.4 to 62.4 in 1895 and 2020, respectively.
                             Minimum temperatures do not illustrate this same increase with values in 1895 being equal to those in 2022."),
       caption = "Source: PRISM Climate Group, Oregon State University | #30Day Chart Challenge | Day 28 | Trend | Inspired by @nrennie") +
  theme_void() +
  theme(text = element_text(family = "Cabin Condensed", colour = "grey95", size = 36),
        plot.background = element_rect(fill = colorspace::lighten("grey10", amount = 0.3), colour = colorspace::lighten("grey10", amount = 0.3)),
        panel.background = element_rect(fill = colorspace::lighten("grey10", amount = 0.3), colour = colorspace::lighten("grey10", amount = 0.3)),
        plot.title = element_text(family = "Cabin", size = 64, face = "bold", hjust = 0.5, margin = margin(t = -10)),
        plot.subtitle = element_textbox_simple(size = 36, halign = 0.5, lineheight = 0.5, colour = "grey95", 
                                               margin = margin(t = 10, r = 5, b = 5, l = 5)),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 5, b = 1)),
        plot.margin = margin(t = 20, b = 10), 
        axis.text.x = element_text(size = 36, colour = "grey95"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(size = 36)) +
  guides(colour = guide_colorbar(title = "Annual Mean Temperature (°F)", title.position = "top", title.hjust = 0.5,
                                 barwidth = 10, barheight = 1, frame.colour = "grey95", ticks = FALSE, 
                                 label.hjust = 0.5, label.theme = element_text(size = 32, family = "Cabin Condensed",
                                                                               colour = "grey95",
                                                                               margin = margin(t = -10))))

ggsave(plot = plot_28, path = here::here("2023/charts/"), "28_trend.png", dpi = 320, height = 11, width = 8, unit = "in")