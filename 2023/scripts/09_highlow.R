library(tidyverse)
library(ggbump)
library(ggtext)
library(showtext)

font_add_google("Cabin")
font_add_google("Cabin Condensed")
showtext_auto()

# Manually download data source from OWID, same data from Day 8:
# Migration Data: https://ourworldindata.org/migration

humans <- read_csv(here::here("2023/data/08_humans.csv")) %>%
  filter(Year %in% c(2000, 2005, 2010, 2015, 2020)) %>%
  select(c(Year, Country, `Net migration rate`)) %>%
  group_by(Country) %>%
  arrange(`Net migration rate`) %>%
  na.omit()

btm5 <- humans %>%
  filter(Country %in% c("Georgia", "Syria", "Guyana"))

top5 <- humans %>%
  filter(Country %in% c("United Arab Emirates", "Bahrain", "Kuwait")) %>%
  mutate(Country = case_when(Country == "United Arab Emirates"~"UAE", .default = Country))


plot_09 <- ggplot(data = humans) +
  geom_bump(data = btm5, aes(x = Year, y = `Net migration rate`, group = Country), size = 1,
            colour = "grey50") +
  geom_bump(data = top5, aes(x = Year, y = `Net migration rate`, group = Country), size = 1,
            colour = "grey50") +
  geom_bump(data = btm5 %>% filter(Country == "Syria"), aes(x = Year, y = `Net migration rate`),
            size = 2, colour = "#D76067") +
  geom_bump(data = top5 %>% filter(Country == "Bahrain"), aes(x = Year, y = `Net migration rate`),
            size = 2, colour = "#66CDAA") +
  geom_point(data = top5 %>% filter(Year %in% c(2000, 2020)),
             aes(x = Year + 0.05, y = `Net migration rate`),
             size = 3, color="grey50") +
  geom_point(data = btm5 %>% filter(Year %in% c(2000, 2020)),
             aes(x = Year + 0.05, y = `Net migration rate`),
             size = 3, color = "grey50") +
  geom_text(data = top5 %>% filter(Year == 2000),
            aes(x = Year - 0.3, y = `Net migration rate`,
                label = str_wrap(Country)),
            hjust = 1, vjust = 0.5, family = "Cabin", fontface = "bold", size = 12) +
  geom_text(data = top5 %>% filter(Year == 2020),
            aes(x = Year + 0.3, y = `Net migration rate`,
                label = str_wrap(Country)),
            hjust = 0, vjust = 0.5, family = "Cabin", fontface = "bold", size = 12) +
  geom_text(data = btm5 %>% filter(Year == 2000),
            aes(x = Year - 0.3, y = `Net migration rate`,
                label = str_wrap(Country)),
            hjust = 1, vjust = 0.5, family = "Cabin", fontface = "bold", size = 12) +
  geom_text(data = btm5 %>% filter(Year == 2020),
            aes(x = Year + 0.3, y = `Net migration rate`,
                label = str_wrap(Country)),
            hjust = 0, vjust = 0.5, family = "Cabin", fontface = "bold", size = 12) +
  theme_void() +
  labs(title = "Global Migration in 2020",
       subtitle = "A different visualization with the same data from Day 8. The top and bottom three countries with the largest increase and decrease in net migration rate are shown from 2000 through 2020.
       Negative net migration rates are associated with a percentage of the population emigrating, and positive net migration rates are associated with a percentage of the population immigrating.",
       caption = "Source: OWID | #30DayChartChallenge | Day 9 | High Low") +
  theme(text = element_text(family = "Cabin Condensed", size = 36),
        plot.background = element_rect(fill = "grey95", colour = "grey95"),
        panel.background = element_rect(fill = "grey95", colour = "grey95"),
        plot.title = element_text(family = "Cabin", hjust = 0.5, size = 54, face = "bold",
                                  margin = margin(5, 10, 5, 10)),
        plot.subtitle = element_textbox_simple(halign = 0.5, size = 42, lineheight = 0.5,
                                               margin = margin(5, 10, 5, 10)),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 10, b = 5)),
        axis.text.x = element_text(face = "bold"),
        plot.margin = margin(r = 10, b = 10, l = 10))

ggsave(plot = plot_09, path = here::here("2023/charts/"),
       "09_highlow.png", dpi = 320, height = 8, width = 11, unit = "in")

