library(tidyverse)
library(showtext)
library(ggtext)

font_add_google("Dosis")
showtext_auto()

# Data from Tidy Tuesday 2023, Week 9
tuesdata <- tidytuesdayR::tt_load(2023, week = 9)

afriSenti <- tuesdata$afrisenti
langs <- tuesdata$languages
langScript <- tuesdata$language_scripts
langCountries <- tuesdata$language_countries
countryRegions <- tuesdata$country_regions

afriSentiJoin <- afriSenti %>%
  left_join(langCountries, by = "language_iso_code", multiple = "all") %>%
  left_join(langs, by = "language_iso_code", multiple = "all") %>%
  left_join(langScript, by = "language_iso_code", multiple = "all") %>%
  left_join(countryRegions, by = "country", multiple = "all") %>%
  mutate(language = ifelse(language == "Algerian Arabic/Darja", "Darja", language),
         language = ifelse(language == "Moroccan Arabic/Darija", "Darija", language),
         language = ifelse(language == "Nigerian Pidgin", "Pidgin", language),
         language = ifelse(language == "Mozambican Portuguese", "Mozambican", language)) %>%
  group_by(language) %>%
  reframe(positive_perc = sum(label == "positive") / n(),
          negative_perc = sum(label == "negative") / n(),
          neutral_perc = sum(label == "neutral") / n(),
          label) %>%
  ungroup() %>%
  mutate(language = fct_relevel(language, levels(language))) %>%
  group_by(language, label) %>%
  summarise(n = n()) %>%
  mutate(label.n = sum(n),
         prop = n / sum(n)) %>%
  ungroup()

rm(list = c("afriSenti", "langs", "langScript", "langCountries", "countryRegions"))

plot_15 <- afriSentiJoin %>%
  ggplot(aes(x = language, y = prop, width = label.n, fill = label)) +
  geom_col(position = "fill", color = "black") +
  geom_text(aes(x = 0.05, y = 0.02, label = language), stat = "identity",
            fontface = "bold", color = "black", size = 6, angle = 90, hjust = 0,
            check_overlap = TRUE) +
  ggtitle("Sentiment Analysis of Tweets in the African Languages",
          subtitle = paste0("Percentages of tweets in 14 different African languages and proportions of ",
                            "<span style='color:seagreen'>",
                            "**positive**</span>, ", 
                            "<span style='color:orange'>",
                            "**neutral**</span> or ",
                            "<span styles='color:tomato'>",
                            "**negative**</span> ",
                            "labels assigned by native speakers.")) +
  labs(caption = "Source: **AfriSenti** via **@Shmuhammadd** | #30DayChartChallenge | Day 15 | Positive/Negative") +
  facet_grid(cols = vars(language), scale = "free", space = "free", shrink = TRUE) +
  scale_fill_manual(values = c("positive" = "seagreen",
                               "neutral" = "orange",
                               "negative" = "tomato")) +
  theme_void() +
  scale_size(guide = "none") +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        text = element_text(family = "Dosis"),
        plot.background = element_rect(fill = "gray25",
                                       color = "gray25"),
        panel.spacing = unit(0, "lines"),
        legend.position = "none",
        plot.title = element_markdown(color = "white", face = "bold", size = 42),
        plot.subtitle = element_markdown(color = "white", size = 20),
        plot.caption = element_markdown(color = "white", size = 14),
        plot.margin = margin(0.5, 0.25, 0.5, 0.25, unit = "cm"))

ggsave(plot = plot_15, path = here::here("2023/charts"),
       paste0("15", "_positivenegative", ".png"), scale = 0.5, dpi = 300,
       height = 8, width = 11, unit = "in")
