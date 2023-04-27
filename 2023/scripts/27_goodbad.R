library(tidyverse)
library(showtext)
library(ggtext)

font_add_google("Cabin")
font_add_google("Cabin Condensed")
showtext_auto()

# Data from OpenIntro package, Pew Global Warming dataset

gwPew <- openintro::global_warming_pew %>%
  mutate(sentiment = case_when(response == "Don't know / refuse to answer"~"Neutral",
                               response == "Not warming"~"Bad",
                               response == "Earth is warming"~"Good", .default = response)) %>%
  group_by(party_or_ideology) %>%
  reframe(positivePerc = sum(sentiment == "Good") / n(),
          neutralPerc = sum(sentiment == "Neutral") / n(),
          negativePerc = sum(sentiment == "Bad") / n(),
          response, sentiment) %>%
  ungroup() %>%
  mutate(response = fct_relevel(response, levels(response)),
         party_or_ideology = factor(party_or_ideology, levels = c("Liberal Democrat", "Mod/Cons Democrat", 
                                                                  "Mod/Lib Republican", "Conservative Republican"))) %>%
  group_by(party_or_ideology, sentiment) %>%
  summarise(n = n()) %>%
  mutate(sentiment.n = sum(n),
         prop = (n / sum(n))) %>%
  ungroup()

text <-  glue::glue("A Pew Research Poll (2010) asked 1,306 Americans, ", 
                    "'From what you have read and heard, is there solid evidence that the average temperature on earth has been getting warmer over the past few decades, or not?' ", 
                    "This visualization explores the number of responses that were noted as the Earth is warming, is not warming, or do not know/refuse to answer. ", 
                    "These answers were then coded as",
                    "<span style='color:#7375e9'> **Good**</span>, ",
                    "<span style='color:#d9534f'>**Bad**</span>, or ",
                    "<span style='color:#f9f9f9'>**Neutral**</span>, respectively.",
                    "Negative sentiments surrounding climate change were found in higher percentages of responses with individuals who identify with Conservative Republican ideologies (n = 450, 61%), ",
                    "whereas positive sentiments were found in higher percentages of responses with individuals who identify with Liberal Democratic ideologies (n = 405, 90%).")

title <- "Climate Change Sentiments by Political Party or Ideology"


plot_27 <- gwPew %>%
  ggplot(aes(x = party_or_ideology, y = prop, width = sentiment.n, fill = sentiment)) +
  geom_col(position = "fill", colour = "grey5") +
  facet_grid(cols = vars(party_or_ideology), scale = "free", space = "fixed", shrink = FALSE,
             margins = margin(0, 0, 0, 0)) +
  scale_fill_manual(values = c("Good" = "#7375e9",
                               "Neutral" = "#f9f9f9",
                               "Bad" = "#d9534f")) +
  scale_y_continuous(position = "left", 
                     label = c("0", "25%", "50%", "75%", "100%")) +
  labs(x = "",
       y = "",
       title = title,
       subtitle = text,
       caption = "Source: {openintro} | #30DayChartChallenge | Day 27 | Good/Bad") +
  theme_void() +
  theme(text = element_text(family = "Cabin Condensed", size = 32),
        plot.background = element_rect(fill = colorspace::lighten("steelblue", amount = 0.3), color = NA),
        panel.background = element_rect(fill = colorspace::lighten("steelblue", amount = 0.3), color = NA),
        panel.spacing = unit(0, "lines"),
        plot.title = element_text(family = "Cabin", size = 56, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_textbox_simple(size = 36, lineheight = 0.25, margin = margin(t = 5, b = 10)),
        strip.text = element_text(size = 36, face = "bold"),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 28, margin = margin(r = 5, b = 5)),
        axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(hjust = 1, margin = margin(r = 1)),
        legend.position = "none",
        plot.margin = margin(r = 10, l = 10))

ggsave(plot = plot_27, path = here::here("2023/charts/"), "27_goodbad.png", dpi = 320, height = 8, width = 11, unit = "in")

