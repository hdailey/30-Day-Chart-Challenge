library(tidyverse)
library(showtext)

# Data from yelexa via kaggle: https://www.kaggle.com/datasets/yelexa/spotify200?resource=download

font_add_google("Source Sans Pro")
showtext_auto()

music <- read_csv(here::here("2023/data/13_popculture_music.csv")) %>%
  filter(region == "North America") %>%
  select(c(rank, artist_names, artist_genre, track_name, peak_rank, weeks_on_chart, streams)) %>%
  filter(peak_rank <= 5) %>% 
  mutate(artist_genre = case_when(str_detect(artist_genre, "hip hop")~"Hip Hop",
                                  str_detect(artist_genre, "pop")~"Pop",
                                  str_detect(artist_genre, "rap")~"Rap",
                                  str_detect(artist_genre, "r&b")~"R&B",
                                  str_detect(artist_genre, "mexicana")~"Latin",
                                  str_detect(artist_genre, "latin")~"Latin",
                                  str_detect(artist_genre, "reggaeton")~"Latin",
                                  str_detect(artist_genre, "ranchera")~"Latin",
                                  str_detect(artist_genre, "drill")~"Rap",
                                  .default = artist_genre)) %>%
  mutate(track_name = sub("\\(.*", "", track_name)) %>% 
  filter(artist_genre %in% c("Hip Hop", "Pop", "Rap", "R&B", "Latin")) %>%
  distinct(track_name, .keep_all = TRUE)
  

plot_13 <- music %>%
  ggplot(aes(y = artist_genre, x = weeks_on_chart, color = artist_genre, fill = artist_genre)) +
  ggdist::stat_dist_eye(alpha = 0.2) +
  geom_point(position = position_jitter()) +
  geom_point(position = position_jitter()) +
  ggrepel::geom_text_repel(data = music %>% group_by(artist_genre) %>% slice_max(order_by = weeks_on_chart, n = 5),
                           aes(label = paste0(track_name, "\n", artist_names))) +
  labs(x = "# of Weeks on Chart",
       y = "",
       title = "Top North American Tracks via Spotify",
       subtitle = "From 2021 through 2022, there were over 295 top 5 North American tracks on Spotify.",
       caption = "Source: yelexa via Kaggle | #30DayChart Challenge | Day 13 | Pop Culture") +
  theme_void() +
  theme(text = element_text(family = "Source Sans Pro", color = "#F9F9F9"),
        plot.background = element_rect(colour = "black", fill = "black"),
        panel.backgroun = element_rect(colour = "black", fill = "black"),
        legend.position = "top")

