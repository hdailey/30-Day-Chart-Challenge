library(tidyverse)
library(showtext)
library(packcircles)
library(MetBrewer)

font_add_google("Archivo")
showtext_auto()

dogData <- tidytuesdayR::tt_load(2022, week = 5)
breedRank <- dogData$breed_rank 

breedRank <- breedRank %>%
  select(-c(links, Image)) %>%
  mutate(Breed = case_when(Breed == "Retrievers (Labrador)"~"Labrador Retrievers", .default = Breed),
         Breed = case_when(Breed == "Retrievers (Golden)"~"Golden Retrievers", .default = Breed),
         Breed = case_when(Breed == "Pointers (German Shorthaired)"~"German Shorthaired Pointers", .default = Breed),
         Breed = case_when(Breed == "German Shepherd Dogs"~"German Shepherds", .default = Breed)) %>%
  mutate(rankMean = rowMeans(across(where(is.numeric)))) %>%
  filter(!is.na(rankMean)) %>%
  slice_head(n = 10) %>%
  select(Breed, rankMean) %>%
  arrange(rankMean) %>%
  mutate(ID = 10:1,
         ID = as.factor(ID),
         rankID = row_number())

label <- character(length = nrow(breedRank))
for (i in 1:nrow(breedRank)){
  if(breedRank$Breed[i] %in% c("Labrador Retrievers", "German Shepherds", "Golden Retrievers", 
                                    "Bulldogs", "Beagles", "French Bulldogs", 
                                    "Poodles", "Rottweilers", "German Shorthaired Pointers", "Dachshunds")){
    label[i] = paste0(breedRank$Breed[i], "\n", breedRank$rankID[i])
  }
}
# pack circles
packing <- circleProgressiveLayout(breedRank$ID, sizetype = "area")
data <- cbind(breedRank, packing)
breedRank_2 <- circleLayoutVertices(packing, npoints = 50)
breedRank_2$n <- rep(breedRank$ID, each = 51)

# plot
plot_11 <- ggplot() +
  geom_polygon(data = breedRank_2,
               mapping = aes(x, y, group = id, fill = n)) +
  geom_text(data = data,
            mapping = aes(x, y, size = 10*breedRank$ID, label = str_wrap(label, 6)),
            family = "Archivo", fontface = "bold", colour = "#F3F9F9", lineheight = 0.5) +
  scale_fill_met_d("Renoir") +
  coord_fixed() +
  labs(title = "Top 10 AKC Dog Breeds",
       subtitle = str_wrap("The American Kennel Club (AKC) is a registry of purebred dog pedigrees in the United States. The AKC also promotes and sanctions events for purebred dogs. As of 2022, the AKC recognizes 200 dog breeds. Shown below are the top 10 most popular dog breeds on average from 2013 through 2020."),
       caption = "Source: AKC via KKakey | #30DayChartChallenge | Day 11") +
  theme_void() +
  theme(text = element_text(size = 20, family = "Archivo"),
        legend.position = "none",
        plot.background = element_rect(fill = "#F9F9F9", colour = "#F9F9F9"),
        panel.background = element_rect(fill = "#F9F9F9", colour = "#F9F9F9"),
        plot.title = element_text(size = 32, colour = "#003051"),
        plot.subtitle = element_text(size = 22, colour = "#003060", lineheight = 0.5))

ggsave(plot = plot_11, path = here::here("2023/charts"),
       paste0("11", "_circular", ".png"), scale = 0.5, dpi = 300,
       height = 8, width = 11, unit = "in")

