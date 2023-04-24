library(tidyverse)
library(wordcloud2)
library(tm)
library(showtext)
library(htmlwidgets)


# Data is from Tidy Tuesday, 2021 Week 31: https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-07-27/
olyData <- tidytuesdayR::tt_load(2021, week = 31)

font_add_google("Cabin")
font_add_google("Cabin Condensed")
showtext_auto()

olympics <- olyData$olympics %>%
  filter(noc == "USA")

sports <- olympics$sport
olympicsCloud <- Corpus(VectorSource(sports))

# Document Term Matrix
sportsDTM <- TermDocumentMatrix(olympicsCloud) %>%
  as.matrix()
olympicsWords <- sort(rowSums(sportsDTM), decreasing = TRUE)
olympicsSportCloud <- data.frame(word = names(olympicsWords), freq = olympicsWords)

# plot
set.seed(04142023)
plot_14 <- olympicsSportCloud %>%
  wordcloud2(size = 2, color = "random-light", backgroundColor = "grey25", 
             fontFamily = "Cabin Condensed", shape = "circle")
saveWidget(plot_14,"14_newtool.html",selfcontained = F)
webshot::webshot("14_newtool.html","14_newtool.png", vwidth = 2550, vheight = 3330, delay =10)


