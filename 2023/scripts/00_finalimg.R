library(tidyverse)
library(magick)

file.remove(here::here("2023/charts/00_finalimg.png"))

img_list <- list.files(path = here::here("2023/charts/"), full.names = TRUE)

img_montage <- image_read(img_list) %>% 
  image_montage(tile = '5x4', 
                geometry = "x1000+0+0", 
                shadow = FALSE, 
                bg = "grey95")

image_write(img_montage, format = "png", 
            path = here::here("2023/charts/00_finalimg.png"), 
            quality = 100)
