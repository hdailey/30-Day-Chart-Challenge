library(tidyverse)
library(MetBrewer)
library(showtext)

font_add_google("Cabin")
font_add_google("Cabin Condensed")
showtext_auto()

# Data From eurostat: https://ec.europa.eu/eurostat/databrowser/

waterProcess <- tibble(Sources = c("ABST", "REW", "IMP", "EXP", "LOSS", "LOSS_EVAP", "LOSS_LEAK", 
                                   "AVL_USE", "WAU", "WAFU", "PWS_SOWS", "CONS_USE", "CONSUM", "CWD", "DIS"),
                       Descript = c("Total Gross Abstraction",
                                    "Water Returned Without Use", "Imports of Water", "Exports of Water", "Losses During Transport",
                                    "Evaporation Losses", "Leakage Losses", "Water Available for Use",
                                    "Water Made Available for Use", "Water Available for Final Use",
                                    "Public Water Supply",
                                    "Consumptive Use", "Total Water Consumption",
                                    "Total Cooling Water Discharged",
                                    "Total Discharges of Wastewater Treatment Plants"))

colors <- met.brewer("Signac", n = 14, type = "continuous", direction = 1)

watBal <- read_csv(here::here("2023/data/18_eurostat.csv")) %>%
  select(-c(DATAFLOW, `LAST UPDATE`)) %>% 
  rename(Year = TIME_PERIOD) %>%
  filter(!is.na(OBS_VALUE)) %>%
  filter(wat_proc != "TOTAL") %>%
  mutate(wat_proc = case_when(wat_proc == "ABS_NET"~"ABST",
                              wat_proc == "LOSS_USE"~"LOSS",
                              wat_proc == "CWD_IW"~"CWD",
                              wat_proc == "CWD_SEA"~"CWD",
                              wat_proc == "DIS_SEA"~"DIS",
                              wat_proc == "DIS_IW"~"DIS",
                              wat_proc == "LOSS_LEAK"~"LOSS",
                              .default = wat_proc)) %>%
  left_join(waterProcess, by = c("wat_proc" = "Sources")) %>%
  filter(Year %in% c(1970, 1980, 1990, 2000, 2010, 2020)) %>%
  group_by(Year, wat_proc, Descript) %>%
  reframe(Year, sumProc = sum(OBS_VALUE)) %>%
  distinct() %>%
  ungroup() %>%
  group_by(Year) %>%
  reframe(Year, wat_proc, Descript, sumProc, sumYear = sum(sumProc)) %>%
  mutate(percProc = (sumProc/sumYear) * 100) %>%
  mutate(sumYear_MG = (sumYear*264)/(10^6))


plot_18 <- watBal %>%
  ggplot() +
  geom_bar(mapping = aes(x = Year,
                         y = as.numeric(percProc), fill = Descript), stat = "identity") +
  scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010, 2020)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = "",
       y = "",
       title = "EU Water Use Balance",
       subtitle = "Below is the EU Water Balance for each decade from 1970 through 2020.",
       caption = "Source: EuroStat | #30DayChartChallenge | Day 18 | Data Day: EuroStat") +
  coord_flip() +
  scale_fill_manual(values = colors) + 
  cowplot::theme_minimal_grid() +
  guides(fill = guide_legend(nrow = 2, title = "", label.position = "bottom", keywidth = 3, label.vjust = 5, byrow = FALSE)) +
  theme(text = element_text(family = "Cabin Condensed", size = 32),
        plot.background = element_rect(fill = "grey95", colour = "grey95"),
        panel.background = element_rect(fill = "grey95", colour = "grey95"),
        plot.title = element_text(family = "Cabin", face = "bold", size = 64),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 52),
        axis.text = element_text(size = 32),
        axis.text.x = element_text(hjust = 1),
        legend.position = "bottom",
        legend.justification = "center",
        legend.box.margin = margin(r = 50),
        legend.text = element_text(size = 24),
        legend.spacing.x = unit(0.5, "cm"))

ggsave(plot = plot_18, path = here::here("2023/charts/"), "18_eurostat.png", dpi = 320, height = 8, width = 11, unit = "in")
