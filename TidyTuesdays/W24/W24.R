library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(extrafont)
library(gghighlight)

# load data ----

tuesdata <- tidytuesdayR::tt_load(2021, week = 24)

# wrangle & clean (courtesy of @Kiersi!) ----

fishing <- tuesdata$fishing
stocked <- tuesdata$stocked

fish_tidy <- fishing %>%
  mutate(across(where(is.character), as.factor))

# plot ----

fish_tidy %>%
  select(lake, year, values) %>%
  drop_na() %>%
  group_by(year, lake) %>%
  summarise(total_fishies = sum(values)) %>%
  ggplot(aes(year, total_fishies, group = lake, col = lake)) +
  theme(
    panel.background = element_rect(fill = "#fce7d6", colour = "#fce7d6"),
    plot.background = element_rect(fill = "#fce7d6", colour = "#fce7d6"),
    strip.background = element_rect(fill = "#fce7d6", colour = "#fce7d6"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(family = "Segoe UI Light", size = 10),
    axis.title.y = element_text(family = "Segoe UI Light", size = 10),
    axis.text.x = element_text(family = "Segoe UI Light", size = 10),
    axis.text.y = element_text(family = "Segoe UI Light", size = 10),
  ) +
  geom_step(size = 1, alpha = 6, show.legend = FALSE) +
  gghighlight() +
  facet_wrap(~ lake) +
  scale_color_manual(values = c("#332288", "#117733", "#88CCEE",
                                "#DDCC77", "#CC6677", "#882255")) +
  labs(
    x = "Year",
    y = "Total Fishes"
  ) +
  plot_annotation(
    title = "Total Fish Observed in the Great Lakes",
    caption = "#TidyTuesday Week24 | Data: Great Lakes Fishery Comission | Graphic: M. Jaffee"
  ) &
  theme(panel.background  = element_rect(fill = "#e5e5e5", colour="#e5e5e5"),
        plot.background   = element_rect(fill = "#e5e5e5", colour="#e5e5e5"),
        strip.background  = element_rect(fill = "#e5e5e5", colour="#e5e5e5"),
        plot.title = element_text(size=20, face="bold", hjust = 0, color = "#232229", family="Segoe UI Light"),
        plot.subtitle = element_text(size=15, hjust = 0, face="italic", color = "#232229", family="Segoe UI Light"),
        plot.caption = element_text(size=10, face="bold", hjust = 0, color = "#232229", family="Segoe UI Light")) 

# image ----

ggsave("W24.png", last_plot(), device = "png")
