library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(ggalt)
library(extrafont)
library(survivoR)
library(ggtext)
library(extrafont)


# load data ----
  
idols <- hidden_idols

# clean + organize ----

dumbbell <- idols %>%
  dplyr::filter(!is.na(day_played)) %>%
  dplyr::filter(!is.na(season_name)) %>%
  dplyr::filter(legacy_advantage == "FALSE") %>%
  dplyr::filter(season == "40")

point <- idols %>%
  dplyr:: group_by(season) %>%
  dplyr:: select(season, votes_nullified) %>%
  summarize(count = n())

# create charts ----

p1 <- dumbbell %>%
  ggplot(aes(x = day_found, xend = day_played, y = castaway)) +
  geom_dumbbell(alpha = 0.9, size=3, color = "#e2daa9", colour_x = "#e73f10", 
                colour_xend = "#203c5f") +
  geom_curve(aes(x = 38, y = 5.2, xend = 37, yend = "Natalie Anderson"),
             angle = 0, arrow = arrow(30, unit(0.1, "inches"))) +
  geom_textbox(aes(x = 38.7, y = 5.6), size = 3, 
               fill = NA, box.colour = NA,
               family = "Segoe UI Light", 
               label = ("Anderson utilized a purchased immunity
               idol to propel herself to the final 3")) +
  geom_curve(aes(x = 17, y = "Denise Stapley", xend = 16, 
                 yend = "Denise Stapley"),
             angle = 90, arrow = arrow(30, unit(0.1, "inches")))+
  geom_textbox(aes(x = 18.2, y = 1.6), size = 3,
               fill = NA, box.colour = NA,
               family = "Segoe UI Light", 
               label = ("Stapley used both a found idol & an idol 
               Sandra had given to her
               in order to ensure Sandra was sent home")) +
  geom_text(data=filter(dumbbell, castaway == "Tony Vlachos"),
            aes(x = day_found, y  = castaway, label = "Found"),
            color = "#e73f10", size = 3, vjust = -2, fontface = "bold",
            family = "Segoe UI Light") +
  geom_text(data=filter(dumbbell, castaway == "Tony Vlachos"),
            aes(x = day_played, y  = castaway, label = "Played"),
            color = "#203c5f", size = 3, vjust = -2, fontface = "bold", 
            family = "Segoe UI Light") +
  labs(subtitle = "Use of Hidden Immunity Idol (Season 40)", 
       x = "Day", y = "Castaway") +
  theme(
  panel.background = element_rect(fill = "#fce7d6", colour = "#fce7d6"),
  plot.background = element_rect(fill = "#fce7d6", colour = "#fce7d6"),
  strip.background = element_rect(fill = "#fce7d6", colour = "#fce7d6"),
  panel.grid.major = element_line(colour = "#e2daa9"),
  panel.grid.minor = element_line(colour = "#e2daa9"),
  axis.ticks.y = element_blank(),
  axis.ticks.x = element_blank(),
  axis.title.y = element_blank(),
  axis.title.x = element_text(family = "Segoe UI Light", size = 10),
  axis.text.x = element_text(family = "Segoe UI Light", size = 10),
  axis.text.y = element_text(family = "Segoe UI Light", size = 10),
  )
p1

p2 <- point %>%
  ggplot(aes(x = season, y = count)) +
  geom_point(color = "#203c5f", size = 3) +
  labs(subtitle = "Votes Nullified Since Season 11",
       x = "Season",
       y = "Total Votes Nullified") +
  theme(
    panel.background = element_rect(fill = "#fce7d6", colour = "#fce7d6"),
    plot.background = element_rect(fill = "#fce7d6", colour = "#fce7d6"),
    strip.background = element_rect(fill = "#fce7d6", colour = "#fce7d6"),
    panel.grid.major = element_line(colour = "#e2daa9"),
    panel.grid.minor = element_line(colour = "#e2daa9"),
    axis.title.x = element_text(family = "Segoe UI Light", size = 10),
    axis.title.y = element_text(family = "Segoe UI Light", size = 10),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
  )
p2

# sew them together ----

p <- p1 + p2 + plot_layout(ncol = 1) +
  plot_annotation(
    title = "The Hidden Immunity Idol",
    subtitle = "When do castaways find & use them? How many votes have they nullified?",
    caption = "#TidyTuesday Week23 | Data: survivoR package | Graphic: M. Jaffee"
  ) &
  theme(
    plot.background = element_rect(fill = "#fce7d6", 
                                   colour = "#fce7d6"),
    plot.title = element_text(size = 20, face = "bold", 
                              family = "Segoe UI Light"),
    plot.subtitle = element_text(size = 15, face = "italic",
                                 family = "Segoe UI Light"),
    plot.caption = element_text(size = 10, face = "bold", 
                                family = "Segoe UI Light"),
  )

p

#save image ----

ggsave("W23.png", last_plot(), device = "png")
