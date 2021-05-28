library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)
library(forcats)
library(ggalt)
library(extrafont)

#to get windows fonts
font_import()
loadfonts(device = "win")

#load data
tt_data <- tt_load(2021, week=22)

records <- tt_data$records

drivers <- tt_data$drivers

#initial look
glimpse(records)
glimpse(drivers)

#let's work with records data
records$track %>% unique() %>% sort()

rec <- records %>% filter(type == "Three Lap",
                    system_played == "PAL") %>%
  group_by(track, shortcut) %>% 
  summarise(time = min(time)) %>% 
  dplyr::ungroup() %>%
  tidyr::pivot_wider(names_from = shortcut, values_from = time) %>%
  dplyr::filter(!is.na(Yes)) %>%
  rowwise() %>% 
  dplyr::mutate(avg = mean(c(Yes, No)),
                diff = Yes - No)

#making a dumbbell plot to show the difference in time
p2 <- rec %>%
  ggplot(aes(x = Yes, xend = No, y = fct_reorder(track, diff))) +
  geom_segment(aes(y=track, yend=track, x=0, xend=375), color="#b2b2b2", size=0.15, alpha = .3) +
  geom_dumbbell(alpha = .7, size=3, color = "#FFFFFF", colour_x = "#CC6677", colour_xend = "#117733") +
  geom_rect(data=rec, aes(xmin=358, xmax=392, ymin=-Inf, ymax=Inf), fill="#e5e5e5", color = "#232229") +  
  geom_text(aes(label=diff, y=track, x=375), fontface="bold", size=4, color = "#232229", family="Segoe UI Light") + 
  geom_text(data=filter(rec, track=="Kalimari Desert"), aes(x=375, y=track, label="DIFF"),
                     size=5, vjust=-37.2, fontface="bold", family="Segoe UI Light", color = "#232229") +
  geom_text(data=filter(rec, track == "Yoshi Valley"),
            aes(x = Yes, y  = track, label = "Shortcut"),
            color = "#CC6677", size = 3, vjust = -2, fontface = "bold", family = "Segoe UI Light") +
  geom_text(data=filter(rec, track == "Yoshi Valley"),
            aes(x = No, y  = track, label = "No Shortcut"),
            color = "#117733", size = 3, vjust = -2, fontface = "bold", family = "Segoe UI Light") +
  labs(x="Time(Seconds)") +
  theme(panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank(),
        axis.ticks.y      = element_blank(),
        axis.title.y      = element_blank(),
        axis.title.x      = element_text(color = "#232229", size = 12, family = "Segoe UI Light", face = "bold"),
        axis.text.x       = element_text(color = "#232229", size = 12, family = "Segoe UI Light", face = "bold"),
        axis.text.y       = element_text(color = "#232229", size = 12, family = "Segoe UI Light", face = "bold"),
  ) +
  plot_annotation(
    title = "How Much Difference Does a Shortcut Make?",
    subtitle = "Mariokart 64 Records | PAL System | 3-lap races",
    caption = "#TidyTuesday Week22 | Data: mkwrs.com | Graphic: M. Jaffee | *those tracks with no shortcuts removed"
  ) &
  theme(panel.background  = element_rect(fill = "#e5e5e5", colour="#e5e5e5"),
        plot.background   = element_rect(fill = "#e5e5e5", colour="#e5e5e5"),
        strip.background  = element_rect(fill = "#e5e5e5", colour="#e5e5e5"),
        plot.title = element_text(size=20, face="bold", hjust = 0, color = "#232229", family="Segoe UI Light"),
        plot.subtitle = element_text(size=15, hjust = 0, face="italic", color = "#232229", family="Segoe UI Light"),
        plot.caption = element_text(size=10, face="bold", hjust = 0, color = "#232229", family="Segoe UI Light")) 
p2

ggsave("W22.png", last_plot(), device = "png")
