#setup
library(tidyverse)
library(tidytuesdayR)
library(skimr)
library(tigris)
library(rnaturalearth)
library(ggplot2)
library(janitor)
library(extrafont)
library(patchwork)

#load & organize data
tt_data <- tt_load(2021, week=20)

broadband <- tt_data$broadband
broadbandzip <- tt_data$broadband_zip

#take a look
skim(broadband)
skim(broadbandzip)

#let's work with broadband
#clean and organize data (thanks to M Henderson for some help on this)
broadband2 <- tt_data$broadband %>%
  clean_names() %>%
                rename(Availability = broadband_availability_per_fcc,
                       Usage = broadband_usage
                       ) %>%
                mutate(
                  Usage = as.numeric(Usage),
                  Availability = as.numeric(Availability)
                )

#filter to state
PA_data <- broadband2 %>%
  filter(st == 'PA')

#load & filter state counties & clean
PA_counties <- counties(state = 'PA') %>%
  clean_names()

#join sets
PA_bdnd <- PA_counties %>%
  left_join(PA_data, by = c("namelsad" = "county_name"))

#plot Usage
g <- ggplot() +
  geom_sf(color = "#232229", data = PA_bdnd, aes(fill = Usage), size = 0.25) +
  scale_fill_gradient("% Using", labels = scales::percent, low = "#440154FF", high = "#3CBB75FF") +
  theme(
    plot.margin       = unit(c(0.6, 0.5, 0.5, 0.5), "cm"),
    panel.background  = element_rect(fill = "#232229"),
    plot.background   = element_rect(fill = "#232229"),
    legend.background = element_blank(),
    strip.background  = element_rect(fill = "#232229"),
    plot.title        = element_text(color = "#e9e0cc", size = 25, hjust = 1, family = "mono", face = "bold"),
    plot.subtitle     = element_text(color = "#e9e0cc", size = 15, hjust = 1, family = "mono", face = "bold"),
    plot.caption      = element_text(color = "#e9e0cc", size = 10, hjust = 0, family = "mono", face = "bold"),
    legend.title      = element_text(color = "#e9e0cc", size = 10, hjust = 0, family = "mono", face = "bold"),
    legend.text       = element_text(color = "#e9e0cc", family = "mono"),
    legend.justification = "left",
    legend.position   = "bottom",
    legend.key.width  = unit(1, "cm"),
    axis.title.x      = element_blank(),
    axis.title.y      = element_blank(),
    axis.text.x       = element_blank(),
    axis.text.y       = element_blank(),
    axis.ticks.x      = element_blank(),
    axis.ticks.y      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
  )

#plot Availability
a <- ggplot() +
  geom_sf(color = "#232229", data = PA_bdnd, aes(fill = Availability), size = 0.25) +
  scale_fill_gradient("% Available", labels = scales::percent, low = "#440154FF", high = "#3CBB75FF") +
  theme(
    plot.margin       = unit(c(0.6, 0.5, 0.5, 0.5), "cm"),
    panel.background  = element_rect(fill = "#232229"),
    plot.background   = element_rect(fill = "#232229"),
    legend.background = element_blank(),
    strip.background  = element_rect(fill = "#232229"),
    plot.title        = element_text(color = "#e9e0cc", size = 25, hjust = 1, family = "mono", face = "bold"),
    plot.subtitle     = element_text(color = "#e9e0cc", size = 15, hjust = 1, family = "mono", face = "bold"),
    plot.caption      = element_text(color = "#e9e0cc", size = 10, hjust = 0, family = "mono", face = "bold"),
    legend.title      = element_text(color = "#e9e0cc", size = 10, hjust = 0, family = "mono", face = "bold"),
    legend.text       = element_text(color = "#e9e0cc", family = "mono", face = "bold"),
    legend.justification = "left",
    legend.position   = "bottom",
    legend.key.width  = unit(1, "cm"),
    axis.title.x      = element_blank(),
    axis.title.y      = element_blank(),
    axis.text.x       = element_blank(),
    axis.text.y       = element_blank(),
    axis.ticks.x      = element_blank(),
    axis.ticks.y      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
  )


#Patch plots
pa <- g + a + plot_layout(ncol = 2) +
  plot_annotation(
      title = "PENNSYLVANIA",
      subtitle = "Less than 40% using broadband in many counties despite being widly available",
      caption = "#TidyTuesday Week20 | Data: Microsoft & The Verge | Graphic: M. Jaffee"
    ) &
      theme(panel.background = element_rect(fill = "#232229", colour="#232229"),
            plot.background = element_rect(fill = "#232229", colour="#232229"),
            plot.title = element_text(colour = "#e9e0cc", size=20, face="bold", hjust = 0, family="mono"),
            plot.subtitle = element_text(colour = "#e9e0cc", size=15, hjust = 0, family="mono"),
            plot.caption = element_text(colour = "#e9e0cc", size=10, face="bold", hjust = 0, family="mono")
            )
    
pa
