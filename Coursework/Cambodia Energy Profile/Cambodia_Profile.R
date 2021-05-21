#setup
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(rgeos)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)

#load data & subset
mit <- read.csv(...)
cmb <- subset(mit, Country=="Cambodia")


#set color palette
colors <- c("#D55E00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#CC79A7", "#E69F00")

#plot energy mix
c <- ggplot(data=cmb, aes(x=Subtype, y=Capacity..MW., fill=Subtype, color=Subtype)) +
  geom_bar(stat="Identity", alpha = 1, show.legend = TRUE) +
  scale_fill_manual(values = colors) + 
  scale_color_manual(values = colors) +
  labs(title="Mix", x="Source", y="Capacity(MW)") +
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
    axis.title.x      = element_text(color = "#e9e0cc", size = 15, hjust = 0.5, family = "mono", face = "bold"),
    axis.title.y      = element_text(color = "#e9e0cc", size = 15, hjust = 0.5, family = "mono", face = "bold"),
    axis.text.x       = element_blank(),
    axis.text.y       = element_text(color = "#e9e0cc", size = 10, hjust = 0.5, family = "mono", face = "bold"),
    axis.ticks.x      = element_blank(),
    axis.ticks.y      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_line(color = "#e9e0cc"),
  )
c

#plot project location
world <- ne_countries(scale = "medium", returnclass = "sf")
d <- ggplot() +
  geom_sf(data = world, color = "#232229") +
  geom_point(data = cmb, aes(x = Longitude, y = Latitude, color = Subtype, show.legend=TRUE), size = 3,
             shape = 20) +
  scale_fill_manual(values = colors) + 
  scale_color_manual(values = colors) +
  coord_sf(xlim = c(102, 108), ylim = c(10, 16), expand = FALSE) +
  labs(title="Location") +
  theme( plot.margin       = unit(c(0.6, 0.5, 0.5, 0.5), "cm"),
         panel.background  = element_rect(fill = "#232229"),
         plot.background   = element_rect(fill = "#232229"),
         legend.background = element_rect(fill = "#232229"),
         strip.background  = element_rect(fill = "#232229"),
         plot.title        = element_text(color = "#e9e0cc", size = 15, hjust = 1, family = "mono", face = "bold"),
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
         axis.text.y       = element_text(color = "#e9e0cc", size = 10, hjust = 0.5, family = "mono", face = "bold"),
         axis.ticks.x      = element_blank(),
         axis.ticks.y      = element_blank(),
         panel.grid.major  = element_blank(),
         panel.grid.minor  = element_line(color = "#e9e0cc"),
  )
  
d

#plot project timeline
b <- ggplot(data=cmb, aes(x=Year.of.Completion, y=Capacity..MW., color=Subtype)) + 
  geom_point(size=3) +
  labs(title="Timeline", x="Year", y="Capacity(MW)") +
  scale_fill_manual(values = colors) + 
  scale_color_manual(values = colors) +
  theme( plot.margin       = unit(c(0.6, 0.5, 0.5, 0.5), "cm"),
         panel.background  = element_rect(fill = "#232229"),
         plot.background   = element_rect(fill = "#232229"),
         legend.background = element_rect(fill = "#232229"),
         strip.background  = element_rect(fill = "#232229"),
         plot.title        = element_text(color = "#e9e0cc", size = 15, hjust = 1, family = "mono", face = "bold"),
         plot.subtitle     = element_text(color = "#e9e0cc", size = 15, hjust = 1, family = "mono", face = "bold"),
         plot.caption      = element_text(color = "#e9e0cc", size = 10, hjust = 0, family = "mono", face = "bold"),
         legend.title      = element_text(color = "#e9e0cc", size = 10, hjust = 0, family = "mono", face = "bold"),
         legend.text       = element_text(color = "#e9e0cc", family = "mono", face = "bold"),
         legend.justification = "left",
         legend.position   = "bottom",
         legend.key.width  = unit(1, "cm"),
         axis.title.x      = element_text(color = "#e9e0cc", size = 15, hjust = 0.5, family = "mono", face = "bold"),
         axis.title.y      = element_text(color = "#e9e0cc", size = 15, hjust = 0.5, family = "mono", face = "bold"),
         axis.text.x       = element_text(color = "#e9e0cc", size = 10, family = "mono", face = "bold"),
         axis.text.y       = element_text(color = "#e9e0cc", size = 10, hjust = 0.5, family = "mono", face = "bold"),
         axis.ticks.x      = element_blank(),
         axis.ticks.y      = element_blank(),
         panel.grid.major  = element_line(color = "#e9e0cc"),
         panel.grid.minor  = element_line(color = "#e9e0cc"),
)

b         

#tie it up
cam <- b + c + d + plot_layout(ncol = 3) +
  plot_annotation(
    title = "CAMBODIA'S ENERGY PROFILE",,
    caption = "Data: Mekong Infrastrucutre Tracker | Graphic: M. Jaffee"
  ) &
  theme(panel.background = element_rect(fill = "#232229", colour="#232229"),
        legend.background = element_rect(fill = "#232229", colour="#232229"),
        plot.background = element_rect(fill = "#232229", colour="#232229"),
        plot.title = element_text(colour = "#e9e0cc", size=20, face="bold", hjust = 0, family="mono"),
        plot.subtitle = element_text(colour = "#e9e0cc", size=15, hjust = 0, family="mono"),
        plot.caption = element_text(colour = "#e9e0cc", size=10, face="bold", hjust = 0, family="mono")
  )

cam

ggsave("CMB.png", last_plot(), device = "png")
