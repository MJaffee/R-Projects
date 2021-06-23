library(tidytuesdayR)
library(tidyverse)
library(patchwork)
library(ggbump)

#load data ----

tt_data <- tt_load(2021, week=26)
data <- tt_data$parks

#organize data & parse to convert percentages + dollars(script from @kierisi) ----

parks <- data %>%
  mutate(park_pct_city_data = parse_number(park_pct_city_data),
         pct_near_park_data = parse_number(pct_near_park_data),
         spend_per_resident_data = parse_number(spend_per_resident_data)) %>%
  mutate(across(where(is.character), factor)) %>%
  select(-city_dup)

glimpse(parks)

#create vector of Top 10 most populous cities ----

top_pop <- c("New York", "Los Angeles", "Chicago", "Houston",
             "Phoenix", "Philadelphia", "San Antonio", "San Diego",
             "Dallas", "San Jose")
#create object of Top 10 most populous cities ----

top_pop_parks <- parks %>%
  filter(city %in% top_pop)

#plot ----

p1 <- top_pop_parks %>%
  ggplot(aes(year, pct_near_park_data, color = city, fill = city)) +
  geom_point(size = 4, show.legend = FALSE) +
  geom_bump(size = 1, show.legend = FALSE) +
  scale_color_manual(values = c('#332288', '#88CCEE', '#44AA99', 
                     '#117733', '#999933', '#DDCC77', 
                     '#CC6677', '#882255', '#AA4499', 
                     '#DDDDDD')) +
  geom_area(size = 1, alpha = 0.1, show.legend = FALSE, color = NA) + 
  scale_fill_manual(values = c('#332288', '#88CCEE', '#44AA99', 
                               '#117733', '#999933', '#DDCC77', 
                               '#CC6677', '#882255', '#AA4499', 
                               '#DDDDDD')) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(breaks = c(2014, 2018)) +
  facet_grid(~city) +
  theme(
    panel.background = element_rect(fill = "#232229", colour = "#232229"),
    plot.background = element_rect(fill = "#232229", colour = "#232229"),
    strip.background = element_rect(fill = "#232229", colour = "#232229"),
    panel.grid.minor = element_line(colour = "#e5e5e5",
                                    size = .10, linetype = "dotted"),
    panel.grid.major = element_line(colour = "#e5e5e5", 
                                    size = .10, linetype = "dotted"),
    axis.title.x = element_text(family = "Segoe UI Light", 
                                size = 15, colour="#e5e5e5"),
    axis.title.y = element_text(family = "Segoe UI Light", 
                                size = 15, colour="#e5e5e5"),
    axis.text.x = element_text(family = "Segoe UI Light", 
                               size = 12, colour="#e5e5e5"),
    axis.text.y = element_text(family = "Segoe UI Light",
                               size = 12, colour="#e5e5e5"),
    strip.text.x = element_text(face = "italic", family = "Segoe UI Light", 
                                size = 15, colour="#e5e5e5"),
    axis.ticks = element_blank(),
  ) +
  labs(
    x = "Year",
    y = "% of Residents"
  ) +
  plot_annotation(
    title = "Percentage of Residents Within a 10 Minute Walk of a Park (2012-2020)",
    subtitle = "Top 10 Most Populous US Cities",
    caption = "#TidyTuesday Week26 | Data: The Trust for Public Land | Graphic: @marcjaffee_"
  ) &
  theme(
    panel.background  = element_rect(fill = "#232229", colour="#232229"),
    plot.background   = element_rect(fill = "#232229", colour="#232229"),
    strip.background  = element_rect(fill = "#232229", colour="#232229"),
        plot.title = element_text(size=20, face="bold", hjust = 0,
                                  color = "#e5e5e5", family="Segoe UI Light"),
        plot.subtitle = element_text(size=15, hjust = 0, face="italic",
                                     color = "#e5e5e5", family="Segoe UI Light"),
        plot.caption = element_text(size=10, face="italic", hjust = 0, 
                                    color = "#e5e5e5", family="Segoe UI Light"))

p1

#save plot ----

ggsave("W26.png", last_plot(), device = "png")
