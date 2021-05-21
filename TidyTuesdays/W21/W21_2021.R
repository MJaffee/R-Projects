#setup
library(tidyr)
library(tidytuesdayR)
library(skimr)
library(ggplot2)
library(extrafont)
library(patchwork)
library(ggridges)
'%ni%' <- Negate('%in%') #thanks to deisygysi for this function

#load & organize data
tt_data <- tt_load(2021, week=21)

survey <- tt_data$survey

#clean for easier use
clean <- survey %>%
  mutate(country = recode(country, 
                          "USA" = "US",
                          "US" = "US",
                          "Us" = "US",
                          "U.S." = "US",
                          "Usa" = "US",
                          "usa" = "US",
                          "United states" = "US",
                          "united states" = "US",
                          'United States' = 'US',
                          "United States of America" = "US",
                          "England" = "UK",
                          "UK" = "UK",
                          "Uk" = "UK"),
         highest_level_of_education_completed = recode(highest_level_of_education_completed,
                                                       "Master's degree" = "MA degree",
                                                       "NA" = "Other"),
         gender = ifelse(gender %ni% c("Woman", "Man", "Non-binary"), "Prefer Not to Answer/Other", gender))
                                                       
  )

#let's look just at education & salary in the US
##remove NA entries
US <- clean %>%
  filter(country == 'US') %>%
  drop_na(highest_level_of_education_completed)

#create color palette
colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#CC79A7")

#initial plot for exploration
a <- US %>%
  ggplot(aes(x=annual_salary, y=highest_level_of_education_completed, fill = highest_level_of_education_completed, color = highest_level_of_education_completed)) +
  geom_density_ridges(alpha = 0.4, scale = 0.9, quantile_lines = TRUE, show.legend = FALSE, jittered_points = TRUE, position = "raincloud") +
  coord_cartesian(xlim = c(0, 350000)) + 
  scale_x_continuous(labels = scales::comma) +
  scale_fill_manual(values = colors) + 
  scale_color_manual(values = colors) +
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
    axis.text.x       = element_text(color = "#e9e0cc", size = 10, family = "mono", face = "bold"),
    axis.text.y       = element_text(color = "#e9e0cc", size = 10, family = "mono", face = "bold"),
    axis.ticks.x      = element_blank(),
    axis.ticks.y      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
  )

a

##Interesting to see the amount of respondents for each level of educataion
##Clear that those with higher level degrees have a higher salary


#Let's plot with gender responses

b <- US %>%
  ggplot(aes(x=annual_salary, y=gender, fill = gender, color = gender)) +
  geom_density_ridges(alpha = 0.4, scale = 0.9, quantile_lines = TRUE, show.legend = FALSE, jittered_points = TRUE, position = "raincloud") +
  coord_cartesian(xlim = c(0, 350000)) + 
  scale_x_continuous(labels = scales::comma) +
  scale_fill_manual(values = colors) + 
  scale_color_manual(values = colors) +
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
    axis.text.x       = element_text(color = "#e9e0cc", size = 10, family = "mono", face = "bold"),
    axis.text.y       = element_text(color = "#e9e0cc", size = 10, family = "mono", face = "bold"),
    axis.ticks.x      = element_blank(),
    axis.ticks.y      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
  )

b

##What is the breakdown of each gender and their education level?

#Filter for each gender response

women <- filter(US, gender == "Woman")

men <- filter(US, gender == "Man")

nonbinary <- filter(US, gender == "Non-binary")

Other <- filter(US, gender == "Prefer Not to Answer/Other")

#And plot each

p1 <- women %>%
  ggplot(aes(x=annual_salary, y = highest_level_of_education_completed, fill = highest_level_of_education_completed, color = highest_level_of_education_completed)) +
  geom_density_ridges(alpha = 0.4, scale = 0.9, quantile_lines = TRUE, show.legend = FALSE, jittered_points = TRUE, position = "raincloud") +
  coord_cartesian(xlim = c(0, 300000)) + 
  scale_x_continuous(labels = scales::comma) +
  scale_fill_manual(values = colors) + 
  scale_color_manual(values = colors) +
  labs(subtitle = "Woman") +
  theme(
    plot.margin       = unit(c(0.6, 0.5, 0.5, 0.5), "cm"),
    panel.background  = element_rect(fill = "#232229"),
    plot.background   = element_rect(fill = "#232229"),
    strip.background  = element_rect(fill = "#232229"),
    plot.title        = element_text(color = "#e9e0cc", size = 25, hjust = 1, family = "mono", face = "bold"),
    plot.subtitle     = element_text(color = "#e9e0cc", size = 15, hjust = 1, family = "mono", face = "bold"),
    plot.caption      = element_text(color = "#e9e0cc", size = 10, hjust = 0, family = "mono", face = "bold"),
    axis.title.x      = element_blank(),
    axis.title.y      = element_blank(),
    axis.text.x       = element_text(color = "#e9e0cc", size = 10, family = "mono", face = "bold"),
    axis.text.y       = element_text(color = "#e9e0cc", size = 10, family = "mono", face = "bold"),
    axis.ticks.x      = element_line(color = "#e9e0cc"),
    axis.ticks.y      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
  )


p2 <- men %>%
  ggplot(aes(x=annual_salary, y = highest_level_of_education_completed, fill = highest_level_of_education_completed, color = highest_level_of_education_completed)) +
  geom_density_ridges(alpha = 0.4, scale = 0.9, quantile_lines = TRUE, show.legend = FALSE, jittered_points = TRUE, position = "raincloud") +
  coord_cartesian(xlim = c(0, 300000)) + 
  scale_x_continuous(labels = scales::comma) +
  labs(subtitle = "Man") +
  scale_fill_manual(values = colors) + 
  scale_color_manual(values = colors) +
  theme(
    plot.margin       = unit(c(0.6, 0.5, 0.5, 0.5), "cm"),
    panel.background  = element_rect(fill = "#232229"),
    plot.background   = element_rect(fill = "#232229"),
    strip.background  = element_rect(fill = "#232229"),
    plot.title        = element_text(color = "#e9e0cc", size = 25, hjust = 1, family = "mono", face = "bold"),
    plot.subtitle     = element_text(color = "#e9e0cc", size = 15, hjust = 1, family = "mono", face = "bold"),
    plot.caption      = element_text(color = "#e9e0cc", size = 10, hjust = 0, family = "mono", face = "bold"),
    axis.title.x      = element_blank(),
    axis.title.y      = element_blank(),
    axis.text.x       = element_text(color = "#e9e0cc", size = 10, family = "mono", face = "bold"),
    axis.text.y       = element_text(color = "#e9e0cc", size = 10, family = "mono", face = "bold"),
    axis.ticks.x      = element_line(color = "#e9e0cc"),
    axis.ticks.y      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
  )

p3 <- nonbinary %>%
  ggplot(aes(x=annual_salary, y = highest_level_of_education_completed, fill = highest_level_of_education_completed, color = highest_level_of_education_completed)) +
  geom_density_ridges(alpha = 0.4, scale = 0.9, quantile_lines = TRUE, show.legend = FALSE, jittered_points = TRUE, position = "raincloud") +
  coord_cartesian(xlim = c(0, 300000)) + 
  scale_x_continuous(labels = scales::comma) +
  labs(subtitle = "Non-Binary") +
  scale_fill_manual(values = colors) + 
  scale_color_manual(values = colors) +
  theme(
    plot.margin       = unit(c(0.6, 0.5, 0.5, 0.5), "cm"),
    panel.background  = element_rect(fill = "#232229"),
    plot.background   = element_rect(fill = "#232229"),
    strip.background  = element_rect(fill = "#232229"),
    plot.title        = element_text(color = "#e9e0cc", size = 25, hjust = 1, family = "mono", face = "bold"),
    plot.subtitle     = element_text(color = "#e9e0cc", size = 15, hjust = 1, family = "mono", face = "bold"),
    plot.caption      = element_text(color = "#e9e0cc", size = 10, hjust = 0, family = "mono", face = "bold"),
    axis.title.x      = element_blank(),
    axis.title.y      = element_blank(),
    axis.text.x       = element_text(color = "#e9e0cc", size = 10, family = "mono", face = "bold"),
    axis.text.y       = element_text(color = "#e9e0cc", size = 10, family = "mono", face = "bold"),
    axis.ticks.x      = element_line(color = "#e9e0cc"),
    axis.ticks.y      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
  )

p4 <- Other %>%
  ggplot(aes(x=annual_salary, y = highest_level_of_education_completed, fill = highest_level_of_education_completed, color = highest_level_of_education_completed)) +
  geom_density_ridges(alpha = 0.4, scale = 0.9, quantile_lines = TRUE, show.legend = FALSE, jittered_points = TRUE, position = "raincloud") +
  coord_cartesian(xlim = c(0, 300000)) + 
  scale_x_continuous(labels = scales::comma) +
  labs(subtitle = "Other/Prefer Not to Answer") +
  scale_fill_manual(values = colors) + 
  scale_color_manual(values = colors) +
  theme(
    plot.margin       = unit(c(0.6, 0.5, 0.5, 0.5), "cm"),
    panel.background  = element_rect(fill = "#232229"),
    plot.background   = element_rect(fill = "#232229"),
    strip.background  = element_rect(fill = "#232229"),
    plot.title        = element_text(color = "#e9e0cc", size = 25, hjust = 0, family = "mono", face = "bold"),
    plot.subtitle     = element_text(color = "#e9e0cc", size = 15, hjust = 0, family = "mono", face = "bold"),
    plot.caption      = element_text(color = "#e9e0cc", size = 10, hjust = 0, family = "mono", face = "bold"),
    axis.title.x      = element_blank(),
    axis.title.y      = element_blank(),
    axis.text.x       = element_text(color = "#e9e0cc", size = 10, family = "mono", face = "bold"),
    axis.text.y       = element_text(color = "#e9e0cc", size = 10, family = "mono", face = "bold"),
    axis.ticks.x      = element_line(color = "#e9e0cc"),
    axis.ticks.y      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
  )


#Tie them all together
p <- p1 + p2 + p3 + p4 + plot_layout(ncol = 2) +
  plot_annotation(
    title = "Education & Salary of US Ask a Manager Respondents",
    subtitle = "By Respondent Gender",
    caption = "#TidyTuesday Week21 | Data: Ask a Manager | Graphic: M. Jaffee"
  ) &
  theme(panel.background = element_rect(fill = "#232229", colour="#232229"),
        plot.background = element_rect(fill = "#232229", colour="#232229"),
        plot.title = element_text(colour = "#e9e0cc", size=20, face="bold", hjust = 0, family="mono"),
        plot.subtitle = element_text(colour = "#e9e0cc", size=15, hjust = 0, family="mono"),
        plot.caption = element_text(colour = "#e9e0cc", size=10, face="bold", hjust = 0, family="mono")
  )

p

ggsave("W21.png", last_plot(), device = "png")
