library(tidyverse)
library(gganimate)
library(dplyr) 

spending <- read.csv("~/Desktop/spending.csv")
spending_formatted <- spending %>%
  group_by(year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-value),
         Value_rel = value/value[rank==1],
         Value_lbl = paste0(" ",round(value))) %>%
  group_by(category) %>% 
  filter(rank <=10) %>%
  ungroup()

anim <- ggplot(spending_formatted, aes(rank, group = category, 
                                  fill = as.factor(category), color = as.factor(category))) +
  geom_tile(aes(y = value/2,
                height = value,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, size = 10, label = paste(category, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=value,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( linewidth=.1, color="grey" ),
        panel.grid.minor.x = element_line( linewidth=.1, color="grey" ),
        plot.title=element_text(size=45, hjust=0.5, face="bold", colour="grey", vjust=3),
        plot.subtitle=element_text(size=20, hjust=0.5, face="italic", color="grey", vjust=3),
        plot.caption =element_text(size=20, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,4, 2, 6, "cm")) +
  transition_states(year, transition_length = 10, state_length = 1, wrap = FALSE) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Consumer Expenditures per Year : {closest_state}',  
       subtitle  =  "Top 8 Categories",
       caption  = "Aggregate in Millions USD | Data Source: U.S. Bureau of Labor Statistics") 

animate(anim, 200, fps = 10,  width = 1200, height = 1000, 
        renderer = gifski_renderer("gganim.gif"), end_pause = 15, start_pause =  15) 