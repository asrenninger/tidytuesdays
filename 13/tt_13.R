library(tidyverse)

##

hotels <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv")

##

library(rnaturalearth)
library(sf)

##

countries  <- 
  ne_countries(scale = 50, returnclass = 'sf') %>%
  rename(country = adm0_a3,
         name = admin,
         region = subregion) %>%
  select(country, name, region, continent) %>%
  st_transform(54030)

##

geotels <-
  hotels %>% 
  mutate(country = if_else(country == "CN", "CAN", country)) %>%
  left_join(countries) %>%
  st_as_sf()

## 

library(janitor)

##

geotels <-
  hotels %>% 
  group_by(country, is_canceled) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  spread(key = is_canceled, value = n) %>%
  clean_names() %>%
  rename(kept = x0,
         broken = x1) %>%
  drop_na() %>%
  mutate(rate = broken / (kept + broken)) %>%
  mutate(country = if_else(country == "CN", "CAN", country)) %>%
  left_join(countries) %>%
  st_as_sf()

##

theme_hor <- function () {
  theme_minimal() +
    theme(plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_line(size = 0.5, colour = 'grey50'),
          axis.text.x = element_text(face = 'bold', vjust = 0),
          axis.text.y = element_text(face = 'bold', angle = 270),
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'black', size = 15),
          strip.text = element_text(face = 'bold', colour = 'black'),
          plot.margin = margin(10, 10, 10, 10)
    )
}

##

rankings <- 
  hotels %>%
  group_by(country, is_canceled) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(country, n), y = n, fill = factor(is_canceled))) + 
  geom_bar(position = 'stack', stat = 'identity', colour = '#ffffff', size = 0.1) +
  geom_curve(aes(x = "ISR", y = 48000, xend = "FRA", yend = 40000), 
             size = 0.5, 
             curvature = 0.3,
             arrow = arrow(length = unit(0.01, "npc"))) +
  geom_text(aes("ISR", 50000, label = "portugal")) +
  geom_curve(aes(x = "MOZ", y = 8000, xend = "USA", yend = 2500), 
             size = 0.5, 
             curvature = -0.25,
             arrow = arrow(length = unit(0.01, "npc"))) +
  geom_text(aes("NGA", 8000, label = "united states")) +
  scale_fill_manual(values = c(pal[2], pal[8]),
                    guide = 'none') +
  scale_y_continuous(position = "right", 
                     breaks = c(0, 10000, 20000, 30000, 40000), 
                     labels = c("0", "10", "20", "30", "40")) +
  scale_x_discrete(breaks = c("COL"), labels = c("total bookings by nation, thousands")) +
  ylab("") +
  xlab("") +
  theme_hor()

hotels %>%
  group_by(country, is_canceled) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

hotels %>%
  group_by(country, is_canceled) %>%
  summarise(n = n()) %>%
  filter(country == "USA")

hotels %>%
  group_by(country, is_canceled) %>%
  summarise(n = n()) %>%
  arrange(n) %>%
  pull(country)

##

theme_map <- function () {
  theme_void() + 
    theme(plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major = element_line(size = NA), 
          panel.grid.minor = element_line(size = NA),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'black', size = 15),
          plot.caption = element_text(face = 'bold', colour = 'black'),
          strip.text = element_text(face = 'bold', colour = 'black'),
          plot.margin = margin(5, 5, 5, 5),
          legend.position = 'top'
    )
  
}

guide_continuous <- 
  guide_colorbar(direction = "horizontal",
                 barwidth = unit(50, units = "mm"),
                 barheight = unit(1, units = "mm"),
                 draw.ulim = FALSE,
                 title.position = 'top',
                 label.position = 'bottom',
                 title.hjust = 0.5,
                 label.hjust = 0.5)

pal <- read_csv("https://github.com/asrenninger/palettes/raw/master/grered.txt", col_names = FALSE) %>% pull(X1)

## 

map <- 
  ggplot() +
  geom_sf(data = filter(countries, name != "Antarctica"), aes(), fill = NA, colour = '#b7b7b7', size = 0.25) +
  geom_sf(data = geotels, aes(fill = rate), colour = '#ffffff', size = 0.25) +
  scale_fill_gradientn(colours = rev(pal), guide = guide_continuous, name = "cancelation rate") +
  theme_map()

##

rankings +
  annotation_custom(grob = ggplotGrob(map), ymin = 2000, ymax = 55000) +
  ggsave("test.png", height = 8, width = 11, dpi = 300)


