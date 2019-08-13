install.packages("tidyverse")

library(tidyverse)

##

wine_ratings <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

##

glimpse(wine_ratings)

##

wine_ratings %>%
  group_by(variety) %>%
  summarise(n = n()) %>%
  mutate(position = rank(-n, ties.method = "random")) %>%
  arrange(desc(n))

wine_ratings %>%
  group_by(variety) %>%
  summarise(n = n()) %>%
  mutate(position = rank(-n, ties.method = "random")) %>%
  ggplot(aes(reorder(position, n), n)) +
  geom_bar(aes(fill = n, colour = n), stat = 'identity', show.legend = FALSE) +
  geom_curve(aes(x = 500, y = 10000, xend = 700, yend = 13000), 
             colour = "#555555", 
             size = 0.5, 
             curvature = 0.5,
             arrow = arrow(length = unit(0.01, "npc"))) +
  geom_text(aes(500, 9000, label = "pinot noir")) +
  geom_curve(aes(x = 500, y = 10000, xend = 700, yend = 13000), 
             colour = "#555555", 
             size = 0.5, 
             curvature = 0.5,
             arrow = arrow(length = unit(0.01, "npc"))) +
  geom_text(aes(500, 9000, label = "pinot noir")) +
  geom_curve(aes(x = 230, y = 5000, xend = 100, yend = 3), 
             colour = "#555555", 
             size = 0.5, 
             curvature = -0.3,
             arrow = arrow(length = unit(0.01, "npc"))) +
  geom_text(aes(250, 5000, label = "never heard of this thing called ojaleshi")) +
  scale_fill_gradientn(colours = pal) +
  scale_colour_gradientn(colours = pal) +
  labs(title = "power laws", subtitle = "WINES BY VARIETAL", 
       x = "", y = "n") + 
  coord_flip() +
  theme_minimal() +
  theme(plot.background = element_rect(fill = '#bcbcbc', colour = '#bcbcbc'),
        panel.grid.major.x = element_line(size = 0.1, colour = 'grey50'),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.y = element_line(size = 0.5, colour = 'black'),
        axis.line.x = element_blank(),
        axis.ticks.y = element_line(size = 0.5, colour = 'black'),
        axis.ticks.x = element_line(size = 0.1, colour = 'grey50'),
        axis.text.x = element_text(face = 'bold'),
        axis.text.y = element_blank(),
        plot.title = element_text(face = 'bold', colour = 'grey50'),
        plot.subtitle =  element_text(face = 'plain', colour = 'black'),
        strip.text = element_text(face = 'bold', colour = 'black'),
        plot.margin = margin(20, 20, 20, 20)) +
  ggsave("test.png", height = 8, width = 8, dpi = 300)

##

wine_ratings %>%
  group_by(variety) %>%
  summarise(n = n()) %>%
  mutate(position = rank(-n, ties.method = "random")) %>%
  ggplot(aes(reorder(position, n), n)) +
  geom_bar(aes(fill = n), size = NA, colour = NA, stat = 'identity', show.legend = FALSE) +
  scale_fill_gradientn(colours = pal) +
  labs(title = "power laws", subtitle = "WINES BY VARIETAL", 
       x = "", y = "n") + 
  coord_flip() +
  theme_rot() +
  ggsave("test.png", height = 7, width = 7, dpi = 300)

##

library(googleway)

##

wine_ratings %>%
  mutate(year = str_extract(title, "(\\d)+")) %>%
  group_by(year) %>%
  summarise(price = mean(price)) %>%
  pull(year)

##

library(rnaturalearth)
library(sf)

provinces <- 
  ne_states() %>% 
  st_as_sf() %>%
  rename(code = adm0_a3,
         country = admin) %>%
  select(code, country)

countries  <- 
  ne_countries() %>% 
  st_as_sf() %>%
  rename(code = adm0_a3,
         country = admin,
         region = subregion) %>%
  select(code, name, region, continent) %>%
  st_drop_geometry()

provinces_clipped <- 
  provinces %>%
  left_join(countries) %>%
  st_as_sf() %>%
  filter(continent == "Europe")

ratings_clipped <- 
  wine_ratings %>%
  group_by(country) %>%
  summarise(price = mean(price),
            points = mean(points),
            n = n()) %>%
  filter(country %in% provinces_clipped$country)

library(geofacet)

eu_grid1

ratings_gridded <-
  ratings_clipped %>%
  mutate(name = country) %>%
  left_join(world_countries_grid1) %>%
  select(row, col, name, country, everything())

ratings_gridded$row <- if_else(ratings_gridded$name == "Moldova", 7, if_else(ratings_gridded$name == "Bosnia and Herzegovina", 7, as.double(ratings_gridded$row)))
ratings_gridded$col <- if_else(ratings_gridded$name == "Moldova", 18, if_else(ratings_gridded$name == "Bosnia and Herzegovina", 15, as.double(ratings_gridded$col)))

ratings_gridded$row <- if_else(ratings_gridded$name == "Macedonia", 7, if_else(ratings_gridded$name == "Greece", 8, as.double(ratings_gridded$row)))
ratings_gridded$col <- if_else(ratings_gridded$name == "Macedonia", 16, if_else(ratings_gridded$name == "Greece", 16, as.double(ratings_gridded$col)))


grid <- 
  world_countries_grid1 %>%
  filter(name == "Belgium") %>%
  select(col, row, name) %>%
  bind_rows(ratings_gridded)

grid %>%
  filter(name != "Malta") %>%
  mutate(name = if_else(name == "Bosnia and Herzegovina", "Bosnia\n+\nHerzegovina", if_else(name == "Czech Republic", "Czech\nRepublic", name))) %>%
  ggplot(aes(x = col, y = row)) +
  geom_tile(aes(fill = points), colour = '#ffffff', size = 0.5) +
  geom_text(aes(label = name), size = 3, colour = '#000000', alpha = 0.5, fontface = 'bold', show.legend = FALSE) +
  scale_fill_gradientn(colours = pal, na.value = '#bcbcbc', 
                       breaks = c(86, 88),
#                       limits = c(82, 92),
                       guide = guide_continuous) +
  scale_y_reverse(breaks = NULL) +
  scale_x_continuous(breaks = NULL) +
  labs(title = "viticultures", subtitle = "SCORES BY COUNTRY", 
       x = "", y = "n") + 
  theme_void() + 
  theme(plot.background = element_rect(fill = '#bcbcbc', colour = '#bcbcbc'),
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
        plot.margin = margin(20, 20, 20, 20),
        legend.text = element_text(angle = 270),
        legend.title = element_text(angle = 270),
        legend.position = c(0.95, 0.7)) +
  ggsave("test2.png", height = 8, width = 8, dpi = 300)
  

unique(wine_ratings$variety)

fun <- colorRampPalette(pal)

wine_ratings %>%
  filter(country == "Spain" | country == "Portugal" | country == "France" | country == "Italy") %>%
  mutate(variety = str_replace(variety, pattern = "-", replacement = " ")) %>%
  mutate(varietal = case_when(str_detect(variety, "Chardonnay") ~ "Chardonnay",
                              str_detect(variety, "Pinot Noir") ~ "Pinot Noir",
                              str_detect(variety, "Cabernet") ~ "Cabernet",
                              str_detect(variety, "Pinot Gris") ~ "Pinot Gris",
                              str_detect(variety, "Pinot Grigio") ~ "Pinot Grigio",
                              str_detect(variety, "Syrah") ~ "Syrah",
                              str_detect(variety, "Merlot") ~ "Merlot",
                              str_detect(variety, "Malbec") ~ "Malbec",
                              str_detect(variety, "Carignan") ~ "Carignan",
                              str_detect(variety, "Grenache") ~ "Grenache",
                              str_detect(variety, "Sangiovese") ~ "Sangiovese",
                              str_detect(variety, "Riesling") ~ "Riesling",
                              str_detect(variety, "Sauvignon Blanc") ~ "Sauvignon Blanc",
                              str_detect(variety, "Shiraz") ~ "Shiraz",
                              str_detect(variety, "Gewürztraminer") ~ "Gewürztraminer",
                              str_detect(variety, "Carménère") ~ "Carménère",
                              str_detect(variety, "Tempranillo") ~ "Tempranillo",
                              str_detect(variety, "Zinfandel") ~ "Zinfandel",
                              str_detect(variety, "Muscat") ~ "Muscat",
                              str_detect(variety, "Champagne") ~ "Champagne",
                              str_detect(variety, "Chenin Blanc") ~ "Chenin Blanc",
                              TRUE ~ "what?")) %>%
  mutate(varietal = fct_relevel(varietal, "what")) %>%
  group_by(country, varietal) %>%
  summarise(price = mean(price, na.rm = TRUE),
            points = mean(points,  na.rm = TRUE),
            n = n()) %>%
  ggplot(aes(log(price), points)) +
  geom_point(aes(colour = varietal, size = n)) +
  scale_colour_manual(values = fun(20)) +
  scale_size_continuous(range = c(2, 8)) +
  facet_wrap(~ country) +
  labs(title = "comparative advantage", subtitle = "NATIONS POINTS + PRICES", 
       x = "log price", y = "mean points") + 
  theme_minimal() +
  theme(plot.background = element_rect(fill = '#bcbcbc', colour = '#bcbcbc'),
        panel.grid.major.x = element_line(size = 0.1, colour = 'grey50'),
        panel.grid.major.y = element_line(size = 0.1, colour = 'grey50'),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.y = element_line(size = 0.5, colour = 'black'),
        axis.line.x = element_blank(),
        axis.ticks.y = element_line(size = 0.5, colour = 'black'),
        axis.ticks.x = element_line(size = 0.1, colour = 'grey50'),
        axis.text.x = element_text(face = 'bold'),
        axis.text.y = element_text(face = 'bold'),
        plot.title = element_text(face = 'bold', colour = 'grey50'),
        plot.subtitle =  element_text(face = 'plain', colour = 'black'),
        strip.text = element_text(face = 'bold', colour = 'black'),
        plot.margin = margin(20, 20, 20, 20)) + 
  ggsave("test3.png", height = 8, width = 8, dpi = 300)

