library(tidyverse)

##

meteorites <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")

##

library(rnaturalearth)
library(sf)

##

w <- ne_countries(scale = 'medium', returnclass = 'sf')
p <- ne_download(scale = 'medium', type = 'populated_places', category = 'cultural', returnclass = 'sf')

##

w_Poly <- 
  bind_cols(w %>%
              st_cast("POLYGON") %>%
              st_cast("MULTIPOINT") %>%
              st_cast("POINT"), 
            w %>%
              st_cast("POLYGON") %>%
              st_cast("MULTIPOINT") %>%
              st_coordinates() %>%
              as_tibble())

p_Coord <-
  p %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(p)

##

places <-
  p %>%
  st_transform(54030) %>%
  st_coordinates() %>%
  as.matrix()

sites <- 
  meteorites %>%
  drop_na(long, lat) %>%
  mutate(X = long, Y = lat) %>%
  st_as_sf(coords = c("X", "Y")) %>%
  st_set_crs(4269) %>%
  st_transform(54030) %>%
  st_coordinates() %>%
  as.matrix()

##

ggplot() +
  geom_point(data = sites %>%
               as_tibble(),
             aes(X, Y)) +
  geom_point(data = places %>%
               as_tibble(),
             aes(X, Y), colour = 'blue')

##

library(FNN)

##

nn <- get.knnx(places, sites, k = 1)

##

distances <-
  as.data.frame(nn$nn.dist) %>%
  rownames_to_column(var = "places") %>%
  gather(site, dist_place, V1) %>%
  arrange(as.numeric(places)) %>%
  group_by(places) %>%
  summarize(d_place = mean(dist_place)) %>%
  arrange(as.numeric(places)) %>% 
  select(-places) %>%
  bind_cols(drop_na(meteorites, long, lat))

##

fun <- colorRampPalette(pal)
fun(10)

##

lab <- glue("{as.character(round(quantile((distances$d_place / 1000),
                                   c(.1, .2, .4, .6, .8), na.rm = TRUE)), 
                    0)} +")
                          
##

library(ggrepel)

##

ggplot() +
  geom_polygon(data = w_Poly, 
               aes(x = X, y = Y, group = L1),
               fill = '#696969', colour = '#ffffff', size = 0.1) +
  geom_point(data = p_Coord,
             aes(x = X, y = Y), size = 0.05, alpha = 0.5, colour = '#bababa') +
  geom_point(data = distances, 
             aes(x = long, y = lat, size = mass, colour = factor(ntile(d_place, 5))), alpha = 0.5) +
  geom_label_repel(data = distances %>%
                     group_by(d_place) %>%
                     slice(1) %>%
                     ungroup() %>%
                     mutate(site = str_remove(name, " [0-9]+")) %>%
                     group_by(site) %>%
                     slice(1) %>%
                     ungroup() %>%
                     top_n(50, d_place) %>%
                     sample_n(10) %>%
                     mutate(site = glue("{site}\n{round(d_place / 1000, 0)}km")),
                   aes(x = long, y = lat, label = site),
                   arrow = arrow(length = unit(0.01, "npc"), type = "open", ends = "last"),
                   force = 10) +
  scale_colour_manual(values = rev(fun(6)[1:5]),
                      labels = lab,
                      name = "isolation\n(km)",
                      guide = guide_legend(direction = "horizontal",
                                           keyheight = unit(2, units = "mm"),
                                           keywidth = unit(10, units = "mm"),
                                           title.position = 'top',
                                           label.position = 'bottom',
                                           title.hjust = 0.5,
                                           label.hjust = 0.5,
                                           nrow = 1,
                                           byrow = TRUE)) +
  scale_size_continuous(range = c(1, 10), guide = 'none') +
  coord_map(projection = "mollweide", orientation = c(140, 0, 45)) +
  labs(title = "observation biases", subtitle = "DISTANCE TO NEAREST POPULATED PLACE") +
  theme_map() +
  theme(legend.position = c(0.7, 0.9),
        legend.title = element_text(face = 'bold')) +
  ggsave(filename = "meteorites.png", path = "/Users/andrewrenninger/Desktop/R/tuesdays", height = 8, width = 12, dpi = 300)
