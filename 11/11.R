library(tigris)
library(sf)

##

options(tigris_use_cache = TRUE)

##

roads <- roads("NY", "New York", class = 'sf')
tracts <- tracts("NY", "New York", class = 'sf')

water <- 
  area_water("NY", "New York", class = 'sf') %>%
  st_union() %>%
  st_combine()

##

options(scipen = 999)

library(tidyverse)
library(scales)
library(magrittr)
library(classInt)
library(janitor)

##

background <-
  tracts %>%
  mutate(dissolve = 1) %>%
  group_by(dissolve) %>%
  summarise() %>%
  st_difference(water)

##

roads_local <- 
  roads %>% 
  filter(str_detect(MTFCC, "S1200|S1400")) %>%
  filter(str_detect(RTTYP, "M|C"))

##

library(stplanr)

##
##

network <- 
  roads_local %>%
  as('Spatial') %>%
  SpatialLinesNetwork()

graph <- network@g

##

library(igraph)

##

is.simple(graph)
simplied <- simplify(graph)
is.simple(simplied)

##

coords <- 
  simplified %>%
  set_vertex_attr('x', value = use_series(simplified, x)) %>%
  set_vertex_attr('y', value = use_series(simplified, y))

coords <- 
  graph %>%
  set_vertex_attr('x', value = use_series(simplified, x)) %>%
  set_vertex_attr('y', value = use_series(simplified, y))

##

attributes <- 
  coords %>%
  set_edge_attr('head_x', value = head_of(., E(.))$x) %>%
  set_edge_attr('tail_x', value = tail_of(., E(.))$x) %>%
  set_edge_attr('head_y', value = head_of(., E(.))$y) %>%
  set_edge_attr('tail_y', value = tail_of(., E(.))$y)

##

attributes <- 
  attributes %>%
  set_edge_attr('slope', value = map_dbl(E(.), function(e){
    slope = (e$tail_y - e$head_y)/(e$tail_x - e$head_x)
    if(is.infinite(slope)) return(Inf)
    return(slope)
  })) %>%
  set_edge_attr('color', value = map_chr(E(.)$slope, function(e){
    if(e < 0) return('red')
    return('blue')
  }))

##

plot(attributes, vertex.label = '', vertex.size = .1)

##

roads_labelled <-
  roads_local %>%
  mutate(slope = E(attributes)$slope) %>%
  mutate(rounded = round(slope, 1)) %>% 
  mutate(street = case_when(rounded == -0.4 ~ "sidestreet",
                            rounded > 1.2 & rounded < 1.7 ~ "avenue",
                            TRUE ~ "breaks")) 

##

mapview(roads_labelled, zcol = "street") 

##

guide_discrete <-
  guide_legend(direction = "vertical",
               keywidth = unit(1, units = "mm"),
               keyheight = unit(50 / 3, units = "mm"),
               title.position = 'left',
               label.position = 'right',
               title.vjust = 0.5,
               label.vjust = 1,
               ncol = 1,
               bcol= TRUE)

theme_bm_legend <- function () {
  theme_void() + 
    theme(plot.background = element_rect(fill = 'black', colour = 'black'),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.title = element_text(colour = 'grey50', angle = 90),
          legend.text = element_text(colour = 'white', angle = 90),
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'white', size = 15),
          panel.grid.major = element_line(size = NA), 
          panel.grid.minor = element_line(size = NA),
          legend.position = c(0.8, 0.2),
          #  legend.background = element_rect(fill = "grey10", colour = "grey10", 
          #                                   size = 5, linetype="solid"),
          plot.margin = margin(20, 20, 20, 20)
    )
  
}

##

pal <- read_csv("https://github.com/asrenninger/palettes/raw/master/turbo.txt", col_names = FALSE) %>% pull(X1) 
fun <- colorRampPalette(pal)

##

ggplot() +
  geom_sf(data = background,
          aes(), fill = '#353535', colour = NA, size = 0) +
  geom_sf(data = roads_labelled %>%
            mutate(street = fct_relevel(street, "sidestreet", "avenue", "breaks")),
          aes(colour = street, fill = street), size = 0.25) +
  scale_fill_manual(values = fun(9)[c(2, 5, 8)],
                    na.translate = FALSE,
                    guide = guide_discrete) +
  scale_color_manual(values = fun(9)[c(2, 5, 8)],
                     na.translate = FALSE,
                     guide = guide_discrete) +
  labs(title = "manhattan streets", subtitle = "TYPOLOGIES") +
  theme_bm_legend() +
  ggsave("streets.png", height = 6.5, width = 3.5, dpi = 300)

##

pizza_datafiniti <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-01/pizza_datafiniti.csv")

##

pizza_barstool <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-01/pizza_barstool.csv")

##

barstool_trimmed <- 
  pizza_barstool %>%
  drop_na(latitude, longitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), remove = FALSE, crs = 4326) %>%
  st_intersection(st_transform(background, 4326)) %>%
  st_drop_geometry()

datafiniti_trimmed <- 
  pizza_datafiniti %>%
  drop_na(latitude, longitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), remove = FALSE, crs = 4326) %>%
  st_intersection(st_transform(background, 4326)) %>%
  st_drop_geometry()

glimpse(barstool_trimmed)
glimpse(datafiniti_trimmed)

##

theme_bm_legend_2 <- function () {
  theme_void() + 
    theme(plot.background = element_rect(fill = 'black', colour = 'black'),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.title = element_text(colour = 'grey50'),
          legend.text = element_text(colour = 'white'),
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'white', size = 15),
          panel.grid.major = element_line(size = NA), 
          panel.grid.minor = element_line(size = NA),
          legend.position = c(0.8, 0.2),
          #  legend.background = element_rect(fill = "grey10", colour = "grey10", 
          #                                   size = 5, linetype="solid"),
          plot.margin = margin(20, 20, 20, 20)
    )
  
}

##

ggplot() +
  geom_sf(data = background,
          aes(), fill = '#353535', colour = NA, size = 0) +
  geom_sf(data = roads_labelled,
          aes(), colour = '#a1a1a1', size = 0.25) +
  geom_point(data = barstool_trimmed,
             aes(x = longitude, y = latitude, colour = factor(ntile(review_stats_all_average_score, 5))), 
             size = 1) +
  scale_color_manual(values = fun(5),
                     labels = str_sub(as.character(quantile(barstool_trimmed$review_stats_all_average_score,
                                                            c(.1,.2,.4,.6,.8),
                                                            na.rm = TRUE)), 1, 4),
                     na.translate = FALSE,
                        name = "rating") +
  labs(title = "manhattan pizzerias", subtitle = "AVERAGE RATING") +
  theme_bm_legend_2() +
  ggsave("pizzerias.png", height = 6, width = 3.5, dpi = 300)

##

library(RANN)

##

coords <- roads_labelled %>%
  st_cast('POINT') %>%
  st_coordinates() %>%
  as_tibble()

points <- 
  roads_labelled %>%
  st_cast('POINT') %>%
  st_drop_geometry() %>%
  bind_cols(coords) %>%
  rownames_to_column() %>%
  mutate(nearest = as.numeric(rowname)) %>%
  as_tibble()

##

closest <-
  nn2(data = tibble(X = points$X, Y = points$Y), 
      query = tibble(X = barstool_trimmed$longitude, Y = barstool_trimmed$latitude),
      k = 1, 
      searchtype = "radius", 
      radius = 500)

as_tibble(closest$nn.idx)

barstool_linked <-
  barstool_trimmed %>%
  bind_cols(as_tibble(closest$nn.idx)) %>%
  rename(nearest = V1) %>%
  left_join(points)

barstool_linked %>%
  ggplot(aes(x = review_stats_all_average_score)) +
  geom_density() +
  facet_wrap(~ street)

##

theme_rot <- function () {
  theme_minimal() +
    theme(plot.background = element_rect(fill = 'black', colour = 'black'),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_line(size = 0.5, colour = 'white'),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_text(face = 'bold', colour = 'white'),
          axis.text.y = element_blank(),
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'white'),
          strip.text = element_text(face = 'bold', colour = 'white'),
          plot.margin = margin(20, 20, 20, 20)
    )
}

##

barstool_trimmed <-
  barstool_linked %>%
  mutate(range = ntile(review_stats_all_average_score, 100)) %>%
  filter(range != 100 & range != 1 & !is.na(review_stats_all_average_score))

city_average <-
  barstool_trimmed %>%
  summarise(city_avg = mean(review_stats_all_average_score, na.rm = TRUE))

street_averages <-
  barstool_trimmed %>%
  group_by(street) %>%
  summarise(street_avg = mean(review_stats_all_average_score, na.rm = TRUE))

comparisons <-
  barstool_trimmed %>%
  left_join(street_averages) %>%
  mutate(city_avg = pull(city_average, city_avg))
  
##

ggplot(comparisons, 
       aes(review_stats_all_average_score)) +
  geom_density(aes(fill = street, colour = street), show.legend = FALSE, alpha = 0.5) +
  geom_vline(aes(xintercept = street_avg), color = "gray70", size = 0.75, linetype = 2) +
  facet_wrap(~ street, ncol = 1) +
  scale_fill_manual(values = fun(9)[c(2, 5, 8)],
                    na.translate = FALSE,
                    guide = 'none') +
  scale_color_manual(values = fun(9)[c(2, 5, 8)],
                     na.translate = FALSE,
                     guide = 'none') +
  theme_rot() +
  ggsave("ratings.png", height = 6, width = 8, dpi = 300)

##

library(nngeo)

##

joined <- 
  pizza_barstool %>%
  drop_na(latitude, longitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), remove = FALSE, crs = 4326) %>%
  st_transform(4269) %>%
  st_intersection(background) %>%
  st_nn(roads_labelled)

index <- 
  roads_labelled %>%
  rownames_to_column() %>%
  rename(index = rowname) %>%
  mutate(index = as.numeric(index))

barstool_trimmed %>%
  mutate(index = unlist(joined)) %>%
  left_join(index) %>%
  st_as_sf(coords = c("longitude", "latitude"), remove = FALSE, crs = 4326) %>%
  mapview(zcol = "street")
  


