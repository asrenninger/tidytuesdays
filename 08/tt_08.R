##

library(tidyverse)

##

wildlife_impacts <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-23/wildlife_impacts.csv")

##

glimpse(wildlife_impacts)

##

crosswalk <- 
  wildlife_impacts %>%
  filter(state != "N/A") %>%
  filter(airport_id != "ZZZZ") %>%
  select(airport_id, state) %>%
  distinct() %>%
  mutate(city = glue("{airport_id} Airport, {state}"))

##

library(googleway)
library(glue)

##

geocoderesults <- tibble()

##

for (i in 275:nrow(crosswalk)){
  
  index <- i
  
  city <- 
    crosswalk %>%
    slice(index) %>%
    pull(city) 
  
  location <- google_geocode(city, key = "YOURKEY")
  
  located  <- tibble(city = city, 
                     address = c(location$results$formatted_address),
                     lat = c(location$results$geometry$location[[1]]),
                     lon = c(location$results$geometry$location[[2]])) %>%
    slice(1)
  
  geocoderesults <- bind_rows(geocoderesults, located)
  
  Sys.sleep(1)
  
}

crosswalk %>% slice(274)

glimpse(geocoderesults)
glimpse(crosswalk)

##

library(sf)

##

impacts_spatial <-
  wildlife_impacts %>%
  left_join(crosswalk) %>%
  left_join(geocoderesults) %>%
  drop_na(address) %>%
  filter(!str_detect(species, "Unknown")) %>%
  filter(lon < 0 & lon > -150 & lat < 50 & lat > 20) %>%
  mutate(x = lon, y = lat) %>%
  st_as_sf(coords = c("x", "y"), 
           crs = 4326) %>%
  st_transform(102003)

coords <- 
  st_coordinates(impacts_spatial)

##

library(rnaturalearth)

##

states <- 
  ne_states(returnclass = 'sf') %>% 
  filter(adm0_a3 == "USA") %>%
  filter(!str_detect(name, "Alaska|Hawaii")) %>%
  select(name)

states <- st_transform(states, 102003)

##

library(janitor)

##

cities <-
  ne_download(scale = 'medium', type = 'populated_places', category = 'cultural', returnclass = 'sf') %>%
  clean_names() %>%
  filter(adm0_a3 == "USA" & name != "Honolulu" & pop2010 > 250) %>%
  select(name, pop2010)

cities <- st_transform(states, 102003)

##

states_buffered <-
  states %>%
  mutate(dissolve = 1) %>%
  summarise() %>%
  st_buffer(100000)

##

cells <-
  states_buffered %>%
  st_make_grid(cellsize = 20000)

##

library(kknn)

##

species_train <-
  impacts_spatial %>%
  mutate(lon = coords[, 1],
         lat = coords[, 2]) %>%
  st_drop_geometry() %>%
  select(species, lon, lat) %>%
  group_by(species) %>% 
  nest() %>% 
  mutate(num = map_int(data, nrow)) %>% 
  arrange(desc(num)) %>% 
  slice(1:8) %>% 
  unnest() %>% 
  select(-num) %>%
  mutate(species = factor(species))

##

ggplot(species_train, aes(x = lon, y = lat, colour = species)) +
  geom_point()

##

k <- 1000

species_result <- tibble(species = factor(NA), 
                         lon = st_coordinates(cells)[, 1], 
                         lat = st_coordinates(cells)[, 2])

##

species_kknn <- kknn::kknn(species ~ ., 
                           train = species_train, 
                           test = species_result, 
                           kernel = "gaussian", 
                           k = 1000)

##

species_result <-
  species_result %>%
  mutate(species = fitted(species_kknn), 
         prob = apply(species_kknn$prob, 
                      1, 
                      function(x) max(x)))

##

states_grid <-
  cells %>%
  st_sf() %>%
  rownames_to_column() %>%
  st_join(states) %>%
  drop_na() %>%
  group_by(rowname) %>%
  summarise() %>%
  st_as_sf()

##

species_grid <- 
  species_result %>%
  mutate(x = lon, y = lat) %>%
  st_as_sf(coords = c("x", "y"),
           crs = 102003) %>%
  st_join(states_grid) %>%
  st_drop_geometry() %>%
  drop_na() %>%
  left_join(states_grid) %>%
  st_as_sf()

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
          plot.margin = margin(20, 20, 20, 20),
          legend.title = element_text(face = 'bold', angle = 270)
    )
  
}

##

guide_discrete <-
  guide_legend(direction = "vertical",
               keyheight = unit(10, units = "mm"),
               keywidth = unit(2, units = "mm"),
               title.position = 'right',
               label.position = 'left',
               title.hjust = 0.5,
               label.hjust = 1,
               ncol = 1,
               bycol = TRUE)

##

species_raster <-
  species_result %>%
  st_as_sf(coords = c("lon", "lat"),
           crs = 102003, 
           remove = FALSE) %>%
  st_intersection(states)

plot(species_raster[, 1])

##

install.packages("rcartocolor")
library(rcartocolor)

##

map <- 
  ggplot() +
  geom_raster(data = species_raster,
              aes(x = lon, y = lat, fill = species, alpha = prob)) +
  geom_sf(data = states, 
          aes(), fill = NA, size =  0.1, colour = 'black', alpha = 0.5) +
  scale_fill_carto_d(palette = "Safe",
                     na.translate = FALSE,
                     guide = guide_discrete) +
  scale_alpha(guide = 'none') +
  coord_sf(crs = 102003) +
  labs(title = "whodunnit?", subtitle = "INTERPOLATED LIKELIHOOD",
       caption = "bird most likely to bring down your plane using categorical interpolation") +
  theme_map()

##

guide_discrete <-
  guide_legend(direction = "vertical",
               keyheight = unit(10, units = "mm"),
               keywidth = unit(2, units = "mm"),
               title.position = 'right',
               label.position = 'left',
               title.hjust = 0.5,
               label.hjust = 1,
               ncol = 1,
               bycol = TRUE,
               override.aes = list(size = 3))

##

map <- 
  ggplot() +
  geom_point(data = species_train %>%
               filter(species != "American kestrel"),
             aes(x = lon, y = lat, colour = species),
             size = 1, alpha = 0.5) +
  geom_sf(data = states, 
          aes(), fill = NA, size =  0.1, colour = 'black', alpha = 0.5) +
  scale_colour_carto_d(palette = "Safe",
                      na.translate = FALSE,
                      guide = guide_discrete) +
  scale_alpha(guide = 'none') +
  coord_sf(crs = 102003) +
  labs(title = "whodunnit?", subtitle = "TRAINING DATA",
       caption = "incidences of birds striking planes by airport") +
  theme_map()

##

ggsave(map, path = "~/Desktop", filename = "test.png", height = 6, width = 7, dpi = 300)  

##
