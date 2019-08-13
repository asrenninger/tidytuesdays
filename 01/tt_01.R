library(tidyverse)

##

nobel_winners <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")
nobel_winner_all_pubs <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winner_all_pubs.csv")

##

glimpse(nobel_winners)
glimpse(nobel_winner_all_pubs)

##

library(scico)

##

nobel_winners %>%
  filter(!is.na(organization_city)) %>%
  group_by(organization_city) %>%
  summarise(n = n()) %>%
  top_n(10, n) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(organization_city, n), y = n)) +
  geom_bar(aes(fill = n), stat = 'identity', show.legend = FALSE) +
  scale_fill_gradientn(colours = scico(palette = 'oslo', 11, direction = -1)[2:11]) + ## 2:11 because oslo starts off white
  labs(title = "learned places", subtitle = "cities with most nobel laureates", 
       x = "city", y = "number") +
  coord_flip() +
  theme_rot() +
  ggsave(filename = "bars.png", height = 6, width = 6, dpi = 300)

##

nobel_winners %>%
  filter(!is.na(organization_city)) %>%
  group_by(prize_year, organization_city) %>%
  summarise(n = n()) %>%
  right_join(tibble(prize_year = 1901:2018)) %>%
  ggplot(aes(x = prize_year, y = n)) +
  geom_line(aes(colour = n), stat = 'identity', show.legend = FALSE) +
  scale_colour_gradientn(colours = scico(palette = 'oslo', 11, direction = -1)[2:11]) + ## 2:11 because oslo starts off white
  labs(title = "learned places", subtitle = "cities with most nobel laureates", 
       x = "year", y = "city") +
  theme_hor()

##

top <- 
  nobel_winners %>% 
  filter(!is.na(organization_city)) %>%
  group_by(organization_city) %>%
  summarise(n = n()) %>%
  top_n(9, n) %>%
  pull(organization_city)

##

nobel_winners %>%
  filter(!is.na(organization_city)) %>%
  group_by(organization_city, category) %>%
  summarise(n = n()) %>%
  filter(organization_city %in% top) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = category, y = n)) +
  geom_bar(aes(fill = n), stat = 'identity', show.legend = FALSE) +
  scale_fill_gradientn(colours = scico(palette = 'oslo', 11, direction = -1)[2:11]) + ## 2:11 because oslo starts off white
  labs(title = "learned places", subtitle = "cities with most nobel laureates", 
       x = "category", y = "number") +
  coord_flip() +
  facet_wrap(~ organization_city) +
  theme_rot() +
  ggsave(filename = "facets.png", height = 8, width = 8, dpi = 300)

##

nobel_winners %>%
  filter(!is.na(organization_city)) %>%
  group_by(organization_city, category) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = n)) +
  geom_density(aes(fill = n), show.legend = FALSE) +
  scale_fill_gradientn(colours = scico(palette = 'oslo', 11, direction = -1)[2:11]) + ## 2:11 because oslo starts off white
  labs(title = "distribution of cities", subtitle = "which prize requires the right milieux", 
       x = "number", y = "density") +
  facet_wrap(~ category, nrow = 1) +
  theme_hor() +
  ggsave(filename = "densities.png", height = 6, width = 8, dpi = 300)

##

library(glue)

##

cities <- 
  bind_rows(nobel_winners %>%
              select(full_name, birth_city, birth_country) %>%
              rename(city = birth_city, country = birth_country) %>%
              mutate(type = "birth"),
            nobel_winners %>%
              select(full_name, death_city, death_country) %>%
              rename(city = death_city, country = death_country) %>%
              mutate(type = "death"),
            nobel_winners %>%
              select(full_name, organization_city, organization_country) %>%
              rename(city = organization_city, country = organization_country) %>%
              mutate(type = "institution")) %>%
  mutate(location = glue("{city}, {country}")) %>%
  drop_na()

##

glimpse(cities)

library(googleway)
library(glue)

geocoderesults <- tibble()

for (i in 476:length(unique(cities$location))){
  
  index <- i
  
  city <- as.character(unique(cities$location)[index])
  
  location <- google_geocode(city, key = "YOURKEY")
  
  located  <- tibble(city = city, 
                     name = c(location$results$formatted_address),
                     lat = c(location$results$geometry$location[1]),
                     lon = c(location$results$geometry$location[2]))
  
  geocoderesults <- bind_rows(geocoderesults, located)
  
  Sys.sleep(1)
  
}

rank_joinLocation <- left_join(allyears, geocoderesults)

cities_located <- 
  cities %>%
  left_join(rename(geocoderesults, 
                   location = city,
                   address = name)) %>%
  drop_na()

cities_located$lat

latlon <- tibble()

for (i in 1:length(pull(cities_located, lat))) {
  
  index <- i 
  
  Y <- cities_located$lat[[index]][1]
  X <- cities_located$lon[[index]][1]
  
  iteration <- tibble(lat = Y,
                      lon = X)
  
  latlon <- bind_rows(latlon, iteration)
  
}

cities_located <- 
  cities_located %>%
  select(-lat, -lon) %>%
  bind_cols(latlon)

##

library(rnaturalearth)
library(sf)

##

countries  <- 
  ne_countries() %>% 
  st_as_sf() %>%
  select(name) %>%
  st_transform(54030)

##

scico(palette = 'grayC', 10)

##

cities_shaped <- 
  cities_located %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(4326) %>%
  st_transform(st_crs(countries)) %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(cities_located)

##

w <- ne_countries(scale = "medium", returnclass = "sf")

w_Poly <- bind_cols(w %>%
                      st_cast("POLYGON") %>%
                      st_cast("MULTIPOINT") %>%
                      st_cast("POINT"), 
                    w %>%
                      st_cast("POLYGON") %>%
                      st_cast("MULTIPOINT") %>%
                      st_coordinates() %>%
                      as_tibble())

##

ggplot() +
  geom_sf(data = countries, 
          aes(), fill = '#696969', colour = '#FFFFFF', size = 0.1) +
  geom_point(data = cities_shaped %>%
               filter(type == "institution"), 
             aes(x = X, y = Y, group = full_name, colour = full_name), show.legend = FALSE) +
  geom_line(data = cities_shaped %>%
              filter(type == "institution"), 
            aes(x = X, y = Y, group = full_name, colour = full_name), show.legend = FALSE) +
  theme_map()

##

ggplot() +
  geom_polygon(data = w_Poly, 
               aes(x = X, y = Y, group = L1),
               fill = '#696969', colour = '#FFFFFF', size = 0.1) +
  geom_point(data = cities_shaped %>%
               filter(type == "institution") %>%
               group_by(location, lat, lon) %>%
               summarise(n = n()), 
             aes(x = lon, y = lat, colour = n), show.legend = FALSE) +
  scale_colour_gradientn(colours = scico(palette = 'oslo', 11, direction = -1)[2:11]) +
  scale_size_continuous(range = c(0, 20)) +
  coord_map("ortho", orientation = c(42.952287, -40.989156, 0)) +
  labs(title = "learned places", subtitle = "cities with most nobel laureates", 
       x = "city", y = "number") +
  theme_map() +
  ggsave(filename = "locations.png", height = 10, width = 10, dpi = 300)
