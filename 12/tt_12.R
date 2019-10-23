library(tidyverse)

##

horror_movies <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")

##

glimpse(horror_movies)

## 

locations <-
  horror_movies %>%
  select(filming_locations) %>%
  drop_na() %>%
  distinct()

##

library(googleway)
library(glue)

##

geocoderesults <- tibble()

for (i in 1:length(locations$filming_locations)){
  
  index <- i
  
  city <- as.character(locations$filming_locations[index])
  
  location <- google_geocode(city, key = "YOURKEY")
  
  located  <- tibble(city = city, 
                     name = c(location$results$formatted_address),
                     lat = c(location$results$geometry$location[1]),
                     lon = c(location$results$geometry$location[2]))
  
  geocoderesults <- bind_rows(geocoderesults, located)
  
  Sys.sleep(1)
  
}

##

results <-
  geocoderesults %>%
  group_by(name) %>%
  slice(1)

##

latlon <- tibble()

for (i in 1:nrow(results)) {
  
  index <- i 
  
  Y <- results$lat[[index]][1]
  X <- results$lon[[index]][1]
  
  iteration <- tibble(lat = Y,
                      lon = X)
  
  latlon <- bind_rows(latlon, iteration)
  
}

##

shoots <-
  results %>%
  select(-lon, -lat) %>%
  bind_cols(latlon) %>%
  rename(filming_locations = city) %>%
  right_join(horror_movies) %>%
  drop_na(lon, lat)

##

glimpse(shoots)

ggplot(shoots, aes(lon, lat, colour = review_rating)) +
  geom_point()

##

library(rnaturalearth)
library(sf)

##

world <- ne_countries(scale = "medium", returnclass = "sf")

##

st_crs(world)

counts <- 
  shoots %>%
  st_as_sf(coords = c("lon", "lat"), remove = FALSE, crs = 4326) %>%
  st_join(world) %>%
  st_drop_geometry() %>%
  group_by(continent) %>%
  summarise(n = n())

##

poly <- 
  bind_cols(world %>%
              st_cast("POLYGON") %>%
              st_cast("MULTIPOINT") %>%
              st_cast("POINT"), 
            world %>%
              st_cast("POLYGON") %>%
              st_cast("MULTIPOINT") %>%
              st_coordinates() %>%
              as_tibble())

##

centroids <- 
  world %>%
  group_by(continent) %>%
  summarise() %>%
  filter(!str_detect(continent, "Antarctica|Seven")) %>%
  st_centroid() %>%
  left_join(counts) %>%
  st_as_sf()

coords <-
  centroids %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(centroids)

## 

plots <- list()

##

library(glue)

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
          plot.margin = margin(5, 5, 5, 5)
    )
  
}

##

pal <- read_csv("https://github.com/asrenninger/palettes/raw/master/orred.txt", col_names = FALSE) %>% pull(X1)

##

for (i in 1:nrow(centroids)) {
  
  plot <-
    ggplot() +
    geom_polygon(data = poly, 
                 aes(x = X, y = Y, group = L1),
                 fill = '#696969', colour = '#FFFFFF', size = 0.1) +
    geom_point(data = shoots, 
               aes(x = lon, y = lat, size = review_rating, colour = review_rating), 
               alpha = 0.5,
               show.legend = FALSE) +
    scale_size_continuous(range = c(0.1, 3)) +
    scale_colour_gradientn(colours = pal) +
    coord_map("ortho", orientation = c(coords$Y[i], coords$X[i], 0)) +
    labs(title = glue("{centroids$n[i]} film shoots"), subtitle = glue("{centroids$continent[i]}"), 
         x = "city", y = "number") +
    theme_map()
  
  plots[[glue("plot{i}")]] <- plot
  
}

## 

library(gridExtra)
library(grid)

## 

blank <- grid.rect(gp = gpar(col = 'transparent', fill = 'transparent'))

lay <- rbind(c(1, 1, 1, 2, 2, 2),
             c(1, 1, 1, 2, 2, 2),
             c(1, 1, 1, 2, 2, 2),
             c(3, 3, 3, 4, 4, 4),
             c(3, 3, 3, 4, 4, 4),
             c(3, 3, 3, 4, 4, 4),
             c(5, 5, 5, 6, 6, 6),
             c(5, 5, 5, 6, 6, 6),
             c(5, 5, 5, 6, 6, 6)) 

agg <- grobTree(rectGrob(gp = gpar(fill = 'transparent', lwd = 0)), 
                grid.arrange(grobs = plots, layout_matrix = lay))

ggsave(agg, filename = "aggregate.png", height = 12, width = 12, dpi = 300)

##
