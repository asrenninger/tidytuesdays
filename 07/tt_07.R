devtools::install_github("tylermorganwall/rayshader")

##

library("tidyverse")

##

ufo_sightings <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")

##

glimpse(ufo_sightings)

##

grid <- 
  counties %>%
  st_make_grid(cellsize = 50000) %>%
  st_sf() %>%
  rownames_to_column() %>%
  st_join(counties) %>%
  drop_na() %>%
  group_by(rowname) %>%
  summarise() %>%
  st_as_sf()

plot(grid)

##

ufo_sf <-
  ufo_sightings %>%
  mutate(x = longitude, y = latitude) %>%
  drop_na() %>%
  st_as_sf(coords = c("x", "y")) %>%
  st_set_crs(4269) %>%
  st_transform(102003) %>%
  st_join(grid) %>%
  st_drop_geometry() %>%
  group_by(rowname) %>%
  summarise(sightings = n()) %>%
  right_join(grid) %>%
  replace_na(list(sightings = 0)) %>%
  st_as_sf()

ufo_sf %>%
  filter(country == "us") %>%
  select(date_time) %>%
  st_coordinates() %>%
  as_tibble() %>%
  ggplot(aes(X, Y)) +
  stat_density_2d(aes(fill = stat(nlevel)), geom = 'polygon', n = 100, bins = 10)

ggplot(ufo_sightings %>%
         filter(country == "us"), aes(longitude, latitude)) +
  geom_point()

##

library(viridis)

##

theme_map <- function () {
  theme_void() + 
    theme(plot.background = element_rect(fill = 'white', colour = 'white'),
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
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'black', size = 15),
          plot.margin = margin(20, 20, 20, 20),
          strip.text = element_text(face = 'bold', colour = 'white'),
          panel.grid.major = element_line(size = NA), 
          panel.grid.minor = element_line(size = NA),
          legend.title = element_text(angle = 270, face = 'bold', colour = 'grey50'),
          legend.text = element_text(angle = 270)
    )
  
}

##

plot_sightings <-
  ggplot(ufo_sf) +
  geom_sf(aes(fill = sightings), colour = NA, size = 0) +
  scale_fill_viridis(guide =   guide_colorbar(direction = "vertical",
                                              barheight = unit(50, units = "mm"),
                                              barwidth = unit(2, units = "mm"),
                                              draw.ulim = FALSE,
                                              title.position = 'right',
                                              label.position = 'left',
                                              title.hjust = 0.5,
                                              label.hjust = 0.5),
                     name = "ufo sitings") +
  ggtitle("O'ER THE LAND OF THE FREE") +
  theme_bw() +
  theme(legend.title = element_text(angle = 270, face = 'bold', colour = 'grey50'),
        legend.text = element_text(angle = 270))

plot_sightings

##

library(rayshader)

##

plot_gg(plot_sightings, multicore = TRUE, width = 10, height = 8, scale = 250,
        soliddept = -10)

render_depth(focallength = 100, focus = 0.72)

##

?plot_gg
?render_movie

##

render_movie(filename = "test3", type = 'orbit', frames = 360, fps = 30,
             phi = 30, theta = 0, zoom = 0.5)
