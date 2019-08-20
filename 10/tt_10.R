library(tidyverse)
library(sf)

##

library(rnaturalearth)

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

nuclear_explosions <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")

##

theme_map <- function () {
  theme_void() + 
    theme(plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
          panel.grid.major.x = element_line(size = 0.1, colour = 'grey50'),
          panel.grid.major.y = element_line(size = 0.1, colour = 'grey50'),
          panel.grid.minor.x = element_line(size = 0.1, colour = 'grey50'),
          panel.grid.minor.y = element_line(size = 0.1, colour = 'grey50'),
          panel.grid.major = element_line(size = NA), 
          panel.grid.minor = element_line(size = NA),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(face = 'bold', colour = 'black'),
          plot.subtitle =  element_text(face = 'plain', colour = 'black', size = 15),
          plot.caption = element_text(face = 'bold', colour = 'black'),
          strip.text = element_text(face = 'bold', colour = 'black'),
          plot.margin = margin(20, 20, 20, 20)
    )
  
}

##

ggplot() +
  geom_polygon(data = w_Poly, 
               aes(x = X, y = Y, group = L1),
               fill = '#696969', colour = '#FFFFFF', size = 0.1) +
  geom_point(data = nuclear_explosions, 
             aes(x = longitude, y = latitude, colour = yield_upper, size = yield_upper), show.legend = FALSE) +
  scale_colour_gradientn(colours = pal[2:11]) +
  scale_size_continuous(range = c(0, 10)) +
  coord_map("ortho", orientation = c(42.952287, -40.989156, 0)) +
  labs(title = "doom around the globe", subtitle = "NUCLEAR DETONATIONS") +
  theme_map() +
  ggsave(filename = "test.png", height = 10, width = 10, dpi = 300)

##

explosions_filled <-
  nuclear_explosions %>%
  mutate(rounded = round(longitude, -1)) %>%
  right_join(tibble(rounded = seq(from = -180, to = 180, by = 10)))
        
##

library(magick)

##

img <- image_graph(1000, 1000, res = 300)

datalist <- split(explosions_filled, explosions_filled$rounded)

##

out <- lapply(datalist, function(data){
  
  p <- 
    ggplot() +
    geom_polygon(data = w_Poly, 
                 aes(x = X, y = Y, group = L1),
                 fill = '#696969', colour = '#FFFFFF', size = 0.1) +
    geom_point(data = nuclear_explosions, 
               aes(x = longitude, y = latitude, colour = yield_upper, size = yield_upper), show.legend = FALSE) +
    scale_colour_gradientn(colours = pal[2:11]) +
    scale_size_continuous(range = c(0, 10)) +
    coord_map("ortho", orientation = c(42.952287, unique(data$rounded), 0)) +
    ggtitle("atomic booms") +
    theme_map()
  
  print(p)
  
})

dev.off()

##

animation <- image_animate(img, fps = 2)
image_write(animation, "booms.gif")

##

