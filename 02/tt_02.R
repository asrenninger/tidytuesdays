library(tidyverse)
library(janitor)

coast_vs_waste <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/coastal-population-vs-mismanaged-plastic.csv") %>%
  clean_names()

mismanaged_vs_gdp <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv") %>%
  clean_names()

waste_vs_gdp <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv") %>%
  clean_names()

glimpse(coast_vs_waste)
glimpse(mismanaged_vs_gdp)
glimpse(waste_vs_gdp)

##

data <- 
  coast_vs_waste %>%
  left_join(waste_vs_gdp) %>%
  left_join(mismanaged_vs_gdp)

##

library(rnaturalearth)
library(sf)

##

countries  <- 
  ne_countries() %>% 
  st_as_sf() %>%
  rename(code = adm0_a3,
         name = admin,
         region = subregion) %>%
  select(code, name, region, continent) %>%
  st_transform(54030)

##

spatial_data <-
  data %>%
  filter(year == 2010) %>%
  left_join(countries) %>%
  st_as_sf()

##

coastline <- 
  ne_coastline(scale = 110) %>%
  st_as_sf() %>%
  st_transform(54030) %>%
  st_snap(countries, tolerance = 1000) %>%
  st_intersection(countries) %>%
  mutate(coastline = st_length(geometry)) %>%
  filter(region != "Antarctica") %>%
  group_by(name) %>%
  summarise(coastline = sum(coastline))

##

map_data <- 
  spatial_data %>%
  left_join(st_drop_geometry(coastline)) %>%
  mutate(bylength = mismanaged_plastic_waste_tonnes / (as.numeric(coastline) / 1000),
         bypopulation = mismanaged_plastic_waste_tonnes / coastal_population) %>%
  mutate(bylengthtile = ntile(bylength, 100),
         bypopulationtile = ntile(bypopulation, 100)) %>%
  replace_na(list(bylengthtile = 1,
                  bypopulationtile = 1,
                  mismanaged_plastic_waste_tonnes = 0)) %>%
  select(entity, name, region, continent, mismanaged_plastic_waste_tonnes, 
         bylengthtile, bypopulationtile, geometry) %>%
  drop_na() %>%
  st_as_sf()

##

pal <- c("#007f00",
         "#00501f",
         "#00223d",
         "#000c5d",
         "#003a7c",
         "#00699a",
         "#2f97ba",
         "#8cc6d8",
         "#ffffff")

##

guide_continuous <- 
  guide_colorbar(direction = "horizontal",
                 barwidth = unit(50, units = "mm"),
                 barheight = unit(1, units = "mm"),
                 draw.ulim = FALSE,
                 title.position = 'top',
                 label.position = 'bottom',
                 title.hjust = 0.5,
                 label.hjust = 0.5)


##

library(stringr)

##

ggplot(data %>%
         filter(year == 2010) %>%
         mutate(ratio = coastal_population / total_population_gapminder) %>%
         mutate(name = case_when(nchar(entity) > 15 ~ word(entity, 1, 2, sep=" "),
                                 TRUE ~ entity)) %>%
         top_n(64, ratio), 
       aes(x = "", y = ratio)) +
  geom_bar(aes(fill = ntile(ratio, 10)), width = 1, stat = "identity", show.legend = FALSE) +
  coord_polar("y", start = 0) +
  scale_fill_gradientn(colours = rev(pal[1:8])) +
  facet_wrap(~ reorder(name, -ratio), nrow = 8) +
  labs(title = "settlement patterns", subtitle = "PERCENT COASTAL") +
  theme_map() +
  ggsave("test.png", height = 10, width = 10, dpi = 300)

##

library(scales)

##

spatial_data %>%
  left_join(st_drop_geometry(coastline)) %>%
  group_by(entity) %>%
  summarise(coastline = sum(as.numeric(coastline), na.rm = TRUE),
            population = sum(coastal_population, na.rm = TRUE)) %>%
  top_n(10, coastline) %>%
  ggplot(aes(x = reorder(entity, coastline), y = coastline / 1000)) +
  geom_bar(aes(fill = population / 1000), position = 'stack', stat = 'identity',
           colour = 'grey50', size = 0.1, linetype = 2) +
  scale_fill_gradientn(colours = rev(pal),
                       guide = guide_continuous,
                       name = "coastal population",
                       limits = c(0, 200000),
                       breaks = c(50000, 100000, 150000),
                       labels = c("50m", "100m", "150m"), 
                       oob = squish) +
  labs(title = "a tale of ten countries", subtitle = "COASTS AND COASTAL POPULATION",
       x = "entity", y = "coastline (km)") +
  coord_flip() +
  theme_rot() +
  ggsave("test2.png", height = 8, width = 8, dpi = 300)

##

spatial_data %>%
  left_join(st_drop_geometry(coastline)) %>%
  mutate(bylength = mismanaged_plastic_waste_tonnes / (as.numeric(coastline) / 1000),
         bypopulation = mismanaged_plastic_waste_tonnes / coastal_population) %>%
  select(entity, name, region, continent, mismanaged_plastic_waste_tonnes, 
         bylength, bypopulation, geometry) %>%
  drop_na() %>% 
  mutate(length = rank(rescale(bylength, to = c(100, 0))),
         population = rank(rescale(bypopulation, to = c(100, 0)))) %>%
  mutate(selector = population) %>%
  filter(selector < 50) %>%
  gather(variable, rank, length:population) %>%
  ggplot(aes(x = rank, y = reorder(entity, -selector))) +
  geom_line(aes(group = entity), colour = 'grey50', size = 1) +
  geom_point(aes(colour = variable), shape = '|', size = 5) +
  scale_colour_manual(values = c(pal[1], pal[8]),
                      name = "mass of stray plastic by\n(denominator)",
                      labels = c("/ coastline", "/ population")) +
  labs(title = "weighted and ranked wastefulness", subtitle = "THEORIES OF RELATIVITY",
       x = "rank", y = "entity") +
  theme_ver() +
  ggsave("test3.png", height = 10, width = 10, dpi = 300)

##

rank <-
  spatial_data %>%
  filter(year == 2010) %>%
  filter(entity != "World" & !is.na(continent)) %>% 
  group_by(continent) %>%
  summarise(population = sum(mismanaged_plastic_waste_tonnes, na.rm = TRUE)) %>%
  mutate(rank = rank(population)) %>%
  select(-population)

spatial_data %>%
  st_drop_geometry() %>%
  filter(year == 2010) %>%
  filter(entity != "World" & !is.na(continent)) %>% 
  group_by(continent, entity) %>%
  summarise(total = sum(mismanaged_plastic_waste_tonnes, na.rm = TRUE),
            population = sum(coastal_population, na.rm = TRUE)) %>%
  left_join(rank) %>%
  ggplot(aes(x = reorder(continent, rank), y = total / 1000)) +
  geom_bar(aes(fill = population / 1000, group = continent), position = 'stack', stat = 'identity',
           colour = 'grey50', size = 0.05, linetype = 2) +
  scale_fill_gradientn(colours = rev(pal),
                       guide = guide_continuous,
                       name = "coastal population",
                       limits = c(0, 200000),
                       breaks = c(50000, 100000, 150000),
                       labels = c("50m", "100m", "150m"), 
                       oob = squish) +
  labs(title = "a tale of six continents", subtitle = "LITTER BY CONTINENT",
       x = "plastic waste (kilotonnes)", y = "continent") +
  coord_flip() +
  theme_rot() +
  ggsave("test4.png", height = 8, width = 8, dpi = 300)

spatial_data %>%
  filter(year == 2010) %>%
  filter(entity != "World") %>%
  filter(str_detect(region, "Asia")) %>%
  group_by(region) %>%
  top_n(5, mismanaged_plastic_waste_tonnes) %>%
  ggplot(aes(x = reorder(entity, mismanaged_plastic_waste_tonnes), y = mismanaged_plastic_waste_tonnes / 1000)) +
  geom_bar(aes(fill = coastal_population / 1000), stat = 'identity',
           colour = 'grey50', size = 0.05, linetype = 2) +
  scale_fill_gradientn(colours = rev(pal),
                       guide = guide_continuous,
                       name = "coastal population",
                       limits = c(0, 200000),
                       breaks = c(50000, 100000, 150000),
                       labels = c("50m", "100m", "150m"), 
                       oob = squish) +
  facet_wrap(~ region, scales = 'free_y') +
  labs(title = "a tale of four subcontinents", subtitle = "LITTER BY REGION", 
       x = "plastic waste (kilotonnes)", y = "country") +
  coord_flip() +
  theme_rot() +
  theme(legend.position = c(0.3, 0.2)) +
  ggsave("test5.png", height = 8, width = 8, dpi = 300)

##

library(cartogram)

##

gram_length <- cartogram_cont(map_data, "bylengthtile", itermax = 500, prepare = 'adjust')
gram_population <- cartogram_cont(map_data, "bypopulationtile", itermax = 500, prepare = 'adjust')

##

gram_start <- st_normalize(sf::st_combine(gram_length))
gram_start <- sf::st_sf(geometry = sf::st_sfc(gram_start))

gram_end <- st_normalize(sf::st_combine(gram_population))
gram_end <-  sf::st_sf(geometry = sf::st_sfc(gram_end))

##

library(transformr)
library(tweenr)
library(ggplot2)

##

morph <- tween_sf(gram_start, gram_end,
                  ease = 'cubic-in-out',
                  nframes = 50)

##

morph <-
  morph %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>%
  mutate(area = st_area(geometry)) %>%
  group_by(.frame) %>%
  mutate(ntile = ntile(area, 100)) %>%
  st_cast("MULTIPOLYGON")

##

library(gganimate)

##

animate <-
  ggplot(morph) + 
  geom_sf(aes(geometry = geometry, fill = ntile), colour = 'white', size = .1, show.legend = FALSE) + 
  coord_sf(datum = 54030) + 
  scale_fill_gradientn(colours = rev(pal)) +
  labs(title = "coast against coastal population", subtitle = "THEORIES OF RELATIVITY",
       caption = "FRAME 1: waste / coastline     FRAME 2: waste / population\ncartogram distorting each country according to how much plastic waste it produces by the length of its coasts or the number of people living on said coasts") +
  transition_manual(.frame) +
  ease_aes('cubic-in-out') + 
  theme_map()

anim_save("cartogram.gif", animation = animate, 
          height = 600, width = 800, nframes = 100, fps = 5,
          start_pause = 2, end_pause = 2)

