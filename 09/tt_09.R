
library(tidyverse)

##

rome_peeps <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv")

##

library(sf)

##

rome_shape <- st_read("09/pplaces_out.shp")

## loose

rome_shape %>%
  filter(str_detect(TITLE, paste(rome_peeps$birth_cty, collapse = "|"))) %>%
  ggplot(aes(REPRLONG, REPRLAT)) +
  geom_point(data = rome_shape,
             aes(REPRLONG, REPRLAT), alpha = 0.25) +
  geom_point(aes(colour = GEOCONTEXT), show.legend = FALSE)

## tight

rome_shape %>%
  filter(TITLE %in% rome_peeps$birth_cty | GEOCONTEXT %in% rome_peeps$birth_cty) %>%
  drop_na(GEOCONTEXT) %>%
  ggplot(aes(REPRLONG, REPRLAT)) +
  geom_point(data = rome_shape,
             aes(REPRLONG, REPRLAT), alpha = 0.25) +
  geom_point(aes(colour = GEOCONTEXT), show.legend = FALSE)

## remaining

remainder <- 
  rome_peeps %>%
  filter(!birth_cty %in% rome_shape$GEOCONTEXT & !birth_cty %in% rome_shape$TITLE) %>%
  distinct(birth_cty)

## blend

library(stringdist)

##

narrow <- 
  rome_shape %>%
  mutate(match_wide = str_extract(GEOCONTEXT, paste(rome_peeps$birth_cty, collapse = "|")),
         match_narrow = str_extract(TITLE, paste(rome_peeps$birth_cty, collapse = "|"))) %>%
  filter(match_wide != "<NA>" | match_narrow != "<NA>") %>%
  mutate(similarity_wide = stringdist(GEOCONTEXT,  match_wide),
         similarity_narrow = stringdist(TITLE,  match_narrow)) %>% 
  mutate(match = case_when(is.na(match_wide) ~ match_narrow,
                           TRUE ~ match_wide),
         similarity = case_when(is.na(match_wide) ~ similarity_narrow,
                                TRUE ~ similarity_wide)) %>%
  group_by(match) %>%
  filter(similarity == min(similarity)) %>%
  select(GEOCONTEXT, TITLE, REPRLAT, REPRLONG, match, similarity)

## how many

sum(str_detect(unique(rome_shape$GEOCONTEXT), paste(remainder$birth_cty, collapse = "|")), na.rm = TRUE)

## which ones

remainder <-
  rome_peeps %>%
  filter(!birth_cty %in% narrow$match) %>%
  distinct(birth_cty) %>%
  pull()

## grasping at straws

wide <- 
  rome_shape %>%
  filter(str_detect(GEOCONTEXT, "Gamzigrad") | str_detect(TITLE, "Arca") | str_detect(TITLE, "Lepti Minus")) %>%
  filter(str_detect(GEOCONTEXT, "Gamzigrad") | str_detect(TITLE, "Caesarea") | str_detect(TITLE, "Lepti Minus")) %>%
  mutate(match = "manual",
         similarity = 0) %>%
  select(GEOCONTEXT, TITLE, REPRLAT, REPRLONG,  match, similarity) %>%
  mutate(match = str_extract(TITLE, "Romuliana|Arca|Lepti Minus")) %>%
  mutate(match = str_replace_all(match, "Romuliana", "Felix Romuliana")) %>%
  mutate(match = str_replace_all(match, "Arca", "Arca Caesarea")) %>%
  mutate(match = str_replace_all(match, "Lepti Minus", "Leptis Magna")) %>%
  rename(lat = REPRLAT,
         lon = REPRLONG) %>%
  select(match, lon, lat)

## combined

combined <- 
  narrow %>%
  filter(TITLE != "Alba") %>%
  filter(GEOCONTEXT != "Salona") %>%
  group_by(match) %>%
  summarise(lat = mean(REPRLAT),
            lon = mean(REPRLONG)) %>%
  rbind(wide)

joined <-
  combined %>%
  mutate(birth_cty = match) %>%
  right_join(rome_peeps) %>%
  group_by(birth_cty) %>%
  summarise(lat = median(lat),
            lon = median(lon),
            n = n())

## plotting

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
          legend.position = c(0.8, 0.8)
    )
  
}

##

library(ggrepel)

##

coast <- st_read("09/coastline.shp")
empire <- st_read("09/roman_empire_ad_200_extent.shp")

##

ggplot() +
  geom_sf(data = empire,
          aes(), fill = 'red4', colour = NA) +
  geom_sf(data = coast,
          aes(), colour = 'navyblue', size = 0.1) +
  geom_point(data = rome_shape,
             aes(REPRLONG, REPRLAT), colour = 'grey70', alpha = 0.25, size = 0.1) +
  geom_text_repel(data = joined,
                  aes(lon, lat, label = birth_cty),
                  fontface = 'bold', colour = 'black',
                  segment.alpha = 0.5,
                  force = 1) + 
  geom_point(data = joined,
             aes(lon, lat, colour = n, size = n)) +
  scale_color_gradientn(colors = c('gold', 'gold4')) +
  guides(color = guide_legend(), size = guide_legend()) +
  coord_sf(xlim = c(-12.50000,  93.33042),
           ylim = c(-7.50000, 62.50000)) +
  labs(title = "places of birth for roman emporers",
       subtitle = "WHICH ROADS LEAD TO ROME?",
       caption = "each point represents a site from antiquity") +
  theme_map() +
  ggsave("hometowns.png", height = 8, width = 10, dpi = 300)

