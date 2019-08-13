library(tidyverse)

##

ramen_ratings <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")

##

library(scico)

##

guide <- 
  guide_legend(direction = "horizontal",
               keywidth = unit(10, units = "mm"),
               keyheight = unit(2, units = "mm"),
               title.position = 'top',
               label.position = 'right',
               title.hjust = 0.5,
               label.hjust = 1,
               nrow = 1,
               byrow = TRUE)

##

pal <- sample(scico(palette = 'tokyo', 8))

##

ramen_ratings %>% 
  mutate(country = if_else(country == "USA", "United States", country)) %>%
  group_by(country) %>%
  add_tally() %>%
  ungroup() %>%
  group_by(country, n, style) %>%
  summarise(count = n()) %>%
  mutate(ratio = count / n) %>%
  filter(n > 8) %>%
  ggplot(aes(x = "", y = ratio)) +
  geom_bar(aes(fill = style), width = 1, position = 'fill', stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = pal,
                    na.translate = FALSE,
                    guide = guide) +
  facet_wrap(~ reorder(country, -n)) +
  labs(title = "cupped, packed, boxed or bowled?", subtitle = "RATIO OF RAMEN STYLES BY COUNTRY",
       caption = "(ordered by the number of varieties)") +
  theme_map() +
  theme(legend.position = 'bottom',
        legend.title = element_text(colour = 'grey50', face = 'bold'),
        legend.text = element_text(colour = 'black')) +
  ggsave("test.png", height = 10, width = 10, dpi = 300) 

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

ramen_map <-
  ramen_ratings %>% 
  mutate(country = case_when(country == "United States" ~ "United States of America",
                             country == "USA" ~ "United States of America",
                             country == "UK" ~ "United Kingdom",
                             country == "Hong Kong" ~ "China",
                             TRUE ~ country)) %>%
  group_by(country) %>%
  add_tally() %>%
  ungroup() %>%
  group_by(country, n, style) %>%
  summarise(count = n()) %>%
  mutate(ratio = count / n) %>%
  mutate(name = country) %>%
  right_join(countries) %>%
  st_as_sf() %>%
  select(name, country, everything()) %>%
  replace_na(list(n = 0.5))

##

library(cartogram)
library(ggrepel)

##

gram <- 
  ramen_map %>%
  filter(count == max(count)) %>%
  slice(1) %>%
  cartogram_dorling("n", itermax = 100)

ggplot(gram) +
  geom_sf(data = countries, 
          aes(), fill = NA, colour = 'black', linetype = 3) +
  geom_sf(aes(fill = style, colour = style)) +
  geom_text_repel(data = gram %>%
                    filter(n > 0.5) %>%
                    st_centroid() %>%
                    st_coordinates() %>%
                    as_tibble() %>%
                    bind_cols(gram),
                  aes(X, Y, label = name),
                  fontface = 'bold', colour = 'grey50') +
  scale_fill_manual(values = c("#9AC495", "#562456","#CAF3B4"),
                    na.translate = FALSE,
                    guide = guide) +
  scale_colour_manual(values = c("#9AC495", "#562456","#CAF3B4"),
                      na.translate = FALSE,
                      guide = guide) +
  labs(title = "most like it packed", subtitle = "MOST POPULAR RAMEN TYPE",
       caption = "(sized to the number of reviews in that country)") +
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
        legend.position = c(0.5, 0.2),
        legend.title = element_text(colour = 'grey50', face = 'bold'),
        legend.text = element_text(colour = 'black')) +
  ggsave("test2.png", height = 8, width = 12, dpi = 300) 
  
multigram <- 
  ramen_map %>%
  replace_na(list(count = 0.5)) %>%
  cartogram_dorling("count", k = 0.75, itermax = 100)

ggplot(multigram) +
  geom_sf(data = countries, 
          aes(), fill = NA, colour = 'grey50', linetype = 3) +
  geom_sf(aes(fill = country), show.legend = FALSE) +
  geom_text(data = multigram %>%
                    filter(n > 0.5) %>%
                    st_centroid() %>%
                    st_coordinates() %>%
                    as_tibble() %>%
                    bind_cols(filter(multigram, n > 0.5)),
                  aes(X, Y, label = style)) +
  theme_map()


ramens <- 
  ramen_ratings %>% 
  mutate(country = case_when(country == "United States" ~ "United States of America",
                             country == "USA" ~ "United States of America",
                             country == "UK" ~ "United Kingdom",
                             country == "Hong Kong" ~ "China",
                             TRUE ~ country)) %>%
  group_by(country, style, variety) %>%
  summarise(count = n(), 
            rating = mean(stars)) %>%
  ungroup() %>%
  group_by(country) %>%
  add_tally() %>%
  mutate(name = country) %>%
  right_join(countries) %>%
  st_as_sf() %>%
  select(name, country, everything()) %>%
  replace_na(list(n = 0, rating = 0)) %>%
  filter(n > 0) %>%
  ungroup()

ramen_nations <- 
  ramens %>%
  select(name, n) %>%
  group_by(name) %>%
  slice(1) %>%
  ungroup()

dots <- 
  ramen_nations %>%
  select(n) %>%
  st_drop_geometry()
  
sample <- 
  st_sample(ramen_nations, size = dots$n, type = "random", exact = TRUE) %>%
  st_sf() %>%
  bind_cols(st_drop_geometry(ramens)) %>%
  select(everything(), geometry) %>%
  st_as_sf()

ggplot() +
  geom_point(data = sample %>%
               st_coordinates() %>%
               as_tibble() %>%
               bind_cols(sample),
             aes(X, Y, size = ntile(rating, 100), colour = style), 
             alpha = 0.25) +
  geom_sf(data = countries, 
          aes(), fill = NA, colour = 'black', linetype = 3) +
  scale_colour_manual(values = pal,
                    na.translate = FALSE,
                    guide = guide) +
  scale_size_continuous(range = c(1, 6), 
                        guide = 'none') +
  labs(title = "dotting the nation", subtitle = "RAMENS AS POINTS",
       caption = "(sized to the number of stars for that variety)") +
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
        legend.position = c(0.5, 0.2),
        legend.title = element_text(colour = 'grey50', face = 'bold'),
        legend.text = element_text(colour = 'black')) +
  ggsave("test3.png", height = 8, width = 12, dpi = 300) 

multigram <- 
  sample %>%
  mutate(rating = ntile(rating, 100)) %>%
  cartogram_dorling("rating", itermax = 10)

##

ramen_ratings %>% 
  mutate(country = if_else(country == "USA", "United States", country)) %>%
  filter(str_detect(country, "China|Japan|Taiwan|Hong Kong|Thailand|Vietnam|South Korea|Malaysia|United States")) %>%
  ggplot() +
  geom_density(aes(stars, fill = style, colour = style), position = 'stack') +
  scale_fill_manual(values = pal[c(2, 3, 5, 6, 7, 8)],
                    na.translate = FALSE,
                    guide = guide) +
  scale_colour_manual(values = pal[c(2, 3, 5, 6, 7, 8)],
                      na.translate = FALSE,
                      guide = guide) +
  facet_wrap(~ country) +
  labs(title = "pacific theater", subtitle = "RAMEN RATINGS BY STYLE AND COUNTRY",
       caption = "(top 9 countries by selection)") +
  theme_hor() +
  theme(legend.position = 'bottom',
        legend.title = element_text(colour = 'grey50', face = 'bold'),
        legend.text = element_text(colour = 'black')) +
  ggsave("test4.png", height = 10, width = 10, dpi = 300) 
 


