library(tidyverse)

##

read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv') %>%
  group_by(country) %>%
  mutate(total = sum(co2_emmission)) %>%
  ungroup() %>%
  mutate(rank = rank(total)) %>%
  ggplot(aes(x = reorder(country, rank), y = co2_emmission, fill = food_category)) +
  geom_bar(position = 'stack', stat = 'identity', colour = '#ffffff', size = 0.05) +
  coord_flip() +
  theme_minimal() +
  ggsave("test.png", height = 12, width = 8, dpi = 300)

##

library(rnaturalearth)
library(sf)
library(units)

##

food <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

##

food %>% pull(country) %>% unique() %>% sort()

##

countries <- 
  ne_countries(scale = 50, returnclass = 'sf') %>%
  st_transform(54030) %>%
  mutate(area = st_area(geometry)) %>%
  mutate(area = set_units(area, km^2)) %>%
  transmute(area = area, 
           country = name_en, 
           population = pop_est, 
           income = gdp_md_est)

##

joined <- 
  food %>%
  mutate(country = 
           recode(country, 
                  "USA" = "United States of America",
                  "Bahamas" = "The Bahamas",
                  "Macedonia" = "Republic of Macedonia", 
                  "Taiwan. ROC" = "Taiwan", 
                  "China" = "People's Republic of China",
                  "Congo" = "Republic of the Congo", 
                  "Gambia" = "The Gambia")) %>%
  left_join(countries) %>%
  mutate(area = area,
         population = as.numeric(population),
         income = income / 1000) %>%
  drop_na(area)

##

long <- 
  bind_rows(joined %>% 
              transmute(country = country,
                        food = food_category, 
                        co2_emmission = co2_emmission,
                        rate = "unadjusted (annual personal emisions)") %>%
              group_by(country) %>%
              mutate(total = sum(co2_emmission)) %>%
              ungroup() %>%
              group_by(food) %>%
              mutate(rank = rank(total)) %>%
              ungroup(),
            joined %>% 
              transmute(country = country,
                        food = food_category, 
                        co2_emmission = co2_emmission / (area / population),
                        rate = "adjusted by land per person (km / population)") %>%
              group_by(country) %>%
              mutate(total = sum(co2_emmission)) %>%
              ungroup() %>%
              group_by(food) %>%
              mutate(rank = rank(total)) %>%
              ungroup(),
            joined %>% 
              transmute(country = country,
                        food = food_category, 
                        co2_emmission = co2_emmission / income,
                        rate = "adjusted by income (per $1000)") %>%
              group_by(country) %>%
              mutate(total = sum(co2_emmission)) %>%
              ungroup() %>%
              group_by(food) %>%
              mutate(rank = rank(total)) %>%
              ungroup()) %>%
  group_by(rate) %>%
  mutate(max = max(co2_emmission)) %>%
  ungroup()

##

library(gganimate)

##

theme_hor <- function () {
  theme_minimal() +
    theme(plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size = 0.1, colour = 'grey50'),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.x = element_line(size = 0.5, colour = 'black'),
          axis.line.y = element_blank(),
          axis.ticks.x = element_line(size = 0.5, colour = 'black'),
          axis.ticks.y = element_line(size = 0.1, colour = 'grey50'),
          axis.text.x = element_text(face = 'bold'),
          axis.text.y = element_text(face = 'bold'),
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'black', size = 15),
          strip.text = element_text(face = 'bold', colour = 'black'),
          plot.margin = margin(20, 20, 20, 20),
          legend.position = c(0.3, 0.6)
    )
}

options(scipen = 999)

##

selection <- c("United States of America", "Russia", "Luxembourg", "Argentina", "Brazil", "France", "United Kingdom")

anim <- 
  ggplot(long, aes(x = rank, y = co2_emmission, fill = food)) +
  geom_bar(position = 'stack', stat = 'identity', colour = '#ffffff', size = 0.05) +
  geom_text(data = long %>%
              filter(country %in% selection) %>%
              group_by(country, rate) %>%
              slice(1),
              aes(x = rank, y = total, label = country),
            check_overlap = TRUE) +
  theme_hor() +
  ylab("emissions, kg per person per year") +
  xlab("country") +
  transition_states(rate) +
  ggtitle("{closest_state}") +
  ease_aes('cubic-in-out') +
  view_follow()

##

animate(anim, height = 600, width = 1000)
anim_save(filename = "test.gif", animation = last_animation())