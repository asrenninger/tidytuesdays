library(tidyverse)
library(ggplot2)

################################ LIGHT ################################ 
################################ 
## Horizontal Emphasis
## White

theme_hor <- function () {
  theme_minimal() +
    theme(plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size=0.1, color='grey50'),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.x = element_line(size=0.5, color='black'),
          axis.line.y = element_blank(),
          axis.ticks.x = element_line(size=0.5, color='black'),
          axis.ticks.y = element_line(size=0.1, color='grey50'),
          axis.text.x = element_text(face = 'bold'),
          axis.text.y = element_text(face = 'bold'),
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'black', size = 15),
          plot.margin = margin(20, 20, 20, 20),
          legend.position = 'none'
          )
}

## Black

theme_hor <- function () {
  theme_minimal() +
    theme(plot.background = element_rect(fill = 'white', colour = 'white'),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size=0.1, color='grey50'),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.x = element_line(size=0.5, color='black'),
          axis.line.y = element_blank(),
          axis.ticks.x = element_line(size=0.5, color='black'),
          axis.ticks.y = element_line(size=0.1, color='grey50'),
          axis.text.x = element_text(face = 'bold'),
          axis.text.y = element_text(face = 'bold'),
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'black', size = 15),
          plot.margin = margin(20, 20, 20, 20),
          legend.position = 'none'
          )
}

## Flipped Horizontal

theme_rot <- function () {
  theme_minimal() +
    theme(plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
          panel.grid.major.x = element_line(size=0.1, color='grey50'),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.y = element_line(size = 0.5, color='black'),
          axis.line.x = element_blank(),
          axis.ticks.y = element_line(size = 0.5, color='black'),
          axis.ticks.x = element_line(size = 0.1, color='grey50'),
          axis.text.x = element_text(face = 'bold'),
          axis.text.y = element_text(face = 'bold'),
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'black'),
          plot.margin = margin(20, 20, 20, 20), 
          legend.position = c(0.6, 0.2)
          )
}

################################
## Vertical Emphasis
## White

theme_ver <- function () {
  theme_minimal() +
    theme(plot.background = element_rect(fill = 'white', colour = 'white'),
          panel.grid.major.x = element_line(size=0.1, color='grey50'),
          panel.grid.major.y = element_line(size=0.1, color='grey50'),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.y = element_line(size = 0.5, color='black'),
          axis.line.x = element_blank(),
          axis.ticks.y = element_line(size = 0.5, color='black'),
          axis.ticks.x = element_line(size = 0.1, color='grey50'),
          axis.text.x = element_text(face = 'bold'),
          axis.text.y = element_text(face = 'bold'),
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'black'),
          plot.margin = margin(20, 20, 20, 20), 
          legend.position = c(.8, .4)
          )
}

## Black

theme_ver_black <- function () {
  theme_minimal() +
    theme(plot.background = element_rect(fill = 'black', colour = 'black'),
          panel.grid.major.x = element_line(size=0.1, color='white'),
          panel.grid.major.y = element_line(size=0.1, color='white'),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.y = element_line(size = 0.5, color='black'),
          axis.line.x = element_blank(),
          axis.ticks.y = element_line(size = 0.5, color='black'),
          axis.ticks.x = element_line(size = 0.1, color='white'),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_text(face = 'bold', color = 'white'),
          axis.title.y = element_text(face = 'bold', color = 'white'),
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'white'),
          plot.margin = margin(20, 20, 20, 20), 
          legend.position = 'none'
          )
}

################################
## Map Theme
## White

theme_map <- function () {
  theme_void() + 
    theme(plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
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
          plot.caption = element_text(face = 'bold', colour = 'black'),
          plot.margin = margin(5, 5, 5, 5),
          strip.text = element_text(face = 'bold', colour = 'black'),
          panel.grid.major = element_line(size = NA), 
          panel.grid.minor = element_line(size = NA),
          legend.position = 'none'
          )
  
}

## Legend

theme_map_legend <- function () {
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
          panel.grid.major = element_line(size = NA), 
          panel.grid.minor = element_line(size = NA),
          legend.position = 'bottom'
          )
  
}

################################ DARK ################################ 
################################ 
## Horizontal

theme_bh <- function () {
  theme_minimal() +
    theme(plot.background = element_rect(fill = 'black', colour = 'black'),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size = 0.1, color='grey50'),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.x = element_line(size = 0.5, color = 'white'),
          axis.line.y = element_blank(),
          axis.ticks.x = element_line(size = 0.5, color = 'white'),
          axis.ticks.y = element_line(size = 0.1, color = 'grey50'),
          axis.text.x = element_text(face = 'bold'),
          axis.text.y = element_text(face = 'bold'),
          axis.title.x = element_text(colour = 'white'),
          axis.title.y = element_text(colour = 'white'),
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'white', size = 15),
          plot.margin = margin(20, 20, 20, 20),
          legend.position = 'none'
    )
}

## Rotated

## Verticle

## Map (No Legend)

theme_bm <- function () {
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
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'white', size = 15),
          plot.margin = margin(20, 20, 20, 20),
          panel.grid.major = element_line(size = NA), 
          panel.grid.minor = element_line(size = NA),
          legend.position = 'none'
    )
  
}

## Map (Legend)

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
          legend.title = element_text(colour = 'grey50', angle = 270),
          legend.text = element_text(colour = 'white', angle = 270),
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'white', size = 15),
          panel.grid.major = element_line(size = NA), 
          panel.grid.minor = element_line(size = NA),
          legend.position = c(0.8, 0.3),
          plot.margin = margin(20, 20, 20, 20)
    )
  
}

## Graph

theme_graph <- function () {
  theme_void() + 
    theme(plot.background = element_rect(fill = 'black', colour = 'black'),
          panel.background = element_rect(fill = 'black', colour = 'black'),
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
          legend.text = element_text(colour = 'white'),
          legend.title = element_text(colour = 'white'),
          plot.title = element_text(face = 'bold', colour = '#A4A4A4'),
          plot.subtitle =  element_text(face = 'plain', colour = 'white', size = 15),
          plot.caption = element_text(colour = '#A4A4A4', size = 8),
          plot.margin = margin(20, 20, 20, 20),
          panel.grid.major = element_line(size = NA), 
          panel.grid.minor = element_line(size = NA),
          legend.position = 'none'
          )
  
}

################################ Legends ################################
################################ 
## Continuous

guide_continuous <- 
  guide_colorbar(direction = "vertical",
                 barheight = unit(50, units = "mm"),
                 barwidth = unit(2, units = "mm"),
                 draw.ulim = FALSE,
                 title.position = 'right',
                 label.position = 'left',
                 title.hjust = 0.5,
                 label.hjust = 0.5)

## Discrete

guide_discrete <-
  guide_legend(direction = "vertical",
               keywidth = unit(1, units = "mm"),
               keyheight = unit(100 / length(lab), units = "mm"),
               title.position = 'right',
               label.position = 'left',
               title.hjust = 0.5,
               label.hjust = 1,
               ncol = 1,
               bycol = TRUE)

################################ Colours ################################
## Space Syntax

pal <- c('#00007f', '#0000fe', '#0160ff', '#01d0ff', '#49ffad', 
         '#a4ff53', '#fbec00', '#ff8500', '#ff1e00', '#7f0000')


colorRampPalette(colors = 
                   c('#00007f', '#0000fe', '#0160ff', '#01d0ff', '#49ffad', 
                     '#a4ff53', '#fbec00', '#ff8500', '#ff1e00', '#7f0000'))

## With White

colorRampPalette(colors = c('#F05154', '#FAFBFB', '#62ACC9'))

colfunc <- colorRampPalette(c('#F05154', '#FAFBFB', '#62ACC9'))
colfunc(10)

## With Purple

colfunc <- colorRampPalette(c('#F05154', '#62ACC9', '#7B6576'))
colfunc(5)

## Palettte

pal <- c("Clinton" = "#62ACC9", "Trump" = "#F05154", "Stein" = "#67B1B8", "Johnson" = "#7B6576", "Other" = "#555655")

  