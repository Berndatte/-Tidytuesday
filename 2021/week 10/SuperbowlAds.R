#TIDYTUESDAY WEEK 10
#SuperBowl Ads
#BERNDATTE

#loading packages
library(tidyverse)
library(lubridate)
library(extrafont)
install.packages('moderndive')
library(moderndive)
#Getting Data
youtube <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv')


youtube_1 <- youtube %>%
  select(year,brand,funny, show_product_quickly,patriotic,celebrity, animals, like_count,dislike_count,view_count,comment_count)%>%
  filter(year >= '2010')%>%
  filter(!is.na(view_count))
  

ggplot(youtube_1, aes(brand, log(view_count))) +
  geom_bar(stat = 'identity', fill = 'darkblue') +
  facet_wrap(.~ year) +
  coord_flip() +
  labs(title = 'The view Count per Brand Over Time', x = 'BRAND', caption = 'Berndatte \n source:FiveThirtyEight')+
  theme_bw() +
  theme(axis.text = element_text(color = 'blueviolet', size = 16, family = 'Verdana'),
        axis.title = element_text(color = 'brown4', size = 20, family = 'Bodoni MT', face = 'bold'),
        strip.text = element_text(color = 'dodgerblue4', size = 18, family = 'Courier New'),
        plot.title = element_text(color = 'firebrick4', family = 'Verdana', size = 24, face = 'bold'),
        plot.caption = element_text(family = 'Segoe Script', size = 16, face = 'bold' ))


ggsave(filename = 'SuperowlAds.png', plot = last_plot(),width = 450, height = 300, units = 'mm',
       path = '2021/week 10')

        