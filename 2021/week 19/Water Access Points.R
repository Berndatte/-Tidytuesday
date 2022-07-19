#TIDYTUESDAY WEEK 19
#WATER ACCESS POINTS
#BERNDATTE

#LOADING PACKAGES
library(tidyverse)
library(lubridate)
library(extrafont)


#READING DATA
water <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-04/water.csv')
 kenya <- water %>%
   filter(country_name == 'Kenya' ) %>%
   filter(install_year>=1980)
 
 source <- 'Source: Water Point Data Exchange\n Berndatte'
 text <- 'water_source\ndata missing'

  ggplot(kenya, aes(install_year, fill = water_source))+
  geom_bar(show.legend = F)+
  facet_wrap(~water_source)+
    labs(title = 'WATER ACCESS POINTS IN KENYA',
         subtitle = 'Number of water access points from 1980-2020',
         x = 'Installation Year',
         y = 'Total',
         caption = source)+
    geom_text(x = 300, y = 50,label = text)+
    theme_bw()+
    theme(plot.title = element_text(family = 'Berlin Sans FB Demi', size = 24, color = '#023e8a', face = 'bold'),
          plot.subtitle = element_text(family = 'Britannic Bold', size = 20,color = '#00b4d8', face = 'bold'),
          strip.background = element_rect(fill = '#03071e'),
          strip.text = element_text(color = '#e5e5e5', face = 'bold', size = 14),
          axis.text = element_text(color = '#184e77', family = 'serif', face = 'bold', size = 18),
          plot.caption = element_text(color = '#4ea8de', family = 'Verdana', face = 'bold', size = 16),
          axis.title = element_text(color = '#00bbf9', family = 'serif', face = 'bold', size = 20))


  ggsave(filename = 'water.png', plot = last_plot(), width = 450, height = 300, units = 'mm', path = '2021/week 19/')

         