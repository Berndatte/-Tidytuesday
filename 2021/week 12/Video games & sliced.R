#TIDYTUESDAY WEEK 12
#VIDEO GAMES + SLICED
#BERNDATTE

#LOADING PACKAGES
library(tidyverse)
library(extrafont)

#Getting Data
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')


year_2020 <- games %>%
  filter(year == '2020')%>%
  filter(gamename %in% c('Counter-Strike: Global Offensive', 'Dota 2', "PLAYERUNKNOWN'S BATTLEGROUNDS")) %>%
  mutate(gain_per_100 = gain/100)

ggplot(year_2020, aes(month, gain_per_100)) + 
  geom_bar(stat = 'identity') +
  facet_wrap(~gamename) +
  coord_flip() + 
  labs(title = 'Gain Difference compared to previous Month in 2020', x = 'Month', y = 'Gain/100',
       caption = 'Berndatte \n Data:Steam')+
  scale_x_discrete(limits = c('December',
                              'November',
                              'October',
                              'September',
                              'August',
                              'July',
                              'June',
                              'May',
                              'April',
                              'March',
                              'February',
                              'January')) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18),
        plot.title = element_text(family = 'Verdana', face = 'bold', size = 28, color = 'blue1'),
        axis.title = element_text(family = 'Pristina', face = 'bold', size = 24, color = 'coral4'),
        axis.text.y = element_text(family = 'serif', color = 'cadetblue4', size = 16, face = 'bold'),
        strip.text = element_text(family = 'mono', color = 'blueviolet', size = 18, face = 'bold'),
        plot.caption = element_text(family = 'Rage Italic', size =24, color = 'darkslateblue'))

ggsave(filename = 'Videogames.png', height = 300, width = 450, units = 'mm',plot = last_plot(), path = '2021/week 12/')
