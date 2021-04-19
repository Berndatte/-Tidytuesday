#TIDYTUESDAY WEEK 14
#MAKEUP SHADES
#BERNDATTE

#LIBRARIES
library(tidyverse)
library(lubridate)
library(extrafont)

#GETTING DATA
sephora <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/sephora.csv')
ulta <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/ulta.csv')
allCategories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allCategories.csv')
allShades <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allShades.csv')
allNumbers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allNumbers.csv')


#WRANGLE DATA
shades <- allShades %>%
  select(brand,lightness, hex, hue)%>%
  filter(hue <150)
  
source = 'Source : The Pudding \n Berndatte'
  


#VISUALIZE
shades %>%
ggplot(aes(hue, lightness, color = hex)) +
  geom_point(alpha = .5, size = 2.5) +
  labs(x = 'Hue',
       y = 'Lightness',
       title = 'Relationship between Hue and Lightness of Makeup',
       caption = source ) +
  theme_classic() +
  theme(legend.position = 'none',
        plot.title = element_text(
          family = 'Tahoma', 
          size = 25, 
          color = '#370617', 
          face = 'bold'),
        axis.title = element_text(
          family = 'Tempus Sans ITC',
          size = 20, face = 'bold'),
        axis.text = element_text(
          family = 'serif',
          face = 'bold',
          size = 16),
        plot.caption = element_text(
          color = '#4cc9f0',
          size = 16,
          face = 'bold'))


#SAVE
ggsave(filename = 'week_14.png',
       height = 300,
       width = 450,
       units = 'mm',
       plot = last_plot(),
       path = '2021/week 14/')       
  
  

