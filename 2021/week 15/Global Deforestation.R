#TIDYTUESDAY WEEK 15
#GLOBAL DEFORESTATION
#BERNDATTE

#LIBRARIES
library(tidyverse)
library(lubridate)
library(extrafont)
library(scales)


#READING DATA
forest <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/forest.csv')
forest_area <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/forest_area.csv')
brazil_loss <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/brazil_loss.csv')
soybean_use <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/soybean_use.csv')
vegetable_oil <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/vegetable_oil.csv')


#Wrangle Data
Brazil_loss <-brazil_loss %>%
  gather(type,loss,c(4:14))%>%
 filter(type %in% c('pasture','small_scale_clearing', 'commercial_crops', 'fire', 'selective_logging'))%>%
  mutate(loss = loss/1000000)

total <- Brazil_loss %>%
  group_by(type) %>%
  summarise(total_n = sum(loss)) %>%
  arrange(desc(total_n))
  
  
#Visualize
ggplot(Brazil_loss,aes(year, loss,col = type)) +
  geom_line(size = 2)+
  labs(title = 'Loss of Brazilian Forest over time',
       x = 'Year', y = 'Loss in million hectares',
       caption = 'Berndatte \n Our World in Data', col = 'Type')+
  scale_x_continuous(breaks = seq(2001,2013, 2),
                     limits = c(2001, 2013)
                     ) +
  guides(col = guide_legend(reverse = TRUE)
         )+
  theme(panel.background = element_rect(fill = '#000000'
                                        ),
        panel.grid.minor  = element_blank(
        ),
        panel.grid.major = element_line(color = 'grey'
                                        ),
        plot.title = element_text(
          color = '#023047',
          size = 24,
          family = 'Algerian',
          face = 'bold'
          ),
        axis.title = element_text(
          color = '#9d0208',
          size = 20,
          family = 'Ebrima',
          face = 'italic'
          ),
        legend.title = element_text(
          color = '#4361ee',
          size = 20, 
          family = 'serif'
          ),
        legend.text = element_text(
          color = '#118ab2',
          size = 20,
          face = 'bold',
          family = 'mono'
          ),
        plot.caption = element_text(
          color = '#f77f00',
          size = 20,
          family = 'Arial Narrow'
          ),
        axis.text = element_text(
          color = '#5f0f40',
          size = 18,
          family = 'mono'
          ),
        legend.key  = element_rect(
          fill = '#000000')
        )
 #Save      
ggsave(filename = 'Deforestation.png',
       height = 300,
       width = 450,
       units = 'mm',
       plot = last_plot(),
       path = '2021/week 15/'
       )
  
  





