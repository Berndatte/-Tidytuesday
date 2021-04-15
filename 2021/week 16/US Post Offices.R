#TIDYTUESDAY WEEK 16
#US POST OFFICES
#BERNDATTE

#LOADING PACKAGES
library(tidyverse)
library(lubridate)
library(extrafont)
library(scales)

#READING DATA
post_offices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-13/post_offices.csv')

source <- 'Source: Cameron Blevins and Richard W.Helbock\n visualizer : Berndatte'
#GLIMPSE DATA
summary(post_offices)

glimpse(post_offices)


#DATA WRANGLE
post <- post_offices %>%
  select(state, established, discontinued) %>%
  gather(Title, year, 2:3) %>%
  filter(year >= 1800 & year < 2000)%>%
  group_by(Title, year)%>%
  summarize(count = n())


#VISUALIZE

ggplot(post, aes(year,count, color = Title)) +
  geom_point(size  = 2.5)+
  labs(title = 'Total Number of USA Post Office ',
       subtitle = 'Established and Discontinued - since 1800',
       caption = source,
       x = 'Total', y = 'Year') +
  guides(col = guide_legend(reverse = TRUE))+
  scale_color_discrete(name = "", labels = c('Discontinued', 'Established'))+
  theme(axis.line = element_line(color = "#3A2115"),
        axis.ticks.x =  element_line(color = "#3A2115"), 
        axis.text= element_text(color = "#3A2115", 
                           face = "bold", size = 18),
        plot.caption = element_text(color = "#3A2115", 
                            face = "bold.italic", size = 16),
        plot.title = element_text(color = '#283618', size = 26, family = 'Elephant', face = 'bold'),
        plot.subtitle = element_text(color = '#370617', size = 24, family = 'Colonna MT', face = 'bold'),
        axis.title = element_text(color = '#219ebc', size = 24, family = 'mono', face = 'bold',
                                  hjust = .5),
        legend.text = element_text(color = '#003049', size = 18, family = 'mono', face = 'bold'))



#SAVE
ggsave(filename = 'week_16.png', plot = last_plot(), height = 300, width = 450, units = 'mm', path = '2021/week 16/' )
