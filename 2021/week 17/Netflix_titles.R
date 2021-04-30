#TIDYTUESDAY WEEK 17
#Netflix Titles
#BERNDATTE


library(tidyverse)
library(extrafont)
#GETTING DATA
netflix_titles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')



netflix <- netflix_titles %>%
  filter(release_year <= 2020)%>%
  group_by(type, release_year) %>%
  summarise(n = n())

source = 'Data:Kaggle\nBerndatte'

  
txt <- 'NETFLIX'

ggplot(netflix, aes(release_year,y = n, fill = type)) +
  geom_col(na.rm = T, position = 'dodge') +
  scale_x_continuous(
    breaks = seq(
      1925,2020,5),
    limits= c(
      1925, 2020)
    ) +
  labs(
    y = '', 
    x = '', 
    title = 'Number of Movies/TV Shows from 1925 to 2019',
    caption = source,
    fill = 'Type')+
  scale_fill_manual(
    values = c('red', '#ffc300'))+
  annotate('text',
           1933, 670,
           label = txt,
           color = 'red',
           size =20)+
          # hjust = -1)+
  theme_bw() +
  theme(plot.title = element_text(
    face = 'bold',
    size = 24,
    family = 'Algerian'
    ),
       legend.title = element_text(
         face='bold',
         size = 16,
         family = 'serif'
         ),
       legend.text = element_text(
         face = 'bold',
         size = 14,
         family = 'mono'
         ),
        axis.text = element_text(
          family = 'mono',
          size = 16,
          face = 'bold'
          ),
       plot.caption = element_text(
         color = '#9d0208',
         size = 20,
         family = 'serif',
         face = 'bold'
         ),
       panel.background = element_rect(
         fill = 'black'
         ),
       panel.grid.major = element_blank()
    )

ggsave(filename = 'week_17.png', width = 450, height = 250, plot = last_plot(), units = 'mm', path = '2021/week 17/')
  

