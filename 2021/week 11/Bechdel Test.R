#TIDYTUESDAY WEEK 11
#BECHDEL TEST
#BERNDATTE

#LOADING PACKAGES
library(tidyverse)
library(extrafont)

#Getting Data
raw_bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/raw_bechdel.csv')
movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')


#DATA WRANGLING
movie <- movies %>%
  select(year,title, budget, clean_test)%>%
  drop_na(year,budget, clean_test)%>%
  mutate(budget = budget /1000000)%>%
  filter(clean_test != 'dubious')

source <- 'Source:FiveThirtyEight \n Berndatte'

#VISUALIZING
 ggplot(movie,
        aes(year,budget, color = clean_test)
        ) +
  geom_jitter(size = 2.5)+
   
  scale_color_manual(values = c("#fca311", "#583101", "#ef476f", "#2b9348")
                     )+
   geom_text(aes(x = 2009, y = 410, label = 'Movie: Avatar'), show.legend = F) +
  labs(x = 'Year',
       y = 'Budget in million',
       title = 'Budget of Movies -since 1970',
       caption = source) +
   theme_bw()+
  theme(plot.title = element_text(
    color = '#772e25',
    size = 24,
    family = 'Algerian',
    face = 'bold'
    ),
        plot.caption = element_text(
          color = '#d77a61',
          size = 16,
          face = 'bold'),
        axis.title = element_text(
          color = '#585123',
          size = 18, 
          face = 'bold'
          ),
        axis.text = element_text(
          size = 16, 
          face = 'bold',
          family = 'mono'
          ),
        legend.text = element_text(
          size = 18,
          family = 'mono',
          face = 'bold'
          ),
        legend.title = element_text(
          size = 18,
          family = 'serif',
          face = 'bold')
    )

 
 
 #SAVING
 ggsave(filename = 'week_11.png',
        height = 250,
        width = 400, 
        units = 'mm', 
        plot = last_plot(),
        path = '2021/week 11/'
        )
