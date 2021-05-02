#TIDYTUESDAY WEEK 18
#CEO Departures
#BERNDATTE

#LOADINF PACKAGES
library(tidyverse)
library(extrafont)

#GETTING DATA
departures <- 
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv')

#DATA WRANGLE
depart_CEO <- departures %>%
  select(fyear, 
         departure_code,
         ceo_dismissal,
         tenure_no_ceodb,
         max_tenure_ceodb)%>%
  filter(
    departure_code < 8 &
      fyear > 1995)%>%
  mutate(
    departure_code = as.factor(departure_code)
    )%>%
  mutate(Reason = recode(departure_code,
                         '1' = 'Death',
                         '2' = 'Illness',
                         '3' = 'Dismissed-Perfomance',
                         '4' = 'Dismissed-Legal concerns',
                         '5' = 'Retired',
                         '6' = 'New Opportunities',
                         '7' = 'Other')) %>%
  group_by(fyear, Reason) %>%
  count()

source <- 'Source:Gentry et al.\nVisualizer:Berndatte'


#VISUALIZING
ggplot(depart_CEO,
       aes(n,
           fyear,
           color = Reason)
       ) +
  geom_point(size = 5) +
  labs(
    title = 'CEO Departures by Reason', 
    caption = source,
    y = 'year', 
    x = 'Number of Departures'
    )+
  scale_y_continuous(
    breaks = seq(1995,2020,1),
    limits = c(1995, 2020))+
  scale_x_continuous(
    breaks = seq(0, 200, 10),
    limits = c(0, 200))+
  theme_light()+
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(
          family = 'Tw Cen MT',
          size= 26, 
          color = '#283618',
          face = 'bold'
          ),
        axis.title = element_text(
          family = 'serif',
          size = 20),
        axis.text = element_text(
          family = 'mono',
          size = 16,
          face = 'bold'),
        legend.title = element_text(
          family = 'serif',
          size = 18,
          face= 'bold'),
        legend.text = element_text(
          family = 'mono',
          size = 16,
          face= 'bold'),
        plot.caption = element_text(
          family = 'Tw Cen MT',
          color = '#370617',
          size = 20 ))
  
#SAVING PLOT
ggsave(filename = 'week_18.png',
       height = 350,
       width = 400,
       units = 'mm',
       path = '2021/week 18/')

