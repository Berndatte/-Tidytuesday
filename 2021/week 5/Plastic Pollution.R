#TIDYTUESDAY WEEK5
#PLASTIC POLLUTION
#BERNDATTE

#loading the packages
library(tidyverse)
library(patchwork)
library(extrafont)

#reading data
plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')


kenya <- plastics %>% 
  filter(country == 'Kenya') %>%
  gather(type, n, 5:11,convert = T) %>%
  select(-c(country,volunteers, num_events, empty))%>%
  filter(parent_company != 'Grand Total')%>%
  arrange(desc(grand_total))%>%
  filter(grand_total > 100)%>%
  filter(n != 0)

#plotting

ggplot(kenya, aes(parent_company,n ,color = type)) +
  geom_point(size = 3)+
  facet_wrap(~year)+
  coord_flip()+
  scale_color_brewer(palette = 'Set1')+
  labs(title = 'Plastics Wastes in Kenya', x = 'Parent Company',
       y = 'Total Count', caption = 'Nthambi \n source:Break Free From Plastic')+
  theme(plot.title = element_text(family = 'Algerian', size = 24, color = 'dodgerblue3', face = 'bold'),
        axis.title = element_text(family = 'Arial', size = 24, color = 'darkred'),
        axis.text = element_text(family = 'Arial', size = 18),
        plot.caption = element_text(family = 'Bahnschrift', size = 18,color = 'Deeppink3'),
        legend.title = element_text(family = 'Impact', size = 20, color = 'gray1', face = 'bold'),
        strip.background = element_rect(color = 'gold4'),
        strip.text = element_text(color = 'gray1', size = 20, family = 'Algerian'),
        legend.text = element_text(size = 20, family = 'Gabriola'))

#Saving the plot
ggsave(filename = 'Plastics.png', plot = last_plot(), width = 450, height = 300, units = 'mm', path = '2021/week 5/')
