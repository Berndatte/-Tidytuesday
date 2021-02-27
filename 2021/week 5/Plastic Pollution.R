#week 5
#Plastic Pollution

#loading the packages
library(tidyverse)
library(patchwork)

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


ggplot(kenya, aes(parent_company,n ,color = type)) +
  geom_point(size = 3)+
  facet_wrap(~year)+
  coord_flip()+
  scale_color_brewer(palette = 'Set1')+
  labs(title = 'Plastics Wastes in Kenya', x = 'Parent Company',
       y = 'Total Count', caption = 'Nthambi \n source:Break Free From Plastic')


  
