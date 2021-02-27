#W.E.B Du Bois Challenge

georgia_pop <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/georgia_pop.csv')
census <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/census.csv')
furniture <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/furniture.csv')
city_rural <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/city_rural.csv')
income <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/income.csv')
freed_slaves <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/freed_slaves.csv')
occupation <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/occupation.csv')
conjugal <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/conjugal.csv')


library(tidyverse)
library(extrafont)

census_clean <- census%>%
  select(region,year, white, black)%>%
  gather(Group, total,c(3,4))%>%
  filter(region != 'USA Total')

ggplot(census_clean, aes(year, total)) +
  geom_line()



ggplot(occupation, aes(y = Percentage, x = Group, fill = Occupation)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(expand = c(0,0))+
  geom_text(aes(label = paste0(Percentage,'%')),position = position_stack(vjust = 0.5), size = 5,
            fontface = 'bold', color = 'brown4')+
  labs(title = 'Occupation by Race', x = 'Racial Group', y = 'Percentage',
       caption = 'Nthambi \n Source:Du Bois Data Challenge')+
  theme_bw() +
  theme(axis.title = element_text(family = 'Vivaldi', face = 'bold', size = 28),
        plot.title = element_text(family = 'Verdana', face = 'bold', size = 28, color = '#1f2c93', hjust = 0.5),
        axis.text = element_text(color = '#522888', face = 'bold', size = 18),
        axis.ticks.x = element_line(color = '#2b2c20'),
        legend.title = element_text(family = 'Bodoni MT', color = 'firebrick4', size = 24, face = 'bold'),
        legend.text = element_text(family = 'serif', size = 22, face = 'bold'),
        plot.caption = element_text(family = 'Colonna MT', size = 22, color = 'deepskyblue4'))


ggsave(filename = 'Occupation.png', plot = last_plot(), height = 300, width = 450, units = 'mm', path = 'week 8/')


