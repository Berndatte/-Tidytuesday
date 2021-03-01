##TIDYTUESDAY WEEK 2-2021
##TRANSIT COSTS PROJECT
##BERNDATTE#

#loading the packages
library(tidyverse)
library(patchwork)


#reading Data
transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

#filtering the US data only
US <- transit_cost %>%
  filter(country == 'US')

length_of_road <- US %>%
  select(city, length) %>%
  group_by(city) %>%
  summarise(total_length = sum(length))

road_cost <- US %>%
  select(city, cost) %>%
  group_by(city) %>%
  summarize(total_cost = sum(cost))

#plotting
p1 <- ggplot(length_of_road, aes(city, total_length, fill = city)) +
  scale_fill_brewer(palette = 'Dark2') +
  geom_bar(stat = 'identity', color = 'blue') +
  labs(title = 'TOTAL LENGTH OF THE ROADS IN KM', x = 'City', y = 'Total_length') +
  theme_bw() +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'none',
        axis.title = element_text(size = 28, face = 'bold.italic'),
        axis.text = element_text(size = 22, family = 'mono', color = 'purple', face = 'bold'),
        plot.title = element_text(family = 'mono', size = 28, face = 'bold'),
        panel.grid = element_blank())


p2 <- ggplot(road_cost, aes(city, total_cost, fill = city)) +
  scale_fill_brewer(palette = 'Set1') +
  geom_bar(stat = 'identity') +
  labs(title = 'TOTAL COST', x = 'City', y = 'Total_cost') +
  theme_bw() +
  scale_y_continuous(labels = scales::dollar) +
  theme(panel.background = element_blank(),
        axis.text = element_text(size = 20, face = 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'none', 
        axis.title = element_text(size = 25, face = 'bold'),
        panel.grid = element_blank(),
        plot.title = element_text(size = 26, face = 'bold'))



p1 + p2 + plot_annotation(title = 'US TRANSIT COST',
                          theme = theme(plot.title = element_text(size = 25, color = 'blue', hjust = 0.5, face = 'bold')))



ggsave(filename = 'transit_cost.png', plot = last_plot(), width = 450, height = 300, units = 'mm',path = '2021/week 2/')

  
