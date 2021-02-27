##transit_cost
library(tidyverse)


transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

transit_cost_US <- transit_cost %>%
  filter(country == 'US')

  ggplot(transit_cost_US,aes(y = length, x = city)) +
  geom_smooth() 
  