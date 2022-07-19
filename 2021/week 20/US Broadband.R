#TIDYTUESDAY WEEK 20
#US BROADBAND
#BERNDATTE


library(tidyverse)
#GETTING DATA
broadband <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-11/broadband.csv')

broadband1 <- broadband %>%
  rename(per_access = 'BROADBAND AVAILABILITY PER FCC',
         per_users = 'BROADBAND USAGE') %>%
  group_by(ST) %>%
  summarise(perc = sum(per_users))


ggplot(broadband1, aes(ST, per_access)) +
  geom_col() 



