#TIDYTUESDAY WEEK 22
#MARIO KART WORLD RECORDS
#BERNDATTE

library(tidyverse)
library(lubridate)

records <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/records.csv')
drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/drivers.csv')


drivers_t10 <- drivers %>%
  filter(position <= 10) %>%
  filter(!is.na(records))


ggplot(drivers_t10, aes(year, records, color = nation)) +
  geom_point() +
  facet_wrap(.~ player)
