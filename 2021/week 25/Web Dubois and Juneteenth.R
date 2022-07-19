#TIDYTUESDAY WEEK 25
#WEB Du Bois and Juneteenth
#BERNDATTE
library(tidyverse)
library(lubridate)
tweets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-15/tweets.csv')



tweets_2 <- tweets %>%
  filter(!is.na(datetime))%>%
  mutate(month = month(datetime))%>% 
  mutate(Month = ifelse(month == 3, 'March',
                                   ifelse(month == 4, 'April',
                                          ifelse(month == 5, 'May',
                                                 ifelse(month == 2, 'February','no'))))) %>%
  mutate(Month = fct_relevel(Month, 'February', 'March', 'April', 'May'))


ggplot(tweets_2, aes(Month, retweet_count, size = like_count)) +
  geom_jitter() +
  geom_point(filter(data = tweets_2,retweet_count > 150),
             color = 'blue')
