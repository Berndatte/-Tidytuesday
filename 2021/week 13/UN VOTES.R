#TIDYTUESDAY WEEK 13
#UN VOTES
#BERNDATTE

#LOADING PACKAGES
library(tidyverse)
library(lubridate)
library(extrafont)

#Getting Data
unvotes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/unvotes.csv')
roll_calls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/roll_calls.csv')
issues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/issues.csv')


#DATA WRANGLE
UN_votes <- unvotes %>%
  full_join(roll_calls, by = 'rcid')%>%
  inner_join(issues, by ='rcid')%>%
  mutate(year = year(date))%>%
  rename("topic" = 'short_name')



by_year_country_topic<- UN_votes %>%
  group_by(year,country,topic)%>%
  summarise(total = n(), 
            percent_yes = mean(vote == 'yes'))

by_year_country_topic_tidied <- by_year_country_topic %>%
  mutate(topic = recode(topic, 
                        me = 'Palestian Conflict',
                        nu = 'Nuclear weapons and Material',
                        di = 'Arms control and disarmament',
                        hr = 'Human Rights',
                        co = 'Colonialism',
                        ec = 'Economic Development'))



kenya_SA <- by_year_country_topic_tidied %>%
  filter(country %in% c('Kenya', 'South Africa'))%>%
  filter(year >= '1963')


#VISUALIZE

ggplot(kenya_SA, aes(year, percent_yes, color = country)) +
  geom_line(size = 1) +
  facet_wrap(~topic)+
  labs(title = 'Percentage of Yes votes per Topic',
       subtitle = 'Kenya & South Africa', 
       caption = 'Berndatte \n Data : Harvard Dataverse') +
  theme_bw()+
  theme(plot.title = element_text
        (family = 'Algerian',
          color = '#3a86ff',
          face = 'bold',
          size = 24 ),
        strip.background = element_rect(
          fill = '#adb5bd'),
        strip.text = element_text(
          family = 'serif',
          face = 'bold',
          color = '#072ac8',
          size = 18),
        axis.title = element_text(
          family = 'Viner Hand ITC',
          color = '#5e60ce',
          size = 22,
          face = 'bold'),
        axis.text = element_text(
          family = 'mono',
          color = '#240046',
          size = 18),
        plot.subtitle = element_text(
          family = 'Verdana',
          face = 'bold',
          color = '#00b4d8',
          size = 22),
        plot.caption = element_text(
          family = 'Ink Free',
          size =22,
          color = '#006400'),
        legend.text = element_text(
          family = 'Ebrima',
          size = 18,
          color = '#17c3b2',
          face = 'bold'),
        legend.title = element_text(
          family = 'Elephant',
          size = 20,
          color = '#2ec4b6',
          face = 'bold'))


#SAVE
ggsave(filename = 'unvotes.png',
       height = 300,
       width = 450, 
       units = 'mm',
       plot = last_plot(),
       path = '2021/week 13/')

