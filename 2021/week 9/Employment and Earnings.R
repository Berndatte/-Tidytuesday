#TIDYTUESDAY WEEK 9
#EMPLOYMENTS AND EARNINGS
#BERNDATTE

#LOADING PACKAGES
library(tidyverse)
library(lubridate)
library(extrafont)

#Getting Data
employed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/employed.csv')

earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv')


#Filtering the observations and selecting the variables i will use
earn_race <- earn %>% 
  filter(race %in% c('White', 'Asian', 'Black or African American'))%>% #To remove 'All races' observations
  filter(sex != 'Both Sexes')%>% #to get the distinct gender
  filter(age %in% c('16 to 24 years', '25 to 54 years', '55 years and over'))%>% #filtering out 16yrs and over and 25yrs and over
  select(-ethnic_origin) %>%
  mutate(age = recode(age, '16 to 24 years' = '16-24yrs',
                      '25 to 54 years' = '25-54yrs',
                      '55 years and over' = '55yrs and over'))%>%
  group_by(sex,race,age,year)%>%
  summarise( mean_weekly_earn = round(mean(median_weekly_earn)))%>%
  filter(year >= '2015')
         

#plotting
ggplot(earn_race, aes(race, mean_weekly_earn, fill = sex)) + 
  geom_boxplot()+
  facet_wrap(~age)+
  theme_bw()+
  labs(title = 'Weekly mean earning by Race, gender,age group over time', x = "Race", 
  y = "Mean Weekly Earnings",
  caption = 'Source:BLS \n Visualization: Nthambi')+
  theme(plot.title = element_text(family = 'Elephant', size = 28, color = 'coral4'),
        axis.title = element_text(family = 'Gabriola', size = 24, color = 'chartreuse4', face = 'bold'),
        axis.text = element_text(family = 'mono', size = 16, color = 'darkgoldenrod4', face = 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
        legend.title = element_text(family = 'Segoe Print', size = 16, face = 'bold'),
        legend.text = element_text(family = 'Segoe Print', size = 18),
        strip.text = element_text(family = 'serif', size = 19, color = 'darkviolet'),
        plot.caption = element_text(family = 'Bodoni MT', size = 22, color = 'chocolate4'))


#Saving
ggsave(filename = 'Earnings.png', plot = last_plot(), height = 300, width = 450, units = 'mm', path = '2021/week 9/')


  
