#TIDYTUESDAY WEEK 21
#ASK A MANAGER SALARY SURVEY 
#BERNDATTE



survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-18/survey.csv')

survey1 <- survey %>%
  filter(gender == 'Woman')%>%
  group_by(country)
  filter( annual_salary < 70000000 & annual_salary > 50000)

ggplot(survey1, aes(country, annual_salary)) +
  geom_point()+
  coord_flip()


