#TIDYTUESDAY WEEK 7
#WEALTH AND INCOME
#BERNDATTE


#Getting Data
lifetime_earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/lifetime_earn.csv')
student_debt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/student_debt.csv')
retirement <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/retirement.csv')
home_owner <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/home_owner.csv')
race_wealth <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/race_wealth.csv')
income_time <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_time.csv')
income_limits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_limits.csv')
income_aggregate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_aggregate.csv')
income_distribution <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_distribution.csv')
income_mean <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_mean.csv')

#Loading the packages 
library(tidyverse)
library(extrafont)

#plotting
p1 <- ggplot(lifetime_earn, aes(race, lifetime_earn, fill = race)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales:: dollar)+
  labs(title = 'Lifetime earnings by Race', x = 'Race', y = 'Lifetime earnings', 
       caption = 'Berndatte \n Data:Urban Institute & US Census')+
  theme_bw() +
  theme(legend.position = 'none',
        plot.title = element_text(family = "Broadway", face = 'bold', size = 24),
        axis.title = element_text(face = 'bold', family = 'Courier New', size = 22),
        axis.text = element_text(face = 'bold', family = 'serif', size = 16),
        plot.caption = element_text(face = 'bold', color = 'dodgerblue4', family = 'Bodoni MT', size = 18))

#saving first plot
ggsave(filename = 'lifetime_earnings.png', plot = p1,width = 450, height = 300, units = 'mm', path = '2021/week 7/')                                     

#plotting secong plot and saving
p2 <- ggplot(student_debt, aes(year, loan_debt, color = race)) +
  geom_line(size = 1)+
  labs(title = 'Average Family Student Loan Debt By Race over time', x = 'Year',
       y = 'Loan Debt', color = 'RACE', caption = 'Nthambi \n Source: Urban Institute &
       US Census')+
  scale_y_continuous(labels = scales:: dollar) +
  theme_light()+
  theme(plot.title = element_text(family = 'Segoe Print',face = 'bold', size = 22 ),
        axis.title = element_text(family = 'Viner Hand ITC', face = 'bold', size = 20),
        axis.text = element_text(family = 'mono', size = 19, , color = 'black'),
        legend.title = element_text(family = 'Colonna MT', size = 18, face = 'bold.italic'),
        legend.text = element_text(family = 'Arial', size =16, face = 'bold'),
        plot.caption = element_text(family = 'Algerian', color = 'darkgreen', size = 20))
ggsave(filename = 'student_debt.png', plot = p2, width = 450, height = 300, units = 'mm', path = '2021/week 7/')

