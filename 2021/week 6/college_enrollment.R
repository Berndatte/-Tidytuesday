#TIDYTUESDAY WEEK 6
#HBCU ENROLLMENT
#BERNDATTE


#Reading in Data
hbcu_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hbcu_all.csv')

#loading the packages to be used
library(tidyverse)
library(patchwork)

#Tidying the data
hbcu_all_1 <- hbcu_all%>%
  gather(gender,total,3:4)%>%
  rename("total_enrollment" = "Total enrollment" )%>%
  rename("Public" = 'Total - Public')%>%
  rename("Private" = 'Total - Private')%>%
  gather(school_type, Total,c(5,8))%>%
  select(-c(3:8))


#plotting
 p1 <- ggplot(hbcu_all_1, aes(Year, Total, color = school_type)) + 
  geom_line(size = 1) +
  labs(title = "Distribution of Students in Public and Private Schools") +
   scale_color_discrete(name ="", labels = c('Private School', 'Public School'))+
   theme_bw()+
   theme(legend.text = element_text(face = 'bold', color = '#e56b6f', size = 16),
         plot.title = element_text(color = "#9d0208", size = 23, face = 'bold', family = 'serif'),
         axis.title = element_text(color = '#6a040f', size = 20, face = 'bold'),
         axis.text = element_text(size = 16, face = 'bold', family = 'mono'),
         panel.grid.minor.y = element_blank(),
         legend.position = 'bottom')
  
  

p2 <- ggplot(hbcu_all_1, aes(Year, total, color = gender)) +
  geom_point(size = 2)+
  labs(title = 'Total Enrollment Per Gender', y = 'Total')+
  scale_color_discrete(name = "", labels = c('Females', 'Males')) +
  theme(legend.text = element_text(face = 'bold.italic', family = 'serif', color = '#5f0f40', size = 18),
        axis.title = element_text(color = '#fb8b24', size = 20,face = 'bold'),
        axis.text = element_text(color ='#0353a4', size = 16, face = 'bold'),
        title = element_text(color = '#5a189a', size = 24, face = 'bold', family = 'mono'),
        panel.background = element_rect(fill = '#ede7e3'),
        legend.position = 'bottom')


#Combining the plots
p1 + p2 + plot_annotation(title = 'College Enrollment', caption = 'Berndatte \n Data:Data.World & Data.World',
                          theme = theme(plot.title = element_text(hjust = 0.5, color = '#c44536', size = 26,
                                                                  face = 'bold', family = 'serif'),
                                        plot.caption = element_text(color = 'chocolate4', size = 20, face = 'bold', family = 'Colonna MT')))

#Saving the plots
ggsave(filename = 'college_enrollment.png', plot = last_plot(),width = 450, height = 300, units = 'mm', path = '2021/week 6/')                                     
