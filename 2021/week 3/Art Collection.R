
#TIDYTUESDAY WEEK 3
##ART COLLECTION
#BERNDATTE

#loading packages
library(tidyverse)
library(extrafont)

#reading data
artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')
artists <- readr::read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv")

#renaming variables
artwork <- artwork %>%
 rename(name = artist)

colnames(artwork)
colnames(artists)

artworks <- artwork %>%
  select(-id,-url, -thumbnailUrl, -thumbnailCopyright)

artist <- artists %>%
  select(-url)

#joining artist and artworks datasets
artistss <- full_join(artist, artworks, by = 'name')
colnames(artistss)

#selecting the variables i need 
artist_clean <- artistss %>%
  select(name,gender,year) %>%
  filter(!is.na(year), !is.na(gender))%>%
  filter(year >= '1700')

arts <- artist_clean %>%
  group_by(year, gender) %>%
  summarise(total = n())
  
#Plotting
ggplot(arts, aes(year,total, color = gender)) +
  geom_path(size =1) +
  labs(title = 'Artworks purchased by Tate since 1700', x = 'Year', y = 'Total Number of Artwork',
       caption = 'Visualization:Nthambi \n Source: Tate Collection')+
  scale_color_manual(name = 'Gender', labels = c('Female', 'Male'), values = c('blue', 'tomato')) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        title = element_text(family = 'serif' , face = 'bold', size = 22, hjust = 0.5),
        axis.title = element_text(family = 'serif', face = 'bold', size = 22),
        axis.text = element_text(color = 'blue', size = 18),
        plot.caption = element_text(size = 18, colour = 'tomato'),
        legend.position = c(0.9, 0.9),
        legend.text = element_text(color = 'cornflowerblue', size = 16, family = 'Ravie'),
        legend.title = element_text(family = 'Vivaldi', face = 'bold', size = 22))

#Saving the plot

ggsave(filename = 'artwork.png', plot = last_plot(), width = 450, height = 300, units = 'mm', path = '2021/week 3/')  
  


