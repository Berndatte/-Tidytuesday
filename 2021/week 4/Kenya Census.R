#TIDYTUESDAY WEEK 4
#KENYA CENSUS
#BERNDATTE

#Getting the Data
gender <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/gender.csv')
crops <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/crops.csv')
households <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/households.csv')


#Loading the packages
library(tidyverse)
library(patchwork)
library(extrafont)

#renaming columns
crops <- crops %>%
  rename(County = SubCounty)



#removing farming column and row 1(Kenya)

crops <- crops[-1, -2]

#calculating total crops for each county
crops$total <- rowSums(crops[2:10], na.rm =  T)


#putting crops into one column(making wide dataset long)
crops_1 <- crops %>%
  gather(crop, n, 2:10, convert = T)




#sorting in alphabetical order
crops_1_sorted <- with(crops_1, crops_1[order(County),])


#Removing NAs
crops_1_sorted <- na.omit(crops_1_sorted)



crops_1_sorted$fraction = crops_1_sorted$n / crops_1_sorted$total 


crops_1_sorted <- crops_1_sorted %>% 
  rename(Crop = crop, Total = total)


#Plotting
p1 <- ggplot(crops_1_sorted, aes(County, fraction,fill = Crop)) +
  geom_bar(stat = 'Identity') +
  labs(title = 'Farming Proportion by Crop and County',
       x = '',
       y = 'Crop Proportion',
       caption = 'Nthambi \n Data: Kenya population and housing Census 2019') +
  coord_flip() +
  scale_y_continuous(expand = c(0,0))+
  theme(title = element_text(face = 'bold', size = 24, family = 'Lucida Bright', color = 'blue4'), 
        axis.title = element_text(face = 'bold.italic', family = 'Ink Free', color = 'blue', size = 22),
        axis.text = element_text(face = 'bold', size = 18, family = 'mono'),
        legend.text = element_text(size = 20,face = 'bold.italic', family = 'serif'),
        legend.title = element_text(size = 24, face = 'bold', family = 'Lucida Bright', color = 'cyan3'),
        plot.caption = element_text(family = 'mono', size = 22, color = 'cornflowerblue'))



crop_1_sort <- crops_1_sorted %>%
  group_by(County, Crop) %>%
  summarise(total = sum(n)) %>%
  filter(!is.na(total)) %>%
  filter(Crop %in% c('Avocado','Mango'))


p2 <- ggplot(crop_1_sort, aes(County, total)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  coord_flip()+
  facet_wrap(~Crop)+
  labs(title = 'Top Two Crops Produced In Kenya', x = 'Total Population growing crops', y = 'County', caption = 'Nthambi \n Source:rKenyaCensus') +
  theme_light() +
  theme(plot.title = element_text(family = 'serif', face = 'bold.italic',color = 'tomato', size = 24 ),
        axis.title = element_text(face = 'italic', size = 22),
        axis.text = element_text(face = 'bold', color = 'purple', size = 18),
        strip.background = element_rect(fill = 'deepskyblue3'),
        strip.text = element_text(color = 'white', face = 'bold.italic', family = 'serif', size = 24),
        plot.caption = element_text(color = 'darkmagenta', family = 'Impact', size = 18))

ggsave(filename = 'farming.png', plot = p1, width = 450, height = 300, units = 'mm', path = '2021/week 4/') 


ggsave(filename = 'top_2_crops.png', plot = p2, width = 450, height = 300, units = 'mm', path = '2021/week 4/') 
