##############################
## Operation save the fish
##
## Matt Brachmann (PhDMattyB)
##
## 08.11.2024
##
##############################

setwd('~/Parsons_Postdoc/Experiment1/')

library(tidyverse)

save_fish = read_csv('save_the_fish_data.csv')

save_fish = save_fish %>% 
  mutate(condition_factor = Weight/Length^(1/3)*100)

save_fish_pal = c("#778da9",
                  '#f72585')

save_da_fish = save_fish %>% 
  ggplot()+
  geom_point(aes(x = Weight, 
                 y = Length, 
                 fill = status),
             size = 3, 
             col = 'black', 
             pch = 21)+
  geom_smooth(aes(x = Weight, 
                  y = Length), 
              col = 'Black')+
  scale_fill_manual(values = save_fish_pal)+
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12), 
        legend.position = 'none')


ggsave('Save_The_Fish.tiff', 
       plot = save_da_fish, 
       dpi = 'retina', 
       units = 'cm')


save_fish %>% 
  mutate(.data = save_fish,
         status2 = as.factor(case_when(
           status == 'post' ~ 'background',
           status == 'initial' ~ 'background',
           status == 'Save' ~ 'Save'))) %>% 
  ggplot()+
  geom_violin(aes(x = status2, 
                  y = condition_factor, 
                  fill = status2), 
              col = 'black')+
  geom_boxplot(aes(x = status2, 
                   y = condition_factor, 
                   fill = status2), 
               col = 'black')+
  scale_fill_manual(values = save_fish_pal)+
  labs(y = 'Condition factor')

cond_factor_plot = save_fish %>% 
  mutate(.data = save_fish,
         status2 = as.factor(case_when(
           status == 'post' ~ 'background',
           status == 'initial' ~ 'background',
           status == 'Save' ~ 'Save'))) %>% 
  ggplot()+
  # geom_histogram(aes(x = condition_factor, 
  #                    fill = status), 
  #                col = 'black')+
  geom_dotplot(aes(x = condition_factor,
                   fill = status2, 
                   col = status2),
               # col = 'black', 
               binwidth = 1)+
  geom_segment(aes(x = 179.84, 
                   y = 0.50,
                   xend = 179.84, 
                   yend = 0.1), 
               arrow = arrow(length = unit(0.5, 
                                           'cm')))+
  # geom_freqpoly(aes(x = condition_factor, 
  #                  fill = status), 
  #              col = 'black')+
  scale_fill_manual(values = save_fish_pal)+
  scale_color_manual(values = save_fish_pal)+
  labs(x = 'Condition factor', 
       y = 'Number of fish')+
  theme(panel.grid = element_blank(), 
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12), 
        legend.position = 'none')
ggsave('save_fish_dotplot.tiff', 
       plot = cond_factor_plot, 
       dpi = 'retina', 
       units = 'cm')
