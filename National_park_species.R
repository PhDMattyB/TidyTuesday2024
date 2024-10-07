##############################
## Tidy Tuesday - national park species
##
## Matt Brachmann (PhDMattyB)
##
## 07.10.2024
##
##############################

library(tidytuesdayR)
library(tidyverse)

data = tidytuesdayR::tt_load(2024, 
                             week = 41)

n_park = data$most_visited_nps_species_data

clean_mammal = n_park %>% 
  group_by(ParkCode, 
           SciName, 
           CategoryName) %>% 
  filter(CategoryName == 'Mammal', 
         TaxonRecordStatus == 'Active', 
         RecordStatus == 'Approved', 
         Occurrence == 'Present') %>% 
  select(-Synonyms, 
         -OccurrenceTags, 
         -NativenessTags, 
         -ParkTags, 
         -OzoneSensitiveStatus, 
         -GRank, 
         -SRank) %>% 
  select(1:18)

theme_set(theme_bw())

col_pal = c('#5f0f40', 
            '#0f4c5c',
            '#9a031e', 
            '#fb8b24',
            '#e36414')

clean_mammal %>% 
  group_by(Family, 
           ParkCode, 
           Abundance) %>% 
  na.omit() %>% 
  filter(Abundance %in% c('Abundant', 
                          'Common', 
                          'Occasional', 
                          'Rare', 
                          'Uncommon')) %>% 
  ggplot(aes(x = Family, 
           y = Observations)) +
  geom_point(aes(col = Abundance), 
             size = 2)+
  facet_grid(~ParkCode)+
  scale_color_manual(values = col_pal)+
  theme(axis.text.x = element_text(angle = 90, 
                                   size = 5), 
        axis.title = element_text(size = 14), 
        axis.title.x = element_blank(), 
        axis.text = element_text(size = 12), 
        strip.background = element_rect(fill = 'white'), 
        strip.text = element_text(size = 12),
        panel.grid = element_blank())


# clean_mammal %>% 
#   group_by(Family, 
#            ParkCode, 
#            Abundance) %>% 
#   na.omit() %>% 
#   filter(Abundance %in% c('Abundant', 
#                           'Common', 
#                           'Occasional', 
#                           'Rare', 
#                           'Uncommon')) %>% 
#   View()
