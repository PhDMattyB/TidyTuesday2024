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
library(patchwork)
library(vegan)

data = tidytuesdayR::tt_load(2024, 
                             week = 41)

n_park = data$most_visited_nps_species_data

theme_set(theme_bw())

col_pal = c('#5f0f40',
            '#9a031e', 
            '#0f4c5c',
            '#fb8b24',
            '#e36414', 
            '#ff006e')

clean_data = n_park %>% 
  select(ParkCode, 
         CategoryName, 
         Abundance, 
         Observations) %>%
  filter(CategoryName %in% c('Mammal', 
                             'Bird', 
                             'Reptile', 
                             'Amphibian', 
                             'Fish')) %>% 
  na.omit()



diversity(clean_data$Observations, 
          index = 'shannon', 
          groups = clean_data$ParkCode)

specnumber(clean_data$Observations,
           groups = clean_data$ParkCode, 
           MARGIN = 1)


# mammals -----------------------------------------------------------------

mammal_div = n_park %>% 
  group_by(ParkCode, 
           SciName, 
           CategoryName) %>% 
  filter(CategoryName %in% c('Mammal', 
                             'Bird', 
                             'Reptile', 
                             'Amphibian', 
                             'Fish')) %>% 
  filter(CategoryName == 'Mammal', 
         TaxonRecordStatus == 'Active', 
         RecordStatus == 'Approved', 
         Occurrence == 'Present') %>%  
  ungroup() %>% 
  group_by(Family, 
           ParkCode, 
           Abundance) 

diversity(mammal_div$Observations, 
          index = 'shannon', 
          groups = mammal_div$ParkCode)

mammal = n_park %>% 
  group_by(ParkCode, 
           SciName, 
           CategoryName) %>% 
  filter(CategoryName %in% c('Mammal', 
                             'Bird', 
                             'Reptile', 
                             'Amphibian', 
                             'Fish')) %>% 
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
  select(1:18) %>% 
  ungroup() %>% 
  group_by(Family, 
           ParkCode, 
           Abundance) %>% 
  na.omit() %>% 
  # filter(Abundance %in% c('Abundant', 
  #                         'Common', 
  #                         'Occasional', 
  #                         'Rare', 
  #                         'Uncommon')) %>% 
  ggplot(aes(x = Family, 
           y = Observations)) +
  geom_point(aes(col = Abundance), 
             size = 2)+
  facet_grid(~ParkCode)+
  labs(title = 'Mammals')+
  scale_color_manual(values = col_pal)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text = element_text(size = 12), 
        strip.background = element_rect(fill = 'white'), 
        strip.text = element_text(size = 12),
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5))



# birds -------------------------------------------------------------------

bird = n_park %>% 
  # select(CategoryName) %>% 
  # distinct()
  group_by(ParkCode, 
           SciName, 
           CategoryName) %>%
  filter(CategoryName %in% c('Mammal', 
                             'Bird', 
                             'Reptile', 
                             'Amphibian', 
                             'Fish')) %>% 
  filter(CategoryName == 'Bird', 
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
  select(1:18) %>% 
  ungroup() %>% 
  group_by(Family, 
           ParkCode, 
           Abundance) %>% 
  na.omit() %>% 
  # filter(Abundance %in% c('Abundant', 
  #                         'Common', 
  #                         'Occasional', 
  #                         'Rare', 
  #                         'Uncommon')) %>% 
  ggplot(aes(x = Family, 
           y = Observations)) +
  geom_point(aes(col = Abundance), 
             size = 2)+
  facet_grid(~ParkCode)+
  labs(title = 'Birds')+
  scale_color_manual(values = col_pal)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text = element_text(size = 12), 
        strip.background = element_rect(fill = 'white'), 
        strip.text = element_text(size = 12),
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.position = 'none')



# Reptiles ----------------------------------------------------------------
reptile = n_park %>% 
  # select(CategoryName) %>% 
  # distinct()
  group_by(ParkCode, 
           SciName, 
           CategoryName) %>%
  filter(CategoryName %in% c('Mammal', 
                             'Bird', 
                             'Reptile', 
                             'Amphibian', 
                             'Fish')) %>% 
  filter(CategoryName == 'Reptile', 
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
  select(1:18) %>% 
  ungroup() %>% 
  group_by(Family, 
           ParkCode, 
           Abundance) %>% 
  na.omit() %>% 
  # filter(Abundance %in% c('Abundant', 
  #                         'Common', 
  #                         'Occasional', 
  #                         'Rare', 
  #                         'Uncommon')) %>% 
  ggplot(aes(x = Family, 
             y = Observations)) +
  geom_point(aes(col = Abundance), 
             size = 2)+
  facet_grid(~ParkCode)+
  labs(title = 'Reptiles')+
  scale_color_manual(values = col_pal)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text = element_text(size = 12), 
        strip.background = element_rect(fill = 'white'), 
        strip.text = element_text(size = 12),
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.position = 'none')




# Amphibians --------------------------------------------------------------

amphibian = n_park %>% 
  # select(CategoryName) %>% 
  # distinct()
  group_by(ParkCode, 
           SciName, 
           CategoryName) %>%
  filter(CategoryName %in% c('Mammal', 
                             'Bird', 
                             'Reptile', 
                             'Amphibian', 
                             'Fish')) %>% 
  filter(CategoryName == 'Amphibian', 
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
  select(1:18) %>% 
  ungroup() %>% 
  group_by(Family, 
           ParkCode, 
           Abundance) %>% 
  na.omit() %>% 
  # filter(Abundance %in% c('Abundant', 
  #                         'Common', 
  #                         'Occasional', 
  #                         'Rare', 
  #                         'Uncommon')) %>% 
  ggplot(aes(x = Family, 
             y = Observations)) +
  geom_point(aes(col = Abundance), 
             size = 2)+
  facet_grid(~ParkCode)+
  labs(title = 'Amphibians')+
  scale_color_manual(values = col_pal)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text = element_text(size = 12), 
        strip.background = element_rect(fill = 'white'), 
        strip.text = element_text(size = 12),
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.position = 'none')


# Fish --------------------------------------------------------------------

fish = n_park %>% 
  # select(CategoryName) %>% 
  # distinct()
  group_by(ParkCode, 
           SciName, 
           CategoryName) %>%
  filter(CategoryName %in% c('Mammal', 
                             'Bird', 
                             'Reptile', 
                             'Amphibian', 
                             'Fish')) %>% 
  filter(CategoryName == 'Fish', 
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
  select(1:18) %>% 
  ungroup() %>% 
  group_by(Family, 
           ParkCode, 
           Abundance) %>% 
  na.omit() %>% 
  # filter(Abundance %in% c('Abundant', 
  #                         'Common', 
  #                         'Occasional', 
  #                         'Rare', 
  #                         'Uncommon')) %>% 
  ggplot(aes(x = Family, 
             y = Observations)) +
  geom_point(aes(col = Abundance), 
             size = 2)+
  facet_grid(~ParkCode)+
  labs(title = 'Fish')+
  scale_color_manual(values = col_pal)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text = element_text(size = 12), 
        strip.background = element_rect(fill = 'white'), 
        strip.text = element_text(size = 12),
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.position = 'none')




# Combo plots -------------------------------------------------------------

div_park = mammal/bird/reptile/amphibian/fish
