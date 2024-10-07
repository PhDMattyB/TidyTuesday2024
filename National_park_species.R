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

data = tidytuesdayR::tt_load(2024, 
                             week = 41)

n_park = data$most_visited_nps_species_data

theme_set(theme_bw())

col_pal = c('#5f0f40',
            '#9a031e', 
            '#0f4c5c',
            '#fb8b24',
            '#e36414')

n_park %>% 
  select(CategoryName) %>%
  distinct()

# mammals -----------------------------------------------------------------

mammal = n_park %>% 
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
  select(1:18) %>% 
  ungroup() %>% 
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
  labs(title = 'Mammals')+
  scale_color_manual(values = col_pal)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14), 
        axis.title.x = element_blank(), 
        axis.text = element_text(size = 12), 
        strip.background = element_rect(fill = 'white'), 
        strip.text = element_text(size = 12),
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.position = 'none')



# birds -------------------------------------------------------------------

bird = n_park %>% 
  # select(CategoryName) %>% 
  # distinct()
  group_by(ParkCode, 
           SciName, 
           CategoryName) %>%
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
  labs(title = 'Birds')+
  scale_color_manual(values = col_pal)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14), 
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
  labs(title = 'Reptiles')+
  scale_color_manual(values = col_pal)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14), 
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
  labs(title = 'Amphibians')+
  scale_color_manual(values = col_pal)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14), 
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
  labs(title = 'Fish')+
  scale_color_manual(values = col_pal)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 14), 
        axis.title.x = element_blank(), 
        axis.text = element_text(size = 12), 
        strip.background = element_rect(fill = 'white'), 
        strip.text = element_text(size = 12),
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.position = 'none')




# Combo plots -------------------------------------------------------------

div_park = mammal/bird/reptile/amphibian/fish
