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

View(n_park)




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

ggplot(data = clean_mammal, 
       aes(x = Family, 
           y = Observations)) +
  geom_point(aes(col = Family))+
  facet_grid(~ParkCode)
