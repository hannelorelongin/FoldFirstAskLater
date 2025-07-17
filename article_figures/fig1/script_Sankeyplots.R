# script for creating Sankey plot from taxonomy data
  # taxonomy data was first manually brought to the right levels

  # installs
#install.packages("dplyr") 
#install.packages("tidyverse")
library(dplyr)
library(tidyverse)

  #import phage taxonomy TSV file into data frame
df <- read.table(file = "C:/Users/hanne/OneDrive - KU Leuven/PhD_papers/fAnnot/figures/fig1/subplotA/phage_lineage_correctTaxLevels.txt", sep = '\t', header = TRUE)
sankey_data <- df
sankey_data$X <- NULL

#prepare file for Pavian
sankey_data <- sankey_data %>% 
  unite(col = "classification", c(Virus, Realm, Kingdom, Phylum, Class, Order, Family, Species), sep='; ') %>% 
  mutate_at("classification", str_replace, "Viruses", "d__Viruses") %>% 
  mutate_at("classification", str_replace, "; ", "|k__") %>% 
  mutate_at("classification", str_replace, "; ", "|p__") %>% 
  mutate_at("classification", str_replace, "; ", "|c__") %>% 
  mutate_at("classification", str_replace, "; ", "|o__") %>%
  mutate_at("classification", str_replace, "; ", "|f__") %>% 
  mutate_at("classification", str_replace, "; ", "|g__") %>% 
  mutate_at("classification", str_replace, "; ", "|s__") 

# Create format for Pavian with counts for each taxonomic level
# we're cheating here and actually not using the correct level because I also want to show realm
sankey_data_d <- sankey_data
sankey_data_d$classification <- sub("\\|k__.*", "", sankey_data_d$classification)  
sankey_data_k <- sankey_data
sankey_data_k$classification <- sub("\\|p__.*", "", sankey_data_k$classification)  
sankey_data_p <- sankey_data
sankey_data_p$classification <- sub("\\|c__.*", "", sankey_data_p$classification) 
sankey_data_c <- sankey_data
sankey_data_c$classification <- sub("\\|o__.*", "", sankey_data_c$classification) 
sankey_data_o <- sankey_data
sankey_data_o$classification <- sub("\\|f__.*", "", sankey_data_o$classification) 
sankey_data_f <- sankey_data
sankey_data_f$classification <- sub("\\|g__.*", "", sankey_data_f$classification) 
sankey_data_g <- sankey_data
sankey_data_g$classification <- sub("\\|s__.*", "", sankey_data_g$classification) 
sankey_data_s <- sankey_data

sankey_data_allTaxa <- bind_rows(sankey_data_s, sankey_data_g, sankey_data_f, sankey_data_o, sankey_data_c, sankey_data_p, sankey_data_k, sankey_data_d) %>% 
  mutate(classification = as.factor(classification)) %>% 
  count(classification) %>% 
  # rename for Pavian format
  rename(`#SampleID` = `classification`) %>% 
  rename(`Metaphlan2_Analysis` = `n`) 

# store file
write_tsv(sankey_data_allTaxa, "C:/Users/hanne/OneDrive - KU Leuven/PhD_papers/fAnnot/figures/fig1/subplotA/phage_input_pavian.txt")

#import host taxonomy TSV file into data frame
df2 <- read.table(file = "C:/Users/hanne/OneDrive - KU Leuven/PhD_papers/fAnnot/figures/fig1/subplotB/host_lineage_correctedCounts_lineageFormatted.txt", sep = '\t', header = TRUE, dec=",")
sankey_data2 <- df2

#prepare file for Pavian
sankey_data2 <- sankey_data2 %>% 
  unite(col = "classification", c(Domain, Phylum, Class, Order, Family, Genus, Species, Below_species_class), sep='; ') %>% 
  mutate_at("classification", str_replace, "Bacteria", "d__Bacteria") %>% 
  mutate_at("classification", str_replace, "; ", "|k__") %>% 
  mutate_at("classification", str_replace, "; ", "|p__") %>% 
  mutate_at("classification", str_replace, "; ", "|c__") %>% 
  mutate_at("classification", str_replace, "; ", "|o__") %>%
  mutate_at("classification", str_replace, "; ", "|f__") %>% 
  mutate_at("classification", str_replace, "; ", "|g__") %>% 
  mutate_at("classification", str_replace, "; ", "|s__") 

# Create format for Pavian with counts for each taxonomic level
# we're cheating here and actually not using the correct level because I also want to show realm
sankey_data_d2 <- sankey_data2
sankey_data_d2$classification <- sub("\\|k__.*", "", sankey_data_d2$classification)  
sankey_data_k2 <- sankey_data2
sankey_data_k2$classification <- sub("\\|p__.*", "", sankey_data_k2$classification)  
sankey_data_p2 <- sankey_data2
sankey_data_p2$classification <- sub("\\|c__.*", "", sankey_data_p2$classification) 
sankey_data_c2 <- sankey_data2
sankey_data_c2$classification <- sub("\\|o__.*", "", sankey_data_c2$classification) 
sankey_data_o2 <- sankey_data2
sankey_data_o2$classification <- sub("\\|f__.*", "", sankey_data_o2$classification) 
sankey_data_f2 <- sankey_data2
sankey_data_f2$classification <- sub("\\|g__.*", "", sankey_data_f2$classification) 
sankey_data_g2 <- sankey_data2
sankey_data_g2$classification <- sub("\\|s__.*", "", sankey_data_g2$classification) 
sankey_data_s2 <- sankey_data2

sankey_data2_upToGenus <- sankey_data_g2 %>%
  group_by(classification) %>%
  summarise(count_tax = sum(size_corrected))

sankey_data2_upToFamily <- sankey_data_f2 %>%
  group_by(classification) %>%
  summarise(count_tax = sum(size_corrected))

sankey_data2_upToOrder <- sankey_data_o2 %>%
  group_by(classification) %>%
  summarise(count_tax = sum(size_corrected))

sankey_data2_upToClass <- sankey_data_c2 %>%
  group_by(classification) %>%
  summarise(count_tax = sum(size_corrected))

sankey_data2_upToPhylum <- sankey_data_p2 %>%
  group_by(classification) %>%
  summarise(count_tax = sum(size_corrected))

sankey_data2_upToKingdom <- sankey_data_k2 %>%
  group_by(classification) %>%
  summarise(count_tax = sum(size_corrected))

sankey_data2_upToDomain <- sankey_data_d2 %>%
  group_by(classification) %>%
  summarise(count_tax = sum(size_corrected))

sankey_data2_upToSpecies <- sankey_data_s2 %>%
  rename(`count_tax` = `size_corrected`) 

sankey_data2_allTaxa <- bind_rows(sankey_data2_upToSpecies, sankey_data2_upToGenus, sankey_data2_upToFamily, sankey_data2_upToOrder, sankey_data2_upToClass, sankey_data2_upToPhylum, sankey_data2_upToKingdom, sankey_data2_upToDomain) %>%
  rename(`#SampleID` = `classification`) %>% 
  rename(`Metaphlan2_Analysis` = `count_tax`) 

# store file
write_tsv(sankey_data2_allTaxa, "C:/Users/hanne/OneDrive - KU Leuven/PhD_papers/fAnnot/figures/fig1/subplotB/host_input_pavian.txt")
