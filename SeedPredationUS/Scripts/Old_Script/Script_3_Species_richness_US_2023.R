####Code for CAMERA TRAP EXPERIMENT US 2017
####Autumn 2023
####ETHZ Eléonore Perret
####dataset containing infos for camera trap experiment US
##In this code, I want to look at species richness
#This code ends with a dataset call data_merged_4 which contains information on the tree density and species richness

#Loading the library needed
library(dplyr)
library(ggplot2)

setwd("C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets")
 
#Loading the data to be treated
load("data_merged.RData")
load("tree_growth_data_2017.RData")
load("to04_data.RData")
load("av06_data.RData")
load("ae10_data.RData")
load("data_site_analysis.RData")
load("data_site_analysis_2.RData")

# SPECIES_DIVERSITY (ALL SITE) -----------------------------------------------------------------------
#ALL THE SITES: For the whole site in general : not per camera 
# Shannon Index (SI) calculation
shannon_index <- -sum(data_site_analysis$relative_abundance * log(data_site_analysis$relative_abundance))

# Evenness Index (E) calculation
species_richness <- n_distinct(data_site_analysis$species)
evenness_index <- shannon_index / log(species_richness)

# Calculate Shannon Index (SI) and Evenness Index (E) for each site
site_diversity <- data_site_analysis %>%
  group_by(stand_id) %>%
  summarise(
    shannon_index = -sum(relative_abundance * log(relative_abundance)),
    species_richness = n_distinct(species),
    evenness_index = shannon_index / log(species_richness)
  )

site_diversity

# SPECIES_DIVERSITY (5M)-------------------------------------------------------
#As I want to see the SI for the trees around my cameras but how they are representative of the overall surrounding, I need to change a bit the data_site_analysis_2 dataset which contains relative abundance based on the 5m area around the cameras and not the real relative abundance based on all trees around. 

data_site_analysis_2 <- data_site_analysis_2 %>%
  mutate(total_tree_site_overall = case_when(
    stand_id == "AE10" ~ 575,
    stand_id == "AV06" ~ 656,
    stand_id == "TO04" ~ 268,
    TRUE ~ NA_real_  # To handle any other cases if needed
  ))

data_site_analysis_2 <- data_site_analysis_2 %>%
  mutate(relative_abundance_2 = species_abundance / total_tree_site_overall)

# Shannon Index (SI) calculation based on only the trees around and at 5 m
shannon_index_2 <- -sum(data_site_analysis_2$relative_abundance * log(data_site_analysis_2$relative_abundance))
# Evenness Index (E) calculation
species_richness_2 <- n_distinct(data_site_analysis_2$species)
evenness_index_2 <- shannon_index_2 / log(species_richness_2)
# Calculate Shannon Index (SI) and Evenness Index (E) for each site
site_diversity_2 <- data_site_analysis_2 %>%
  group_by(stand_id) %>%
  summarise(
    shannon_index_2 = -sum(relative_abundance * log(relative_abundance)),
    species_richness_2 = n_distinct(species),
    evenness_index_2 = shannon_index_2 / log(species_richness_2)
  )

# Shannon Index (SI) calculation based on the trees around 5m relative to overall forest
shannon_index_3 <- -sum(data_site_analysis_2$relative_abundance_2 * log(data_site_analysis_2$relative_abundance_2))
# Evenness Index (E) calculation
species_richness_3 <- n_distinct(data_site_analysis_2$species)
evenness_index_3 <- shannon_index_3 / log(species_richness_3)
# Calculate Shannon Index (SI) and Evenness Index (E) for each site
site_diversity_3 <- data_site_analysis_2 %>%
  group_by(stand_id) %>%
  summarise(
    shannon_index_3 = -sum(relative_abundance_2 * log(relative_abundance_2)),
    species_richness_3 = n_distinct(species),
    evenness_index_3 = shannon_index_3 / log(species_richness_3)
  )

site_diversity;site_diversity_2;site_diversity_3

#Not sure it is necessary to do this? 

# # Merge the new data frame with the existing dataset based on the 'Site' column
data_site_info <- merge(data_site_info, site_diversity,by= "stand_id")
# # Save the updated dataset
# save(data_merged_4, file = "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/data_merged_4.RData")

#ADDING all the data together


# PLOTS -------------------------------------------------------------------
#To plot everything: 
shannon_index_all<-list(ae10 = shannon_index_ae10_all, av06 = shannon_index_av06_all, to04 = shannon_index_to04_all)
shannon_index_all <- do.call(cbind, shannon_index_all)
shannon_index_5m<-list(ae10 = shannon_index_ae10, av06 = shannon_index_av06, to04 = shannon_index_to04)
shannon_index_5ml <- do.call(cbind, shannon_index_5m)
# Create a bar plot for 'shannon_index_all'
barplot(unlist(shannon_index_all), beside = TRUE, col = alpha("blue", 0.5), 
        main = "Shannon-Wiener Index Comparison",
        names.arg = colnames(shannon_index_all),
        xlab = "Data Sets", ylab = "Shannon-Wiener Index")
# Add bars for 'shannon_index_5ml' in a different color
barplot(unlist(shannon_index_5ml), beside = TRUE, col = alpha("black", 0.5), add = TRUE)
# Add a legend with improved appearance
legend("bottomright", legend = c("All Data", "5m Data"), fill = c(alpha("blue", 0.5), alpha("black", 0.5)))

