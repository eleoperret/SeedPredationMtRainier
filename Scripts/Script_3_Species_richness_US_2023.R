####Code for CAMERA TRAP EXPERIMENT US 2017
####Autumn 2023
####ETHZ Eléonore Perret
####dataset containing infos for camera trap experiment US
##In this code, I want to look at species richness
#This code ends with a dataset call data_merged_4 which contains information on the tree density and species richness

#Loading the library needed
library(dplyr)
library(ggplot2)
 
#Loading the data to be treated
load("C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/data_merged.RData")
load("C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/tree_gowth_data_2017.RData")
load("C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/to04_data.RData")
load("C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/av06_data.RData")
load("C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/ae10_data.RData")


#Creating subplot for each of my sites.
to04_data_tree <- tree_gowth_data_2017[tree_gowth_data_2017$stand_id == "TO04", ]
save(to04_data_tree, file = "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/to04_data_tree.RData")
av06_data_tree <- tree_gowth_data_2017[tree_gowth_data_2017$stand_id == "AV06", ]
save(av06_data_tree, file = "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/av06_data_tree.RData")
ae10_data_tree <- tree_gowth_data_2017[tree_gowth_data_2017$stand_id == "AE10", ]
save(ae10_data_tree, file = "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/ae10_data_tree.RData")


# SPECIES_DIVERSITY (ALL SITE) -----------------------------------------------------------------------
#ALL THE SITES: For the whole site in general : not per camera 
#Percentage of each specie per site
species_count_site_ae10 <- ae10_data_tree %>%
  group_by(species) %>%
  summarize(count = n_distinct(species),
            percentage = (count / n()) * 100)
#Percentage of each specie per site
species_count_site_av06 <- av06_data_tree %>%
  group_by(species) %>%
  summarize(count = n_distinct(species),
            percentage = (count / n()) * 100)
#Percentage of each specie per site
species_count_site_to04 <- to04_data_tree %>%
  group_by(species) %>%
  summarize(count = n_distinct(species),
            percentage = (count / n()) * 100)

#DIVERSITY INDEX: SHANNON DIVERSITY INDEX
#Species diversity for the whole site in general
# Species diversity for TO04(all site)
species_vector_to04_all <- to04_data_tree$species
# Calculate species frequencies
species_freq_to04_all <- table(species_vector_to04_all)
# Calculate species proportions
species_prop_to04_all <- prop.table(species_freq_to04_all)
# Calculate Shannon-Wiener Index
shannon_index_to04_all <- -sum(species_prop_to04_all * log(species_prop_to04_all))
# Print the Shannon-Wiener Index
cat("Shannon-Wiener Index:", shannon_index_to04_all)
# Species diversity for av06 (all site)
species_vector_av06_all <- av06_data_tree$species
species_freq_av06_all <- table(species_vector_av06_all)
species_prop_av06_all <- prop.table(species_freq_av06_all)
shannon_index_av06_all <- -sum(species_prop_av06_all * log(species_prop_av06_all))
cat("Shannon-Wiener Index:", shannon_index_av06_all)
# Species diversity for ae10(all site)
species_vector_ae10_all <- ae10_data_tree$species
species_freq_ae10_all <- table(species_vector_ae10_all)
species_prop_ae10_all <- prop.table(species_freq_ae10_all)
shannon_index_ae10_all <- -sum(species_prop_ae10_all * log(species_prop_ae10_all))
cat("Shannon-Wiener Index:", shannon_index_ae10_all)


# SPECIES_DIVERSITY (5M)-------------------------------------------------------
#AMOUNT OF SPECIES AROUND THE CAMERA PER SITE
species_count_by_camera_to04 <- to04_data %>%
  group_by(Camera.number) %>%
  summarise(number_of_species = n_distinct(species.y))
species_count_by_camera_av06 <- av06_data %>%
  group_by(Camera.number) %>%
  summarise(number_of_species = n_distinct(species.y))
species_count_by_camera_ae10 <- ae10_data %>%
  group_by(Camera.number) %>%
  summarise(number_of_species = n_distinct(species.y))
#Merging all results together
species_count_by_camera_ae10$Site<-"ae10"
species_count_by_camera_av06$Site<-"av06"
species_count_by_camera_to04$Site<-"to04"
merged_species_count_by_camera<-rbind(species_count_by_camera_ae10,species_count_by_camera_av06,species_count_by_camera_to04)
species_count_by_camera <- merged_species_count_by_camera %>%
  group_by(Site) %>%
  summarise(number_of_species = n_distinct(number_of_species))
# Now you can perform the merge
data_merged_2 <- merge(data_merged, merged_species_count_by_camera, by.x = "Camera.number", by.y= "Camera.number")
data_merged_2$species_diversity_camera <- data_merged_2$number_of_species

#TYPE OF SPECIES AROUND THE CAMERA PER SITE
#Type of species around the camera at each plot
species_by_camera_to04 <- to04_data %>%
  group_by(Camera.number) %>%
  distinct(species.y)
species_by_camera_av06 <- av06_data %>%
  group_by(Camera.number) %>%
  distinct(species.y)
species_by_camera_ae10 <- ae10_data %>%
  group_by(Camera.number) %>%
  distinct(species.y)
#Merging all results together
species_by_camera_ae10$Site<-"ae10"
species_by_camera_av06$Site<-"av06"
species_by_camera_to04$Site<-"to04"
merged_species_by_camera<-rbind(species_by_camera_ae10,species_by_camera_av06,species_by_camera_to04)
species_by_site<- merged_species_by_camera %>%
  group_by(Site) %>%
  distinct(species.y)

species_list <- merged_species_by_camera %>%
  group_by(Camera.number) %>%
  summarize(SpeciesList = list(unique(species.y))) %>%
  ungroup()

# Merge 'data_merged_2' and 'species_list' on the 'Camera' column
data_merged_3 <- merge(data_merged_2, species_list, by.x = "Camera.number", by.y = "Camera.number", all.x = TRUE)
data_merged_4 <- data_merged_3 %>%
  select(-Site.y, -Site.x, -number_of_species)


#SHANNON-INDEX
#Species diversity for around 5m of the camera traps (using the dataset for the trees around 5m)
# Species diversity for TO04 (5m)
species_vector_to04 <- to04_data$species.y
species_freq_to04 <- table(species_vector_to04)
species_prop_to04 <- prop.table(species_freq_to04)
shannon_index_to04 <- -sum(species_prop_to04 * log(species_prop_to04))
cat("Shannon-Wiener Index:", shannon_index_to04)
# Species diversity for av06(5m)
species_vector_av06 <- av06_data$species.y
species_freq_av06 <- table(species_vector_av06)
species_prop_av06 <- prop.table(species_freq_av06)
shannon_index_av06 <- -sum(species_prop_av06 * log(species_prop_av06))
cat("Shannon-Wiener Index:", shannon_index_av06)
# Species diversity for ae10(5m)
species_vector_ae10 <- ae10_data$species.y
species_freq_ae10 <- table(species_vector_ae10)
species_prop_ae10 <- prop.table(species_freq_ae10)
shannon_index_ae10 <- -sum(species_prop_ae10 * log(species_prop_ae10))
cat("Shannon-Wiener Index:", shannon_index_ae10)

#Shannon_index for the sites (trees within 5m)
shannon_data <- data.frame(
  Site = c("TO04", "AV06", "AE10"),
  Shannon_Index_5m = c(shannon_index_to04, shannon_index_av06, shannon_index_ae10)
)
# Print the new data frame
print(shannon_data)
# Merge the new data frame with the existing dataset based on the 'Site' column
data_merged_4 <- merge(data_merged_4, shannon_data,by.x = "stand_id" ,by.y = "Site")
# Save the updated dataset
save(data_merged_4, file = "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/data_merged_4.RData")





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

