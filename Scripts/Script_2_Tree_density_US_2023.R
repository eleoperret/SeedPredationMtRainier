####Code for CAMERA TRAP EXPERIMENT US 2017
####Autumn 2023
####ETHZ Eléonore Perret
####dataset containing infos for camera trap experiment US
##In this code, I want to look at density of trees
#I created a dataset "data site info" where I added the density of trees for each camera and also have the density of trees in each stand

library(dplyr)
library(ggplot2)

setwd("C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets")


#Density of trees.
#Loading the data to be treated
load("all_tree_data.RData")
load("tree_growth_data_2017.RData")
#Tree growth data is Cleaned_tree_growth_2017 Rainier.csv just only for the year 2017. There are more years (each year the survey was done) in the original dataset so I cropped it do the last one, which is the last state of the forest. New one 2024 to come... 
#Renaming for simplicity. Every time there will be tree growth data in this code, it is the tree growth data from 2017. 
#All tree data is the dataset that contains information about the trees in 5 m around the cameras. 
tree_growth_data<-tree_growth_data_2017

#Cleaning the data set for more clarity
data_tree_filtered <- all_tree_data[, !colnames(all_tree_data) %in% c("dbcode","entity", "tree_id","psp_studyid","quarter","crown_ratio", "main_stem","rooting","crown_pct","tree_pct","lean_angle","db_notes","new_mapping","stand_idplot.x","AreaCookie.x","year_count.x","year_count.y","size_cat.y","geometry","check_notes","tree_id.x","tree_id.y","Notes","AreaCookie.y","Lat","Long","Subplot..e.g...25.25..","stand_id.x","plot.x","species.x","year.x","tree_status.x","dbh.x","size_cat.x","tree_vigor","tree_status","Stand","stand_id.y","tree_status.y","sampledate","stand_idplot.y","dbh_code","plot","plot.y")]
save(data_tree_filtered, file = "C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/data_tree_filtered.RData")

#Creating subplot for each of my sites.
to04_data <- data_tree_filtered[data_tree_filtered$stand_id == "TO04", ]
save(to04_data, file = "C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/to04_data.RData")
av06_data <- data_tree_filtered[data_tree_filtered$stand_id == "AV06", ]
save(av06_data, file = "C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/av06_data.RData")
ae10_data <- data_tree_filtered[data_tree_filtered$stand_id == "AE10", ]
save(ae10_data, file = "C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/ae10_data.RData")


# Tree density per site from the 2017 data collection
tree_abundance <- tree_growth_data %>%
  filter(stand_id %in% c("AE10", "AV06", "TO04")) %>%
  group_by(stand_id) %>%
  summarize(total_trees_site = n(), .groups = "drop") # Total tree abundance per site
# Tree density per site per species
tree_abundance_per_species <- tree_growth_data %>%
  filter(stand_id %in% c("AE10", "AV06", "TO04")) %>%
  group_by(stand_id, species) %>%
  summarize(species_abundance = n(), .groups = "drop") # Abundance per species per site
plot(tree_abundance_per_species$species,tree_abundance_per_species$species_abundance, data= tree_abundance_per_species)
# Create a plot for tree abundance per species
ggplot(tree_abundance_per_species, aes(x = species, y = species_abundance, fill = stand_id)) +
  geom_bar(stat = "identity", position = "dodge") + # Bar plot, dodged to separate the stands
  theme_minimal() + # Minimal theme for clarity
  labs(x = "Species", y = "Species Abundance", title = "Tree Density per Species per Site") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for readability
# Merge total tree abundance with species abundance
data_site_analysis <- tree_abundance_per_species %>%
  left_join(tree_abundance, by = "stand_id") %>%
  mutate(
    relative_abundance = species_abundance / total_trees_site # Calculate relative abundance
  )
# Save the dataset
save(data_site_analysis, file = "C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/data_site_analysis.RData")


barplot_tree_abundance_per_site<- ggplot(data_site_analysis, aes(x = as.factor(stand_id), y = relative_abundance)) +
  geom_bar(stat = "identity",fill = "skyblue") +
  labs(title = "Species abundance per sites", x = "Site", y = "Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~ species, scales = "free_y", ncol = 3)
# Create the bar plot for relative abundance per site, with a shared y-axis scale across species
barplot_tree_abundance_per_site <- ggplot(data_site_analysis, aes(x = as.factor(stand_id), y = relative_abundance, fill = species)) +
  geom_bar(stat = "identity", position = "stack") +  # Use stacked bars to show relative abundance of each species
  labs(title = "Species Abundance per Site", x = "Site", y = "Relative Abundance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~ species, ncol = 3) # Facet by species with a shared y-axis scale

# Print the plot
print(barplot_tree_abundance_per_site)

# Load RColorBrewer package for more color palettes
library(RColorBrewer)

# Set a color palette with more colors (e.g., 12 colors for species)
colors <- brewer.pal(12, "Set3")  # You can choose another palette if needed

# Create the bar plot with stacked species relative abundance and customized colors
barplot_tree_abundance_per_site <- ggplot(data_site_analysis, aes(x = as.factor(stand_id), y = relative_abundance, fill = species)) +
  geom_bar(stat = "identity", position = "stack") +  # Stacked bars for species
  scale_fill_manual(values = colors) +  # Apply custom color palette
  labs(title = "Species Abundance per Site", x = "Site", y = "Relative Abundance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for readability

# Print the plot
print(barplot_tree_abundance_per_site)


print(barplot_tree_abundance_per_site)
barplot_tree_abundance_per_site<- ggplot(data_site_analysis, aes(x = as.factor(stand_id), y = species_abundance)) +
  geom_bar(stat = "identity",fill = "skyblue") +
  labs(title = "Species abundance per sites", x = "Site", y = "Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~ species, scales = "free_y", ncol = 3)
print(barplot_tree_abundance_per_site)
ggsave(filename = "barplot_tree_abundance_per_site.png", plot = barplot_tree_abundance_per_site, path = "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Plots/")
print(barplot_tree_abundance_per_site)
#Sum for species and for species and site
sum_per_species <- combined_tree_abundance_site %>%
  group_by(species) %>%
  summarize(total_count = sum(tree_abundance_site))

#Plot per site. 
ggplot(data_site_analysis, aes(x = stand_id, y = relative_abundance, fill = species)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  labs(
    title = "Relative Abundance of Tree Species by Site",
    x = "Site (Stand ID)",
    y = "Relative Abundance",
    fill = "Species"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )


# INFOS ON THE SURROUNDING DENSITY AND COMPOSITION : Looking at the trees 5m around ------------------------------------------------------------
# TREE_DENSITY ------------------------------------------------------------
# #AMOUNT OF TREES AROUND THE CAMERAS PER SITE
# #Tree density around my cameras
# #Amount of tree in 5m area for each camera at plot to04
# tree_count_by_camera_to04 <- to04_data %>%
#   group_by(Camera.number) %>%
#   summarise(number_of_trees = n_distinct(tag.y.y))
# #Amount of tree in 5m area for each camera at plot av06
# tree_count_by_camera_av06 <- av06_data %>%
#   group_by(Camera.number) %>%
#   summarise(number_of_trees = n_distinct(tag.y.y))
# #Amount of tree in 5m area for each camera at plot ae10
# tree_count_by_camera_ae10 <- ae10_data %>%
#   group_by(Camera.number) %>%
#   summarise(number_of_trees = n_distinct(tag.y.y))
# #Merging all results together
# #Add a site name column to each data frame
# tree_count_by_camera_ae10$Site <- "ae10"
# tree_count_by_camera_av06$Site <- "av06"
# tree_count_by_camera_to04$Site <- "to04"
# merged_tree_count_by_camera<-rbind(tree_count_by_camera_ae10,tree_count_by_camera_av06,tree_count_by_camera_to04)
# tree_count_by_site <- merged_tree_count_by_camera %>%
#   group_by(Site) %>%
#   summarise(number_of_trees = mean(number_of_trees))
# #View(merged_tree_count_by_camera)
# # Now you can perform the merge
# data_merged <- merge(data_tree_filtered, merged_tree_count_by_camera, by.x = "Camera.number", by.y= "Camera.number")
# # Create a ggplot to visualize the number of trees per camera per site
# ggplot(merged_tree_count_by_camera, aes(x = Camera.number, y = number_of_trees, fill = Site)) +
# geom_bar(stat = "identity", position = "dodge") +
#   labs(title = "Tree Counts by Camera and Site", x = "Camera Number", y = "Number of Trees") +
#   theme(legend.title = element_blank())
# ggsave(filename = "merged_tree_count_by_camera.png", plot =merged_tree_count_by_camera, path = "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Plots/")
# 
# save(data_merged, file = "C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/data_merged.RData")


#New code

# Tree density around my cameras from the selection
tree_abundance_2 <- all_tree_data %>%
  filter(stand_id %in% c("AE10", "AV06", "TO04")) %>%
  group_by(stand_id) %>%
  summarize(total_trees_site = n(), .groups = "drop") # Total tree abundance per site
# Tree density per site per species
tree_abundance_per_species_2 <- all_tree_data %>%
  filter(stand_id %in% c("AE10", "AV06", "TO04")) %>%
  group_by(stand_id, species) %>%
  summarize(species_abundance = n(), .groups = "drop") # Abundance per species per site
# Merge total tree abundance with species abundance
data_site_analysis_2 <- tree_abundance_per_species_2 %>%
  left_join(tree_abundance_2, by = "stand_id") %>%
  mutate(
    relative_abundance = species_abundance / total_trees_site # Calculate relative abundance
  )
# Save the dataset
save(data_site_analysis_2, file = "C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/data_site_analysis_2.RData")

data_site_info <- merge(data_tree_filtered, data_site_analysis, by = c("stand_id", "species"), all = TRUE)
#Great now I have more information about the sites 
#I can also add the shannon index later 
#I first want to remove the rows where there was not a species from the tree present. 
data_site_info <- data_site_info[!is.na(data_site_info$Camera.number), ]

# Save the dataset
save(data_site_info, file = "C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/data_site_info.RData")
