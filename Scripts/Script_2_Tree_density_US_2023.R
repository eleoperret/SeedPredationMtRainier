####Code for CAMERA TRAP EXPERIMENT US 2017
####Autumn 2023
####ETHZ Eléonore Perret
####dataset containing infos for camera trap experiment US
##In this code, I want to look at density of trees
#I created a dataset "datamerged" where I added the density of trees for each camera and also have the density of trees in each stand

library(dplyr)
library(ggplot2)

#Density of trees.

#Loading the data to be treated
load("C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/all_tree_data.RData")
load("C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/tree_gowth_data_2017.RData")
tree_gowth_data<-tree_gowth_data_2017

#Cleaning the data set for more clarity
data_tree_filtered <- all_tree_data[, !colnames(all_tree_data) %in% c("dbcode","entity", "tree_id","psp_studyid","quarter","crown_ratio", "main_stem","rooting","crown_pct","tree_pct","lean_angle","db_notes","new_mapping","stand_idplot.x","AreaCookie.x","year_count.x","year_count.y","size_cat.y","geometry","check_notes","tree_id.x","tree_id.y","Notes","AreaCookie.y","Lat","Long","Subplot..e.g...25.25..","stand_id.x","plot.x","species.x","year.x","tree_status.x","dbh.x","size_cat.x","tree_vigor","tree_status","Stand","stand_id.y","tree_status.y","sampledate","stand_idplot.y","dbh_code","plot","plot.y")]
save(data_tree_filtered, file = "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/data_tree_filtered.RData")

#Creating subplot for each of my sites.
to04_data <- data_tree_filtered[data_tree_filtered$stand_id == "TO04", ]
save(to04_data, file = "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/to04_data.RData")
av06_data <- data_tree_filtered[data_tree_filtered$stand_id == "AV06", ]
save(av06_data, file = "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/av06_data.RData")
ae10_data <- data_tree_filtered[data_tree_filtered$stand_id == "AE10", ]
save(ae10_data, file = "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/ae10_data.RData")

#Tree density per site from the 2017 data collection
tree_abundance <- tree_gowth_data %>%
  filter(stand_id %in% c("AE10", "AV06", "TO04")) %>%
  group_by(stand_id) %>%
  summarize(tree_abundance_site = n())
#tree density per site per species
tree_abundance_site<-tree_gowth_data%>%
  group_by(species, stand_id) %>%
  summarize (tree_abundance_site=n())
tree_abundance_site_ae10<-tree_abundance_site%>%
  filter(stand_id=="AE10")
tree_abundance_site_av06<-tree_abundance_site%>%
  filter(stand_id=="AV06")
tree_abundance_site_to04<-tree_abundance_site%>%
  filter(stand_id=="TO04")
combined_tree_abundance_site <- rbind(tree_abundance_site_ae10, tree_abundance_site_av06, tree_abundance_site_to04)
data_site_analysis<-merge(tree_abundance,combined_tree_abundance_site, by= "stand_id")
data_site_analysis$density_site<- data_site_analysis$tree_abundance_site.x
data_site_analysis<- subset(data_site_analysis, select = -tree_abundance_site.x)
save(data_site_analysis, file = "C:/Users/eleop/polybox/phD/PhD/RSeed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/data_site_analysis.RData")


barplot_tree_abundance_per_site<- ggplot(combined_tree_abundance_site, aes(x = as.factor(stand_id), y = tree_abundance_site)) +
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


# INFOS ON THE SURROUNDING DENSITY AND COMPOSITION : Looking at the trees 5m around ------------------------------------------------------------
# TREE_DENSITY ------------------------------------------------------------
#AMOUNT OF TREES AROUND THE CAMERAS PER SITE
#Tree density around my cameras
#Amount of tree in 5m area for each camera at plot to04
tree_count_by_camera_to04 <- to04_data %>%
  group_by(Camera.number) %>%
  summarise(number_of_trees = n_distinct(tag.y))
#Amount of tree in 5m area for each camera at plot av06
tree_count_by_camera_av06 <- av06_data %>%
  group_by(Camera.number) %>%
  summarise(number_of_trees = n_distinct(tag.y))
#Amount of tree in 5m area for each camera at plot ae10
tree_count_by_camera_ae10 <- ae10_data %>%
  group_by(Camera.number) %>%
  summarise(number_of_trees = n_distinct(tag.y))
#Merging all results together
#Add a site name column to each data frame
tree_count_by_camera_ae10$Site <- "ae10"
tree_count_by_camera_av06$Site <- "av06"
tree_count_by_camera_to04$Site <- "to04"
merged_tree_count_by_camera<-rbind(tree_count_by_camera_ae10,tree_count_by_camera_av06,tree_count_by_camera_to04)
tree_count_by_site <- merged_tree_count_by_camera %>%
  group_by(Site) %>%
  summarise(number_of_trees = mean(number_of_trees))
#View(merged_tree_count_by_camera)
# Now you can perform the merge
data_merged <- merge(data_tree_filtered, merged_tree_count_by_camera, by.x = "Camera.number", by.y= "Camera.number")
# Create a ggplot to visualize the number of trees per camera per site
ggplot(merged_tree_count_by_camera, aes(x = Camera.number, y = number_of_trees, fill = Site)) +
geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Tree Counts by Camera and Site", x = "Camera Number", y = "Number of Trees") +
  theme(legend.title = element_blank())
ggsave(filename = "merged_tree_count_by_camera.png", plot =merged_tree_count_by_camera, path = "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Plots/")

save(data_merged, file = "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/data_merged.RData")

