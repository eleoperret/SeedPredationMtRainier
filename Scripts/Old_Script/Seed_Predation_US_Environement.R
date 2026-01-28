####Code for CAMERA TRAP EXPERIMENT US 2017
####Autumn 2023
####ETHZ Eléonore Perret
####dataset containing infos for camera trap experiment US
##In this code, I want to look at the dbh and create plots (the mean dbh per camera is created in Script_4_Conspecific_US_2023)
#I first look at the dbh per site, the dbh per species, the dbh per site for 5m, the dbh per camera 5m and finally see how they differ from each other. Is it a good representation of the dbh of the site ?

#Loading the library needed
library(dplyr)
library(ggplot2)
#install.packages("vegan")
library(vegan)
library(tidyr)

setwd("C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets")
#Loading the data to be treated
load("data_site_info.RData")
load("data_cleaned_2.RData")
load("all_tree_data.RData")
load("tree_trap_5m.RData") #All the trees that are around my camera
load("tree_growth_data_2017_sites.RData") #Only the 3 sites I need


# OVERALL -----------------------------------------------------------------
#Using the tree_growth_data_2017_sites dataset
head(tree_growth_data_2017_sites)
# 1. DBH
# Calculate total and count-based averages for each species
avg_dbh_species <- tree_growth_data_2017_sites %>%
  group_by(species) %>%
  summarise(
    total_dbh = sum(as.numeric(gsub(",", ".", dbh)), na.rm = TRUE), # Total DBH
    count = n(), # Count of observations
    avg_dbh = total_dbh / count # Average DBH
  )
avg_dbh_site <- tree_growth_data_2017_sites %>%
  group_by(stand_id) %>%
  summarise(
    total_dbh = sum(as.numeric(gsub(",", ".", dbh)), na.rm = TRUE), # Total DBH
    count = n(), # Count of observations
    avg_dbh = total_dbh / count # Average DBH
  )
print(avg_dbh_site)
avg_dbh_species <- avg_dbh_species %>%
  arrange(desc(avg_dbh)) %>%
  mutate(species = factor(species, levels = unique(species)))  # Reorder factor levels
species_colors <- c(
  "ABLA" = "#8DD3C7",
  "ABGR" = "#FFFFB3",
  "TSME" = "#BEBADA",
  "THPL" = "#FB8072",
  "ABAM" = "#80B1D3",
  "CANO" = "#FDB462",
  "PSME" = "#B3DE69",
  "ABPR" = "#FCCDE5",
  "TABR" = "#D9D9D9",
  "TSHE" = "#BC80BD"
)
ggplot(avg_dbh_species, aes(x = species, y = avg_dbh, fill = species)) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = "Average DBH for Each Species",
    x = "Species",
    y = "Average DBH (cm)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  scale_fill_manual(values = species_colors) # Predefined color palette

ggplot(avg_dbh_site, aes(x = stand_id, y = avg_dbh)) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = "Average DBH for Each Site",
    x = "Site",
    y = "Average DBH (cm)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) 

# Calculate total and count-based averages for each species and site
avg_dbh_species_site <- tree_growth_data_2017_sites %>%
  group_by(species, stand_id) %>%
  summarise(
    total_dbh = sum(as.numeric(gsub(",", ".", dbh)), na.rm = TRUE), # Total DBH
    count = n(), # Count of observations
    avg_dbh = total_dbh / count # Average DBH
  )
avg_dbh_species_site <- avg_dbh_species_site %>%
  group_by(stand_id) %>%
  arrange(desc(avg_dbh), .by_group = TRUE) %>%
  mutate(species = factor(species, levels = unique(species)))  # Reorder within each stand_id
# Plot average dbh for each species at each site
ggplot(avg_dbh_species_site, aes(x = stand_id, y = avg_dbh, fill = species)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = "Average DBH for Each Species at Each Site",
    x = "Site",
    y = "Average DBH (cm)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1)
  )+
  scale_fill_manual(values = species_colors) # Predefined color palette



#2. Density
# Count the number of trees per stand
tree_density_per_stand <- tree_growth_data_2017_sites %>%
  group_by(stand_id) %>%
  summarise(
    tree_count = n()  # Count the number of trees (observations) per stand
  )
tree_density_per_stand <- tree_growth_data_2017_sites %>%
  group_by(stand_id) %>%
  summarise(
    tree_count = n(),
    stand_area = 100*100,  
    density = tree_count / stand_area  # Tree density (trees per hectare)
  )
# Count trees per species per stand
tree_density_per_species_stand <- tree_growth_data_2017_sites %>%
  group_by(stand_id, species) %>%
  summarise(
    species_tree_count = n()  # Count the number of trees for each species in each stand
  )
tree_density_per_species_stand <- tree_growth_data_2017_sites %>%
  group_by(stand_id, species) %>%
  summarise(
    species_tree_count = n(),
    stand_area = 100*100,  
    species_density = species_tree_count / stand_area  # Density per species
  )


# Plot tree density per stand (number of trees)
ggplot(tree_density_per_stand, aes(x = stand_id, y = tree_count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(
    title = "Tree Density Per Stand",
    x = "Stand ID",
    y = "Number of Trees"
  )
# Plot tree density per stand (trees per hectare)
ggplot(tree_density_per_stand, aes(x = stand_id, y = density)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(
    title = "Tree Density Per Stand (Trees per Hectare)",
    x = "Stand ID",
    y = "Density (Trees per Hectare)"
  )
# Plot tree density per species per stand (number of trees)
ggplot(tree_density_per_species_stand, aes(x = interaction(stand_id, species), y = species_tree_count, fill = species)) +
  geom_bar(stat = "identity", color = "black") +
  theme_minimal() +
  labs(
    title = "Tree Density Per Species Per Stand",
    x = "Stand and Species",
    y = "Number of Trees"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = species_colors) # Predefined color palette
  
# Plot tree density per species per stand (trees per hectare)
ggplot(tree_density_per_species_stand, aes(x = interaction(stand_id, species), y = species_density, fill = species)) +
  geom_bar(stat = "identity", color = "black") +
  theme_minimal() +
  labs(
    title = "Species Density Per Stand (Trees per Hectare)",
    x = "Stand and Species",
    y = "Density (Trees per Hectare)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = species_colors) # Predefined color palette



#3. Diversity
#ALL THE SITES: For the whole site in general : not per camera 
# Shannon Index (SI) calculation
# Add a relative abundance column
tree_growth_data_2017_sites <- tree_growth_data_2017_sites %>%
  group_by(stand_id, species) %>%
  summarise(
    count = n(),  # Count of each species per site
    .groups = "drop"
  ) %>%
  group_by(stand_id) %>%
  mutate(
    total_count = sum(count),  # Total count of trees per site
    relative_abundance = count / total_count  # Proportion of each species
  )
# Calculate diversity metrics
site_diversity <- tree_growth_data_2017_sites %>%
  group_by(stand_id) %>%
  summarise(
    shannon_index = -sum(relative_abundance * log(relative_abundance), na.rm = TRUE),  # Shannon Index
    species_richness = n_distinct(species),  # Number of unique species
    evenness_index = shannon_index / log(species_richness)  # Evenness Index
  )
# View the results
site_diversity



# Only trees around 5m of my cameras --------------------------------------
#Using the all_tree_data dataset
head(all_tree_data)
# Remove specific columns by name
all_tree_data <- all_tree_data %>%
  select(-geometry, -dbcode, -entity, -tree_id, -psp_studyid, -quarter, -crown_ratio, -crown_pct, -tree_pct, -db_notes, -new_mapping, -tree_id.x, -stand_idplot.x, -stand_idplot.y) 

unique(all_tree_data$species)
unique(all_tree_data$species.y)

# 1. DBH
# Calculate total and count-based averages for each species
avg_dbh_species_2 <- all_tree_data %>%
  group_by(species.y) %>%
  summarise(
    total_dbh_2 = sum(as.numeric(gsub(",", ".", dbh)), na.rm = TRUE), # Total DBH
    count_2 = n(), # Count of observations
    avg_dbh_2 = total_dbh_2 / count_2 # Average DBH
  )
avg_dbh_species_2 <- avg_dbh_species_2 %>%
  arrange(desc(avg_dbh_2)) %>%
  mutate(species = factor(species.y, levels = unique(species.y)))  # Reorder factor levels
species_colors <- c(
  "ABLA" = "#8DD3C7",
  "ABGR" = "#FFFFB3",
  "TSME" = "#BEBADA",
  "THPL" = "#FB8072",
  "ABAM" = "#80B1D3",
  "CANO" = "#FDB462",
  "PSME" = "#B3DE69",
  "ABPR" = "#FCCDE5",
  "TABR" = "#D9D9D9",
  "TSHE" = "#BC80BD"
)
ggplot(avg_dbh_species_2, aes(x = species.y, y = avg_dbh_2, fill = species.y)) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = "Average DBH for Each Species around 5m of the camera",
    x = "Species",
    y = "Average DBH (cm)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  scale_fill_manual(values = species_colors) # Predefined color palette

# Calculate total and count-based averages for each species and site
avg_dbh_species_site_2 <- all_tree_data %>%
  group_by(species.y, stand_id) %>%
  summarise(
    total_dbh_2 = sum(as.numeric(gsub(",", ".", dbh)), na.rm = TRUE), # Total DBH
    count_2 = n(), # Count of observations
    avg_dbh_2 = total_dbh_2 / count_2 # Average DBH
  )
avg_dbh_species_site_2 <- avg_dbh_species_site_2 %>%
  group_by(stand_id) %>%
  arrange(desc(avg_dbh_2), .by_group = TRUE) %>%
  mutate(species.y = factor(species.y, levels = unique(species.y)))  # Reorder within each stand_id
# Plot average dbh for each species at each site
ggplot(avg_dbh_species_site_2, aes(x = stand_id, y = avg_dbh_2, fill = species.y)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = "Average DBH for Each Species at Each Site around 5m of camera",
    x = "Site",
    y = "Average DBH (cm)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1)
  )+
  scale_fill_manual(values = species_colors) # Predefined color palette



#2. Density
# Count the number of trees per stand
tree_density_per_stand_2 <- all_tree_data %>%
  group_by(stand_id) %>%
  summarise(
    tree_count_2 = n()  # Count the number of trees (observations) per stand
  )
tree_density_per_stand_2 <- all_tree_data %>%
  group_by(stand_id) %>%
  summarise(
    tree_count_2 = n(),
    stand_area_2 = 100*100,  
    density_2 = tree_count_2 / stand_area_2  # Tree density (trees per hectare)
  )
# Count trees per species per stand
tree_density_per_species_stand_2 <- all_tree_data %>%
  group_by(stand_id, species.y) %>%
  summarise(
    species_tree_count_2 = n()  # Count the number of trees for each species in each stand
  )
tree_density_per_species_stand_2 <- all_tree_data %>%
  group_by(stand_id, species.y) %>%
  summarise(
    species_tree_count_2 = n(),
    stand_area_2 = 100*100,  
    species_density_2 = species_tree_count_2 / stand_area_2  # Density per species
  )


# Plot tree density per stand (number of trees)
ggplot(tree_density_per_stand_2, aes(x = stand_id, y = tree_count_2)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(
    title = "Tree Density Per Stand around 5m of camera",
    x = "Stand ID",
    y = "Number of Trees"
  )
# Plot tree density per stand (trees per hectare)
ggplot(tree_density_per_stand_2, aes(x = stand_id, y = density_2)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(
    title = "Tree Density Per Stand (Trees per Hectare) around 5m of camera",
    x = "Stand ID",
    y = "Density (Trees per Hectare)"
  )
# Plot tree density per species per stand (number of trees)
ggplot(tree_density_per_species_stand_2, aes(x = interaction(stand_id, species.y), y = species_tree_count_2, fill = species.y)) +
  geom_bar(stat = "identity", color = "black") +
  theme_minimal() +
  labs(
    title = "Tree Density Per Species Per Stand around 5m of camera",
    x = "Stand and Species",
    y = "Number of Trees"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = species_colors) # Predefined color palette

# Plot tree density per species per stand (trees per hectare)
ggplot(tree_density_per_species_stand_2, aes(x = interaction(stand_id, species.y), y = species_density_2, fill = species.y)) +
  geom_bar(stat = "identity", color = "black") +
  theme_minimal() +
  labs(
    title = "Species Density Per Stand (Trees per Hectare) around 5m of camera",
    x = "Stand and Species",
    y = "Density (Trees per Hectare)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = species_colors) # Predefined color palette



#3. Diversity
#ALL THE SITES: For the whole site in general : not per camera 
# Shannon Index (SI) calculation
# Add a relative abundance column
all_tree_data_2 <- all_tree_data %>%
  group_by(stand_id, species.y) %>%
  summarise(
    count_2 = n(),  # Count of each species per site
    .groups = "drop"
  ) %>%
  group_by(stand_id) %>%
  mutate(
    total_count_2 = sum(count_2),  # Total count of trees per site
    relative_abundance_2 = count_2 / total_count_2  # Proportion of each species
  )
# Calculate diversity metrics
site_diversity_2 <- all_tree_data_2 %>%
  group_by(stand_id) %>%
  summarise(
    shannon_index_2 = -sum(relative_abundance_2 * log(relative_abundance_2), na.rm = TRUE),  # Shannon Index
    species_richness_2 = n_distinct(species.y),  # Number of unique species
    evenness_index_2 = shannon_index_2 / log(species_richness_2)  # Evenness Index
  )
# View the results
site_diversity_2



# CONSPECIFIC -------------------------------------------------------------

head(all_tree_data)

#I want only the species that are present in a 5m around my camera and that are corresponding to one of the species that I presented in the cafeteria trials. 
all_tree_data_cons<- all_tree_data %>%
  filter(species.y %in% c("ABAM","CANO","PSME","TSHE"))
#Now I want to know per camera (or tag) which specie I have so I can then check if there is a higher removal rate when there is a conspecific in 5m around the tray.
# Create a list of species per tag
species_per_tag <- all_tree_data_cons %>%
  group_by(tag) %>%
  summarise(
    species_list = list(unique(species.y)), # Get unique species for each tag
    num_species = n_distinct(species.y)      # Count the number of species per tag
  )

head(tree_trap_5m_lol)
#I want to know which camera correspond to which tag so I can add this information later.
# Create a mapping of tag.x to tag.y
tag_mapping <- tree_trap_5m_lol %>%
  select(tag.x, Camera.number) %>%
  distinct() %>%# Use distinct to remove duplicate mappings
  rename ("tag" = "tag.x")


species_per_tag <- inner_join(species_per_tag, tag_mapping, by = "tag")
species_per_tag<-species_per_tag %>%
  rename("Camera"= "Camera.number")
data_cleaned_2_consp<-data_cleaned_2 %>%
  filter(Seed_sp %in% c("ABAM", "CANO", "PSME", "TSHE"))
data_cleaned_2_consp <- inner_join(data_cleaned_2_consp, species_per_tag, by = "Camera")

head(data_cleaned_2_consp)

#Presence or absence of conspecific
conspecific <- data_cleaned_2_consp %>%
  mutate(Matches_Species = apply(., 1, function(row) {
    # Use grepl to check if Seed_sp is in species_list
    ifelse(grepl(row["Seed_sp"], row["species_list"], ignore.case = TRUE), "Yes", "No")
  }))


sum_yes<-sum(conspecific$Matches_Species == "Yes")
sum_no<-sum(conspecific$Matches_Species=="No")
conspecific_presence<-rbind(sum_yes,sum_no, colnames("Conspecific_presence"))
barplot(c(sum_yes, sum_no), 
        names.arg = c("Yes", "No"),
        col = c("green", "red"),
        main = "Count of Matches_Species",
        xlab = "Matches_Species",
        ylab = "Count")

# Calculate the total count
total_count <- sum_yes + sum_no
# Calculate the percentages
percent_yes <- sum_yes / total_count * 100
percent_no <- sum_no / total_count * 100
# Create a bar plot with percentages
conspecfic_removal<-barplot(c(percent_yes, percent_no), 
                            names.arg = c("Yes", "No"),
                            col = c("green", "red"),
                            main = "Percentage of Matches_Species",
                            xlab = "Matches_Species",
                            ylab = "Percentage (%)",
                            ylim = c(0, 100))

head(conspecific)


# Calculate average seed removal for each species and Matches_Species condition
removal_comparison_species <- conspecific %>%
  group_by(Seed_sp, Matches_Species) %>%
  summarise(
    mean_removal = mean(removal_per_all, na.rm = TRUE),
    median_removal = median(removal_per_all, na.rm = TRUE),
    count = n()
  )
# Create a box plot to compare removal_per_all by Seed_sp and Matches_Species (Yes/No)
ggplot(conspecific, aes(x = Seed_sp, y = removal_per_all, fill = Matches_Species)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +  # position_dodge to place boxes next to each other
  labs(title = "Seed Removal Comparison by Species and Species Match",
       x = "Species",
       y = "Removal Percentage") +
  theme_minimal() +
  scale_fill_manual(values = c("Yes" = "skyblue", "No" = "lightcoral")) +  # Custom colors
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Perform the Wilcoxon test per species and extract detailed results
stat_tests <- conspecific %>%
  group_by(Seed_sp) %>%
  summarise(
    wilcox_test = list(wilcox.test(removal_per_all ~ Matches_Species, data = .)),
    p_value = wilcox_test[[1]]$p.value,
    statistic = wilcox_test[[1]]$statistic,
    method = wilcox_test[[1]]$method,
    .groups = 'drop'
  )

# View the detailed results
stat_tests


conspecific %>%
  group_by(Seed_sp, Matches_Species) %>%
  summarise(mean_removal = mean(removal_per_all), 
            sd_removal = sd(removal_per_all))
