####Code for CAMERA TRAP EXPERIMENT US 2017
####Autumn 2023
####ETHZ Eléonore Perret
#### Code for measuring seed removal
##Last modifeid : 13.06.2025 for the plots of only camera traps position

# Loading librairies ------------------------------------------------------
install.packages("sf")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)

# Loading the data --------------------------------------------------------
# Set the working directory
setwd("C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2")

# List files in the main directory
list.files()
list.files("Datasets")

# Load datasets
seed_data <- load("Datasets/data_cleaned_2.RData")
seed_predation <- read.csv("Datasets/SeedPredation_First_week.csv", sep=";")
map_tree_rainier <- read.csv("Datasets/Cleaned_mapping_2017 Rainier.csv", sep=";")
tree_growth_data <- read.csv("Datasets/Cleaned_tree_growth_2017 Rainier.csv", sep=";")


str(map_tree_rainier)

# CAMERA TRAP DATA : Process_data -----------------------------------------------------------

seed_predation <- seed_predation %>% slice(1:27)

# Step 1: Select only relevant sites from full tree dataset
stand_data <- map_tree_rainier %>%
  filter(stand_id %in% c("AE10", "AV06", "TO04"))

# Step 2: Select trees with camera traps based on tag
tags_tree <- seed_predation$TreeTag
camera_trap_tree <- stand_data %>%
  filter(tag %in% tags_tree)

# For stand_data (tree data)
stand_data$x_coord <- gsub(",", ".", stand_data$x_coord)  # Replace comma with dot
stand_data$y_coord <- gsub(",", ".", stand_data$y_coord)
stand_data$x_coord <- as.numeric(stand_data$x_coord)  # Convert to numeric
stand_data$y_coord <- as.numeric(stand_data$y_coord)

# For camera_trap_tree (trap data)
camera_trap_tree$x_coord <- gsub(",", ".", camera_trap_tree$x_coord)  # Replace comma with dot
camera_trap_tree$y_coord <- gsub(",", ".", camera_trap_tree$y_coord)
camera_trap_tree$x_coord <- as.numeric(camera_trap_tree$x_coord)  # Convert to numeric
camera_trap_tree$y_coord <- as.numeric(camera_trap_tree$y_coord)

# Step 3: Create site-specific subsets
stand_data_to04 <- filter(stand_data, stand_id == "TO04")
stand_data_av06 <- filter(stand_data, stand_id == "AV06")
stand_data_ae10 <- filter(stand_data, stand_id == "AE10")
camera_trap_tree_to04 <- filter(camera_trap_tree, stand_id == "TO04")
camera_trap_tree_av06 <- filter(camera_trap_tree, stand_id == "AV06")
camera_trap_tree_ae10 <- filter(camera_trap_tree, stand_id == "AE10")

camera_sf_to04 <- st_as_sf(camera_trap_tree_to04, coords = c("x_coord", "y_coord"), crs = NA)
stand_sf_to04 <- st_as_sf(stand_data_to04, coords = c("x_coord", "y_coord"), crs = NA)

camera_sf_av06 <- st_as_sf(camera_trap_tree_av06, coords = c("x_coord", "y_coord"), crs = NA)
stand_sf_av06 <- st_as_sf(stand_data_av06, coords = c("x_coord", "y_coord"), crs = NA)

camera_sf_ae10 <- st_as_sf(camera_trap_tree_ae10, coords = c("x_coord", "y_coord"), crs = NA)
stand_sf_ae10 <- st_as_sf(stand_data_ae10, coords = c("x_coord", "y_coord"), crs = NA)

#FOR TOO4
# Create an empty list to store results
# Step 3: Create site-specific subsets
stand_data_to04 <- filter(stand_data, stand_id == "TO04")
stand_data_av06 <- filter(stand_data, stand_id == "AV06")
stand_data_ae10 <- filter(stand_data, stand_id == "AE10")
camera_trap_tree_to04 <- filter(camera_trap_tree, stand_id == "TO04")
camera_trap_tree_av06 <- filter(camera_trap_tree, stand_id == "AV06")
camera_trap_tree_ae10 <- filter(camera_trap_tree, stand_id == "AE10")

#Creating the subsets before (because I tried the other way and did not work :P)
camera_sf_to04 <- st_as_sf(camera_trap_tree_to04, coords = c("x_coord", "y_coord"), crs = NA)
stand_sf_to04 <- st_as_sf(stand_data_to04, coords = c("x_coord", "y_coord"), crs = NA)
camera_sf_av06 <- st_as_sf(camera_trap_tree_av06, coords = c("x_coord", "y_coord"), crs = NA)
stand_sf_av06 <- st_as_sf(stand_data_av06, coords = c("x_coord", "y_coord"), crs = NA)
camera_sf_ae10 <- st_as_sf(camera_trap_tree_ae10, coords = c("x_coord", "y_coord"), crs = NA)
stand_sf_ae10 <- st_as_sf(stand_data_ae10, coords = c("x_coord", "y_coord"), crs = NA)

#FOR TOO4
# Create an empty list to store results
trees_within_radius_to04 <- list()
# Loop over each camera trap and compute the distance to all trees
for (i in 1:nrow(camera_sf_to04)) {
  # Get the coordinates of the current camera trap
  camera_coords <- st_coordinates(camera_sf_to04[i, ])
  # Compute the distance to each tree
  tree_coords <- st_coordinates(stand_sf_to04)
  distances <- sqrt((tree_coords[, 1] - camera_coords[1])^2 + (tree_coords[, 2] - camera_coords[2])^2)
  # Convert to your coordinate system scale (if needed, e.g., 100x100 coordinate system)
  threshold_distance_custom <- 15  # Adjust this if you want in your custom coordinate system
  # Create a buffer around each tree with the defined threshold distance
  camera_buffer <- st_buffer(camera_sf_to04, dist = threshold_distance_custom)  # Buffer around trees
  # Now, find the trees that are within the buffer zone
  trees_within_buffer <- st_intersects(stand_sf_to04, camera_buffer, sparse = FALSE)
  # Store the result in the list (camera trap id and the trees within the buffer)
  trees_within_radius_to04[[i]] <- trees_within_buffer
}
# Combine the list of results into a single spatial data frame
trees_within_radius_to04_df <- do.call(rbind, trees_within_radius_to04)
str(trees_within_radius_to04_df)


# Convert the logical matrix into an index of selected trees (TRUE values)
selected_tree_indices <- which(trees_within_radius_to04_df == TRUE, arr.ind = TRUE)
# Extract the tree IDs or coordinates from the selected trees
selected_trees_to04 <- stand_sf_to04[selected_tree_indices[, 1], ]
# Visualize the result
ggplot() +
  # Plot all trees
  geom_sf(data = stand_sf_to04, color = "gray", size = 1, alpha = 0.6) +
  # Plot the selected trees within the buffer zone in green
 # geom_sf(data = selected_trees_to04, aes(color = "Within 15m"), size = 2) +
  # Plot camera traps in blue
  geom_sf(data = camera_sf_to04, aes(color = "Camera Trap"), shape = 21, size = 3, fill = "blue") +
  # Customize the color scale
  scale_color_manual(values = c("Camera Trap" = "blue", "Within 15m" = "forestgreen")) +
  # Add labels and theme
  labs(title = "Locations of Camera Traps for the low elevation stand",
       #subtitle = "Trees within 15m of Camera Traps",
       x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal() +
  theme(legend.position = "NA")


#FOR AV06
# Create an empty list to store results
trees_within_radius_av06 <- list()
# Loop over each camera trap and compute the distance to all trees
for (i in 1:nrow(camera_sf_av06)) {
  # Get the coordinates of the current camera trap
  camera_coords <- st_coordinates(camera_sf_av06[i, ])
  # Compute the distance to each tree
  tree_coords <- st_coordinates(stand_sf_av06)
  distances <- sqrt((tree_coords[, 1] - camera_coords[1])^2 + (tree_coords[, 2] - camera_coords[2])^2)
  # Convert to your coordinate system scale (if needed, e.g., 100x100 coordinate system)
  threshold_distance_custom <- 15  # Adjust this if you want in your custom coordinate system
  # Create a buffer around each tree with the defined threshold distance
  camera_buffer <- st_buffer(camera_sf_av06, dist = threshold_distance_custom)  # Buffer around trees
  # Now, find the trees that are within the buffer zone
  trees_within_buffer <- st_intersects(stand_sf_av06, camera_buffer, sparse = FALSE)
  # Store the result in the list (camera trap id and the trees within the buffer)
  trees_within_radius_av06[[i]] <- trees_within_buffer
}
# Combine the list of results into a single spatial data frame
trees_within_radius_av06_df <- do.call(rbind, trees_within_radius_av06)
str(trees_within_radius_av06_df)


# Convert the logical matrix into an index of selected trees (TRUE values)
selected_tree_indices <- which(trees_within_radius_av06_df == TRUE, arr.ind = TRUE)
# Extract the tree IDs or coordinates from the selected trees
selected_trees_av06 <- stand_sf_av06[selected_tree_indices[, 1], ]
# Visualize the result
ggplot() +
  # Plot all trees
  geom_sf(data = stand_sf_av06, color = "gray", size = 1, alpha = 0.6) +
  # Plot the selected trees within the buffer zone in green
  #geom_sf(data = selected_trees_av06, aes(color = "Within 15m"), size = 2) +
  # Plot camera traps in blue
  geom_sf(data = camera_sf_av06, aes(color = "Camera Trap"), shape = 21, size = 3, fill = "blue") +
  # Customize the color scale
  scale_color_manual(values = c("Camera Trap" = "blue", "Within 15m" = "forestgreen")) +
  # Add labels and theme
  labs(title = "Locations of Camera Traps for the mid elevation stand",
       #subtitle = "Trees within 15m of Camera Traps",
       x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal() +
  theme(legend.position = "NA")



#FOR AE10
# Create an empty list to store results
trees_within_radius_ae10 <- list()
# Loop over each camera trap and compute the distance to all trees
for (i in 1:nrow(camera_sf_ae10)) {
  # Get the coordinates of the current camera trap
  camera_coords <- st_coordinates(camera_sf_ae10[i, ])
  # Compute the distance to each tree
  tree_coords <- st_coordinates(stand_sf_ae10)
  distances <- sqrt((tree_coords[, 1] - camera_coords[1])^2 + (tree_coords[, 2] - camera_coords[2])^2)
  # Convert to your coordinate system scale (if needed, e.g., 100x100 coordinate system)
  threshold_distance_custom <- 15  # Adjust this if you want in your custom coordinate system
  # Create a buffer around each tree with the defined threshold distance
  camera_buffer <- st_buffer(camera_sf_ae10, dist = threshold_distance_custom)  # Buffer around trees
  # Now, find the trees that are within the buffer zone
  trees_within_buffer <- st_intersects(stand_sf_ae10, camera_buffer, sparse = FALSE)
  # Store the result in the list (camera trap id and the trees within the buffer)
  trees_within_radius_ae10[[i]] <- trees_within_buffer
}
# Combine the list of results into a single spatial data frame
trees_within_radius_ae10_df <- do.call(rbind, trees_within_radius_ae10)
str(trees_within_radius_ae10_df)


# Convert the logical matrix into an index of selected trees (TRUE values)
selected_tree_indices <- which(trees_within_radius_ae10_df == TRUE, arr.ind = TRUE)
# Extract the tree IDs or coordinates from the selected trees
selected_trees_ae10 <- stand_sf_ae10[selected_tree_indices[, 1], ]
# Visualize the result
ggplot() +
  # Plot all trees
  geom_sf(data = stand_sf_ae10, color = "gray", size = 1, alpha = 0.6) +
  # Plot the selected trees within the buffer zone in green
  #geom_sf(data = selected_trees_ae10, aes(color = "Within 15m"), size = 2) +
  # Plot camera traps in blue
  geom_sf(data = camera_sf_ae10, aes(color = "Camera Trap"), shape = 21, size = 3, fill = "blue") +
  # Customize the color scale
  scale_color_manual(values = c("Camera Trap" = "blue", "Within 15m" = "forestgreen")) +
  # Add labels and theme
  labs(title = "Locations of Camera Traps for the high elevation stand",
       #subtitle = "Trees within 15m of Camera Traps",
       x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal() +
  theme(legend.position = "NA")

# DBH ---------------------------------------------------------------------

#FOR To04
# Make sure DBH is numeric (convert from comma to dot if needed)
stand_sf_to04$dbh_num <- as.numeric(gsub(",", ".", stand_sf_to04$dbh))
# Initialize a data frame to store results
mean_dbh_per_camera_to04 <- data.frame(
  camera_id = character(),
  mean_dbh = numeric(),
  stringsAsFactors = FALSE
)

# Loop over each camera trap
for (i in 1:nrow(camera_sf_to04)) {
  # Get camera ID or tag (adjust as needed)
  camera_id <- camera_sf_to04$tag[i]  # or whatever uniquely identifies your camera
  # Get coordinates and compute distances
  camera_coords <- st_coordinates(camera_sf_to04[i, ])
  tree_coords <- st_coordinates(stand_sf_to04)
  distances <- sqrt((tree_coords[, 1] - camera_coords[1])^2 + (tree_coords[, 2] - camera_coords[2])^2)
  # Define threshold (already in your coordinate system)
  threshold_distance_custom <- 15
  # Get trees within radius
  nearby_trees <- stand_sf_to04[distances <= threshold_distance_custom, ]
  # Compute mean DBH (ignore NAs)
  mean_dbh <- mean(nearby_trees$dbh_num, na.rm = TRUE)
  # Save result
  mean_dbh_per_camera_to04 <- rbind(mean_dbh_per_camera_to04, data.frame(
    camera_id = camera_id,
    mean_dbh = mean_dbh
  ))
}
# View results
print(mean_dbh_per_camera_to04)


#FOR Av06
# Make sure DBH is numeric (convert from comma to dot if needed)
stand_sf_av06$dbh_num <- as.numeric(gsub(",", ".", stand_sf_av06$dbh))
# Initialize a data frame to store results
mean_dbh_per_camera_av06 <- data.frame(
  camera_id = character(),
  mean_dbh = numeric(),
  stringsAsFactors = FALSE
)

# Loop over each camera trap
for (i in 1:nrow(camera_sf_av06)) {
  # Get camera ID or tag (adjust as needed)
  camera_id <- camera_sf_av06$tag[i]  # or whatever uniquely identifies your camera
  # Get coordinates and compute distances
  camera_coords <- st_coordinates(camera_sf_av06[i, ])
  tree_coords <- st_coordinates(stand_sf_av06)
  distances <- sqrt((tree_coords[, 1] - camera_coords[1])^2 + (tree_coords[, 2] - camera_coords[2])^2)
  # Define threshold (already in your coordinate system)
  threshold_distance_custom <- 15
  # Get trees within radius
  nearby_trees <- stand_sf_av06[distances <= threshold_distance_custom, ]
  # Compute mean DBH (ignore NAs)
  mean_dbh <- mean(nearby_trees$dbh_num, na.rm = TRUE)
  # Save result
  mean_dbh_per_camera_av06 <- rbind(mean_dbh_per_camera_av06, data.frame(
    camera_id = camera_id,
    mean_dbh = mean_dbh
  ))
}
# View results
print(mean_dbh_per_camera_av06)


#FOR AE10
# Make sure DBH is numeric (convert from comma to dot if needed)
stand_sf_ae10$dbh_num <- as.numeric(gsub(",", ".", stand_sf_ae10$dbh))
# Initialize a data frame to store results
mean_dbh_per_camera_ae10 <- data.frame(
  camera_id = character(),
  mean_dbh = numeric(),
  stringsAsFactors = FALSE
)

# Loop over each camera trap
for (i in 1:nrow(camera_sf_ae10)) {
  # Get camera ID or tag (adjust as needed)
  camera_id <- camera_sf_ae10$tag[i]  # or whatever uniquely identifies your camera
  # Get coordinates and compute distances
  camera_coords <- st_coordinates(camera_sf_ae10[i, ])
  tree_coords <- st_coordinates(stand_sf_ae10)
  distances <- sqrt((tree_coords[, 1] - camera_coords[1])^2 + (tree_coords[, 2] - camera_coords[2])^2)
  # Define threshold (already in your coordinate system)
  threshold_distance_custom <- 15
  # Get trees within radius
  nearby_trees <- stand_sf_ae10[distances <= threshold_distance_custom, ]
  # Compute mean DBH (ignore NAs)
  mean_dbh <- mean(nearby_trees$dbh_num, na.rm = TRUE)
  # Save result
  mean_dbh_per_camera_ae10 <- rbind(mean_dbh_per_camera_ae10, data.frame(
    camera_id = camera_id,
    mean_dbh = mean_dbh
  ))
}
# View results
print(mean_dbh_per_camera_ae10)

#Adding it to the main data.
seed_predation_with_dbh <- seed_predation %>%
  left_join(mean_dbh_per_camera_ae10, by = c("TreeTag" = "camera_id"))

mean_dbh_all <- bind_rows(
  mean_dbh_per_camera_ae10,
  mean_dbh_per_camera_av06,
  mean_dbh_per_camera_to04
)

seed_predation_with_dbh <- seed_predation %>%
  left_join(mean_dbh_all, by = c("TreeTag" = "camera_id"))


save(seed_predation_with_dbh, file = "C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/seed_predation_with_dbh.RData")




# Additional characteristics ----------------------------------------------

#AE10 Stand characteristics
# Convert DBH to numeric (handle comma as decimal)
stand_data_ae10 <- stand_data_ae10 %>%
  mutate(dbh = as.numeric(gsub(",", ".", dbh)))
# Summarize species composition and mean DBH
# Summarize species composition with proportions
species_summary_ae10 <- stand_data_ae10 %>%
  group_by(species) %>%
  summarise(
    count = n(),
    mean_dbh = round(mean(dbh, na.rm = TRUE), 1)
  ) %>%
  mutate(
    proportion = round(100 * count / sum(count), 1)
  ) %>%
  arrange(desc(proportion))
print(species_summary_ae10)


#AV06 Stand characteristics
# Convert DBH to numeric (handle comma as decimal)
stand_data_av06 <- stand_data_av06 %>%
  mutate(dbh = as.numeric(gsub(",", ".", dbh)))
# Summarize species composition and mean DBH
# Summarize species composition with proportions
species_summary_av06 <- stand_data_av06 %>%
  group_by(species) %>%
  summarise(
    count = n(),
    mean_dbh = round(mean(dbh, na.rm = TRUE), 1)
  ) %>%
  mutate(
    proportion = round(100 * count / sum(count), 1)
  ) %>%
  arrange(desc(proportion))
print(species_summary_av06)


#To04 Stand characteristics
# Convert DBH to numeric (handle comma as decimal)
stand_data_to04 <- stand_data_to04 %>%
  mutate(dbh = as.numeric(gsub(",", ".", dbh)))
# Summarize species composition and mean DBH
# Summarize species composition with proportions
species_summary_to04 <- stand_data_to04 %>%
  group_by(species) %>%
  summarise(
    count = n(),
    mean_dbh = round(mean(dbh, na.rm = TRUE), 1)
  ) %>%
  mutate(
    proportion = round(100 * count / sum(count), 1)
  ) %>%
  arrange(desc(proportion))
print(species_summary_to04)
