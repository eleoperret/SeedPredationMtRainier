####Code for CAMERA TRAP EXPERIMENT US 2017
####Autumn 2023
####ETHZ Eléonore Perret
####dataset containing infos for camera trap experiment US
##In this code, I want to look at the canopy
#Results; all canopies are in the class U. So this is not really usefull (I think).

#Loading the library needed
library(dplyr)
library(ggplot2)

#Loading the data to be treated
load("C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/to04_data.RData")
load("C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/av06_data.RData")
load("C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/ae10_data.RData")

# CANOPY ---------------------------------------------------------
#TO04
# Group the data by Camera.number and canopy_class, and calculate the counts
canopy_class_counts_per_camera_to04 <- to04_data %>%
  filter(!is.na(canopy_class)) %>%
  group_by(Camera.number, canopy_class) %>%
  summarize(count = n())
canopy_class_counts_to04 <- to04_data %>%
  filter(!is.na(canopy_class)) %>%
  group_by(canopy_class) %>%
  summarize(count = n())
# Calculate the total count of canopy_class
total_count <- to04_data %>%
  filter(!is.na(canopy_class)) %>%
  summarize(total_count = n())
# Calculate the count and percentage for each canopy_class
percentage_canopy_class_counts_to04 <- to04_data %>%
  filter(!is.na(canopy_class)) %>%
  group_by(canopy_class) %>%
  summarize(count = n()) %>%
  mutate(percentage = (count / total_count$total_count) * 100)
# Find the main canopy class for each camera
main_canopy_class_per_camera_to04 <- canopy_class_counts_per_camera_to04 %>%
  group_by(Camera.number) %>%
  filter(count == max(count)) %>%
  select(Camera.number, canopy_class)

#AV06
canopy_class_counts_per_camera_av06 <- av06_data %>%
  filter(!is.na(canopy_class)) %>%
  group_by(Camera.number, canopy_class) %>%
  summarize(count = n())
canopy_class_counts_av06 <- av06_data %>%
  filter(!is.na(canopy_class)) %>%
  group_by(canopy_class) %>%
  summarize(count = n())
# Calculate the total count of canopy_class
total_count <- av06_data %>%
  filter(!is.na(canopy_class)) %>%
  summarize(total_count = n())
# Calculate the count and percentage for each canopy_class
percentage_canopy_class_counts_av06 <- av06_data %>%
  filter(!is.na(canopy_class)) %>%
  group_by(canopy_class) %>%
  summarize(count = n()) %>%
  mutate(percentage = (count / total_count$total_count) * 100)
# Find the main canopy class for each camera
main_canopy_class_per_camera_av06 <- canopy_class_counts_per_camera_av06 %>%
  group_by(Camera.number) %>%
  filter(count == max(count)) %>%
  select(Camera.number, canopy_class)

#AE10
canopy_class_counts_per_camera_ae10 <- ae10_data %>%
  filter(!is.na(canopy_class)) %>%
  group_by(Camera.number, canopy_class) %>%
  summarize(count = n())
canopy_class_counts_ae10 <- ae10_data %>%
  filter(!is.na(canopy_class)) %>%
  group_by(canopy_class) %>%
  summarize(count = n())
# Calculate the total count of canopy_class
total_count <- ae10_data %>%
  filter(!is.na(canopy_class)) %>%
  summarize(total_count = n())
# Calculate the count and percentage for each canopy_class
percentage_canopy_class_counts_ae10 <- ae10_data %>%
  filter(!is.na(canopy_class)) %>%
  group_by(canopy_class) %>%
  summarize(count = n()) %>%
  mutate(percentage = (count / total_count$total_count) * 100)
# Find the main canopy class for each camera
main_canopy_class_per_camera_ae10 <- canopy_class_counts_per_camera_ae10 %>%
  group_by(Camera.number) %>%
  filter(count == max(count)) %>%
  select(Camera.number, canopy_class)

# Merge the datasets based on Camera.number
merged_main_canopy_classes <- merge(main_canopy_class_per_camera_ae10, 
                                    main_canopy_class_per_camera_av06, 
                                    by = "Camera.number", 
                                    all = TRUE)

merged_main_canopy_classes <- merge(merged_main_canopy_classes, 
                                    main_canopy_class_per_camera_to04, 
                                    by = "Camera.number", 
                                    all = TRUE)

#
