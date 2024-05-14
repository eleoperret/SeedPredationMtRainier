####Code for CAMERA TRAP EXPERIMENT US 2017
####Autumn 2023
####ETHZ Eléonore Perret
####dataset containing all infos for camera trap experiment US
##In this code, I first clean and then merge the selected information
##For my camera traps, I want to know the trees in their neighbourhood. 


# Loading librairies ------------------------------------------------------
install.packages("sf")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)


list.files("C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/")




# Loading the data --------------------------------------------------------
##Load the data
#I need to load the dataset for the camera trap,seed predation data, the tree location and the tree characteristics.
#First I load the data from the CAMERA TRAP DATA
file_path <- "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/SeedPredation_First_week.csv"
seed_predation <- read.csv(file_path,sep = ";")
#Then I load the data from the overall sites LOCATION OF TREES IN PLOT DATA
file_path <- "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/Cleaned_mapping_2017 Rainier.csv"
map_tree_rainier <- read.csv(file_path, sep = ";")
#First I load the data from the tree growth INFORMATION ON TREES DATA
file_path <- "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/Cleaned_tree_growth_2017 Rainier.csv"
tree_gowth_data<- read.csv(file_path,sep = ";")
#Data from SEED PREDATION RESULTS
data_name <- 'seed_data.csv'
path <- "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/"
all_data_seed <- read.csv(paste(path, '/Datasets/', data_name, sep = ''), header = TRUE, sep = ";")

# CAMERA TRAP DATA : Process_data ------------------------------------------------------------
#Because I have empty rows after the row 27 (this comes from excel), I will first delete all the rows below
seed_predation <- seed_predation %>% slice(1:27)
# Selecting only the sites I need
stand_data <- map_tree_rainier %>%
  filter(stand_id %in% c("AE10", "AV06", "TO04"))

#Because I'm not sure that the coordinate system used in cleaned_mapping is an actual coordinate system (it looks more just as an X Y in the plot), I will select the tree tag used for the camera trap to do my analysis.
#I need first to make sure that I have the correct dataset with only the trees with the camera traps.
# First, I select the tags from seed_predation
tags_tree <- seed_predation$TreeTag
#Create dataset: camera number matches the tree tag from the dataset LOCATION OF TREES IN PLOTS DATA
camera_trap_tree <- stand_data %>%
  filter(tag %in% tags_tree)


#Creating subsets. 
stand_data_to04 <- stand_data[stand_data$stand_id == "TO04", ]
stand_data_av06 <- stand_data[stand_data$stand_id == "AV06", ]
stand_data_ae10 <- stand_data[stand_data$stand_id == "AE10", ]
camera_trap_tree_to04<-camera_trap_tree[camera_trap_tree$stand_id == "TO04", ]
camera_trap_tree_av06<-camera_trap_tree[camera_trap_tree$stand_id == "AV06", ]
camera_trap_tree_ae10<-camera_trap_tree[camera_trap_tree$stand_id == "AE10", ]


# LOCATION OF TREES IN PLOTS DATA:  Finding the trees close to the traps -----------------------------------
#To04
#Change dataset into geospatial objects and then select the ones around 5 m of my camera traps
camera_trap_tree_to04$x_coord <- as.numeric(gsub(",", ".", camera_trap_tree_to04$x_coord))
camera_trap_tree_to04$y_coord <- as.numeric(gsub(",", ".", camera_trap_tree_to04$y_coord))
stand_data_to04$x_coord <- as.numeric(gsub(",", ".", stand_data_to04$x_coord))
stand_data_to04$y_coord <- as.numeric(gsub(",", ".", stand_data_to04$y_coord))
camera_trap_tree_sf_to04 <- st_as_sf(camera_trap_tree_to04, coords = c("x_coord", "y_coord"))
stand_data_sf_to04 <- st_as_sf(stand_data_to04, coords = c("x_coord", "y_coord"))
distances <- st_distance(camera_trap_tree_sf_to04, stand_data_sf_to04)
trees_within_5m_to04 <- distances <= 5
trees_within_5m_camera_to04_lol<- st_join(camera_trap_tree_sf_to04, stand_data_sf_to04, join = st_is_within_distance, dist = 5)
#av06
camera_trap_tree_av06$x_coord <- as.numeric(gsub(",", ".", camera_trap_tree_av06$x_coord))
camera_trap_tree_av06$y_coord <- as.numeric(gsub(",", ".", camera_trap_tree_av06$y_coord))
stand_data_av06$x_coord <- as.numeric(gsub(",", ".", stand_data_av06$x_coord))
stand_data_av06$y_coord <- as.numeric(gsub(",", ".", stand_data_av06$y_coord))
camera_trap_tree_sf_av06 <- st_as_sf(camera_trap_tree_av06, coords = c("x_coord", "y_coord"))
stand_data_sf_av06 <- st_as_sf(stand_data_av06, coords = c("x_coord", "y_coord"))
distances <- st_distance(camera_trap_tree_sf_av06, stand_data_sf_av06)
trees_within_5m_av06 <- distances <= 5
trees_within_5m_camera_av06_lol<- st_join(camera_trap_tree_sf_av06, stand_data_sf_av06, join = st_is_within_distance, dist = 5)
#ae10
camera_trap_tree_ae10$x_coord <- as.numeric(gsub(",", ".", camera_trap_tree_ae10$x_coord))
camera_trap_tree_ae10$y_coord <- as.numeric(gsub(",", ".", camera_trap_tree_ae10$y_coord))
stand_data_ae10$x_coord <- as.numeric(gsub(",", ".", stand_data_ae10$x_coord))
stand_data_ae10$y_coord <- as.numeric(gsub(",", ".", stand_data_ae10$y_coord))
camera_trap_tree_sf_ae10 <- st_as_sf(camera_trap_tree_ae10, coords = c("x_coord", "y_coord"))
stand_data_sf_ae10 <- st_as_sf(stand_data_ae10, coords = c("x_coord", "y_coord"))
distances <- st_distance(camera_trap_tree_sf_ae10, stand_data_sf_ae10)
trees_within_5m_ae10 <- distances <= 5
trees_within_5m_camera_ae10_lol<- st_join(camera_trap_tree_sf_ae10, stand_data_sf_ae10, join = st_is_within_distance, dist = 5)
#Create a dataset with all the information together
stacked_df_lol <- rbind(trees_within_5m_camera_ae10_lol,trees_within_5m_camera_av06_lol,trees_within_5m_camera_to04_lol)
# Merge all together. 
tree_trap_5m_lol <- merge(stacked_df_lol, seed_predation, by.x = "tag.x", by.y = "TreeTag", all.x = TRUE)

#Saving the results: 
#This is the data sets that contains all the trees around my camera trap (5m), for the tree tag info
write.csv(tree_trap_5m_lol, file = "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/tree_trap_5m.csv", row.names = FALSE)
save(tree_trap_5m_lol, file = "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/tree_trap_5m.RData")


# TREE PROPERTIES --------------------------------------------------------
#Working now with another dataset
#Now I have a dataset that contains multiple years, so I want to select only the data from the last year which is 2017. 
tree_gowth_data_2017<- tree_gowth_data %>%
  filter(year %in% c(2017))
save(tree_gowth_data_2017, file = "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/tree_gowth_data_2017.RData")
tree_gowth_data_2017_sites<- tree_gowth_data_2017 %>%
  filter(stand_id %in% c("TO04","AV06","AE10"))
save(tree_gowth_data_2017, file = "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/tree_gowth_data_2017.RData")

#Now I want to have a dataset with everything. So I will merge it based on the tag of tree. 
# Merge by 'tag' for tree_growth_data_2017 and 'tag.y' for tree_trap_5m
all_tree_data <- inner_join(tree_gowth_data_2017_sites, tree_trap_5m_lol, by = c("tag" = "tag.x"))
## Save cleaned data
save(all_tree_data, file = "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/all_tree_data.RData")
#Now I have the dataset with all the tree characteristics for the trees around my camera (5m)

# SEED PREDATION RESULTS ---------------------------------------------------
#Finally, I want to add the results from the seed predation experiment
# Define the number of seeds disposed for each species
seeds_disposed_ABAM <- 5
seeds_disposed_ABLA <- 20
seeds_disposed_CANO <- 30
seeds_disposed_PSME <- 25
seeds_disposed_TSHE <- 90
seeds_disposed_THPL <- 90
# Data Cleaning 
# Replace 'to count' with NA in the Seeds.Remaining column
all_data_seed$Seeds.Remaining[all_data_seed$Seeds.Remaining == 'to count'] <- NA
# Correct misspelled species names
all_data_seed$Seed_sp[all_data_seed$Seed_sp == 'THPLE'] <- 'THPL'
# Remove rows with NA values
data_cleaned_2 <- na.omit(all_data_seed)
# Data treatment
x<-length(data_cleaned_2[,1])
data_cleaned_2$seeds_eaten<-c(1:x)
for(i in 1:x){
  if(data_cleaned_2$Seed_sp[i]=="CANO"){
    data_cleaned_2$seeds_eaten[i]<-seeds_disposed_CANO-as.numeric(data_cleaned_2$Seeds.Remaining[i])
  } else if (data_cleaned_2$Seed_sp[i]=="ABAM"){
    data_cleaned_2$seeds_eaten[i]<-seeds_disposed_ABAM-as.numeric(data_cleaned_2$Seeds.Remaining[i])
  } else if (data_cleaned_2$Seed_sp[i]=="ABLA"){
    data_cleaned_2$seeds_eaten[i]<-seeds_disposed_ABLA-as.numeric(data_cleaned_2$Seeds.Remaining[i])
  }else if (data_cleaned_2$Seed_sp[i]=="PSME"){
    data_cleaned_2$seeds_eaten[i]<-seeds_disposed_PSME-as.numeric(data_cleaned_2$Seeds.Remaining[i])
  }else if (data_cleaned_2$Seed_sp[i]=="THPL"){
    data_cleaned_2$seeds_eaten[i]<-seeds_disposed_THPL-as.numeric(data_cleaned_2$Seeds.Remaining[i])
  }else if (data_cleaned_2$Seed_sp[i]=="TSHE"){
    data_cleaned_2$seeds_eaten[i]<-seeds_disposed_TSHE-as.numeric(data_cleaned_2$Seeds.Remaining[i])
  }
}
data_cleaned_2$relative_seed_eaten<-c(1:x)
for(i in 1:x){
  if(data_cleaned_2$Seed_sp[i]=="CANO"){
    data_cleaned_2$relative_seed_eaten[i]<-as.numeric((data_cleaned_2$seeds_eaten[i]*100)/seeds_disposed_CANO)
  } else if (data_cleaned_2$Seed_sp[i]=="ABAM"){
    data_cleaned_2$relative_seed_eaten[i]<-as.numeric((data_cleaned_2$seeds_eaten[i]*100)/seeds_disposed_ABAM)
  } else if (data_cleaned_2$Seed_sp[i]=="ABLA"){
    data_cleaned_2$relative_seed_eaten[i]<-as.numeric((data_cleaned_2$seeds_eaten[i]*100)/seeds_disposed_ABLA)
  }else if (data_cleaned_2$Seed_sp[i]=="PSME"){
    data_cleaned_2$relative_seed_eaten[i]<-as.numeric((data_cleaned_2$seeds_eaten[i]*100)/seeds_disposed_PSME)
  }else if (data_cleaned_2$Seed_sp[i]=="THPL"){
    data_cleaned_2$relative_seed_eaten[i]<-as.numeric((data_cleaned_2$seeds_eaten[i]*100)/seeds_disposed_THPL)
  }else if (data_cleaned_2$Seed_sp[i]=="TSHE"){
    data_cleaned_2$relative_seed_eaten[i]<-as.numeric((data_cleaned_2$seeds_eaten[i]*100)/seeds_disposed_TSHE)
  }
}
colnames(data_cleaned_2)[colnames(data_cleaned_2) == "relative_seed_eaten"] <- "removal_per_all"
data_cleaned_2$seeds<-c(1:x)
for(i in 1:x){
  if(data_cleaned_2$Seed_sp[i]=="CANO"){
    data_cleaned_2$seeds[i]<-as.numeric((data_cleaned_2$seeds_eaten[i])/seeds_disposed_CANO)
  } else if (data_cleaned_2$Seed_sp[i]=="ABAM"){
    data_cleaned_2$seeds[i]<-as.numeric((data_cleaned_2$seeds_eaten[i])/seeds_disposed_ABAM)
  } else if (data_cleaned_2$Seed_sp[i]=="ABLA"){
    data_cleaned_2$seeds[i]<-as.numeric((data_cleaned_2$seeds_eaten[i])/seeds_disposed_ABLA)
  }else if (data_cleaned_2$Seed_sp[i]=="PSME"){
    data_cleaned_2$seeds[i]<-as.numeric((data_cleaned_2$seeds_eaten[i])/seeds_disposed_PSME)
  }else if (data_cleaned_2$Seed_sp[i]=="THPL"){
    data_cleaned_2$seeds[i]<-as.numeric((data_cleaned_2$seeds_eaten[i])/seeds_disposed_THPL)
  }else if (data_cleaned_2$Seed_sp[i]=="TSHE"){
    data_cleaned_2$seeds[i]<-as.numeric((data_cleaned_2$seeds_eaten[i])/seeds_disposed_TSHE)
  }
}

#Based on the number of seeds disposed at the beggining of the week. This the number of seeds eaten or the predation per seeds specie. 
data_cleaned_2$seeds_disposed<-c(1:x)
for(i in 1:x){
  if(data_cleaned_2$Seed_sp[i]=="CANO"){
    data_cleaned_2$seeds_disposed[i]<-as.numeric(seeds_disposed_CANO)
  } else if (data_cleaned_2$Seed_sp[i]=="ABAM"){
    data_cleaned_2$seeds_disposed[i]<-as.numeric(seeds_disposed_ABAM)
  } else if (data_cleaned_2$Seed_sp[i]=="ABLA"){
    data_cleaned_2$seeds_disposed[i]<-as.numeric(seeds_disposed_ABLA)
  }else if (data_cleaned_2$Seed_sp[i]=="PSME"){
    data_cleaned_2$seeds_disposed[i]<-as.numeric(seeds_disposed_PSME)
  }else if (data_cleaned_2$Seed_sp[i]=="THPL"){
    data_cleaned_2$seeds_disposed[i]<-as.numeric(seeds_disposed_THPL)
  }else if (data_cleaned_2$Seed_sp[i]=="TSHE"){
    data_cleaned_2$seeds_disposed[i]<-as.numeric(seeds_disposed_TSHE)
  }
}


# Save the cleaned data as an RData file
save(data_cleaned_2, file = "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/data_cleaned_2.RData")

