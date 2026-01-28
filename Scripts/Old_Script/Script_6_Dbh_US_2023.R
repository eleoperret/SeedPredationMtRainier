####Code for CAMERA TRAP EXPERIMENT US 2017
####Autumn 2023
####ETHZ Eléonore Perret
####dataset containing infos for camera trap experiment US
##In this code, I want to look at the dbh and create plots (the mean dbh per camera is created in Script_4_Conspecific_US_2023)
#I first look at the dbh per site, the dbh per species, the dbh per site for 5m, the dbh per camera 5m and finally see how they differ from each other. Is it a good representation of the dbh of the site ?

#Loading the library needed
library(dplyr)
library(ggplot2)

setwd("C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets")
#Loading the data to be treated
load("data_site_info.RData")
load("data_cleaned_2.RData")
load("all_tree_data.RData")
load("tree_trap_5m.RData")


head(data_site_info)

# Calculate total and count-based averages for each species
avg_dbh_species <- data_site_info %>%
  group_by(species.y) %>%
  summarise(
    total_dbh = sum(as.numeric(gsub(",", ".", dbh)), na.rm = TRUE), # Total DBH
    count = n(), # Count of observations
    avg_dbh = total_dbh / count # Average DBH
  )

# Calculate total and count-based averages for each species at each site
avg_dbh_species_site <- data_site_info %>%
  group_by(stand_id, species.y) %>%
  summarise(
    total_dbh = sum(as.numeric(gsub(",", ".", dbh)), na.rm = TRUE), # Total DBH
    count = n(), # Count of observations
    avg_dbh = total_dbh / count # Average DBH
  )


# Plot average dbh for each species
ggplot(avg_dbh_species, aes(x = species.y, y = avg_dbh, fill = species.y)) +
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
  )

# Plot average dbh for each species at each site
ggplot(avg_dbh_species_site, aes(x = stand_id, y = avg_dbh, fill = species.y)) +
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
  )








data_site_info_dbh <- data_site_info %>%
  group_by(Camera.number) %>%
  summarize(dbh = paste(unique(dbh), collapse = ", ")) 
colnames(data_site_info_dbh)[colnames(data_site_info_dbh) == "Camera.number"] <- "Camera"
conspecific_with_dbh <- merge(conspecific, data_site_info_dbh, by = "Camera")
save(conspecific_with_dbh, file = "C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/conspecific_with_dbh.RData")

# DBH per site ---------------------------------------------------------------------

#Now for the dbh,I will first extract the data from the tree growth data to look at dbh in general for the sites. 
# DBH FOR ALL TREES:AV06
av06 <- av06_data_tree %>%
  mutate(dbh = as.numeric(gsub(",", ".", dbh)))
av06_dbh<-mean(av06$dbh, na.rm=TRUE)
av06_dbh_species<-av06 %>%
  group_by(species) %>%
  summarize(mean_dbh = mean (dbh, nar.rm= TRUE))
ggplot(av06_dbh_species, aes(x = species, y = mean_dbh)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "species", y = "Mean DBH") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  
#DBH FOR ALL TREES:AE10
ae10 <- ae10_data_tree%>%
  mutate(dbh = as.numeric(gsub(",", ".", dbh)))
ae10_dbh<-mean(ae10$dbh, na.rm=TRUE)
ae10_dbh_species<-ae10 %>%
  group_by(species) %>%
  summarize(mean_dbh = mean (dbh, nar.rm= TRUE))
ggplot(ae10_dbh_species, aes(x = species, y = mean_dbh)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Species", y = "Mean DBH") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
#DBH FOR ALL TREES:TO04
to04 <- to04_data_tree %>%
  mutate(dbh = as.numeric(gsub(",", ".", dbh)))
to04_dbh<-mean(to04$dbh, na.rm=TRUE)
to04_dbh_species<-to04 %>%
  group_by(species) %>%
  summarize(mean_dbh = mean (dbh, nar.rm= TRUE))
dbh_species<-ggplot(to04_dbh_species, aes(x = species, y = mean_dbh)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Species", y = "Mean DBH") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.copy(png, "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Plots/dbh_species.png")
dev.off()

#DBH FOR ALL TREES:Per site
mean_dbh_per_site <- data.frame(
  Site = c("AE10", "AV06", "TO04"),
  Mean_DBH = c(mean(ae10_dbh, na.rm = TRUE), mean(av06_dbh, na.rm = TRUE), mean(to04_dbh, na.rm = TRUE))
)
dbh_sites<-ggplot(mean_dbh_per_site, aes(x = Site, y = Mean_DBH)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Site", y = "Mean DBH") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.copy(png, "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Plots/dbh_sites.png")
dev.off()

#In general we can see that av06 has a smaller dbh than the other sites. 


# DBH_ per species --------------------------------------------------------

#DBH PER SPECIES PER SITES
to04_dbh_species <- to04_dbh_species %>% mutate(site = "TO04")
ae10_dbh_species <- ae10_dbh_species %>% mutate(site = "AE10")
av06_dbh_species <- av06_dbh_species %>% mutate(site = "AV06")
merged_dbh_species <- bind_rows(
  to04_dbh_species,
  ae10_dbh_species,
  av06_dbh_species
)
dbh_site_species<-ggplot(merged_dbh_species, aes(x = site, y = mean_dbh, fill = species)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Site", y = "Mean DBH") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.copy(png, "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Plots/dbh_site_species.png")
dev.off()
dbh_species_sites<-ggplot(merged_dbh_species, aes(x = species, y = mean_dbh, fill = site)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Species", y = "Mean DBH") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("TO04" = "darkblue", "AE10" = "lightblue", "AV06" = "blue"))
dev.copy(png, "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Plots/dbh_species_sites.png")
dev.off()

# DBH_ for trees around camera (5m) ---------------------------------------

#DBH FOR TREES IN 5 M CAMERA TRAP
#TO04
# Convert 'dbh' column to numeric 
to04_data$dbh.y <- as.numeric(gsub(",", ".", to04_data$dbh.y))
# Calculate the mean with NA removal
to04_5m_meandbh <- mean(to04_data$dbh.y, na.rm = TRUE)
#AV06
av06_data$dbh.y <- as.numeric(gsub(",", ".", av06_data$dbh.y))
av06_5m_meandbh <- mean(av06_data$dbh.y, na.rm = TRUE)
#AE10
ae10_data$dbh.y <- as.numeric(gsub(",", ".", ae10_data$dbh.y))
ae10_5m_meandbh <- mean(ae10_data$dbh.y, na.rm = TRUE)
#Comparing the mean per sites.
mean_dbh_5m_all_sites<- c(ae10_5m_meandbh,av06_5m_meandbh,to04_5m_meandbh)
# Create a scatter plot of mean_dbh values for different sites
dbh_site_5m<- plot(x = 1:3, y = mean_dbh_5m_all_sites, 
     xlab = "Site", ylab = "Mean dbh",
     main = "Mean dbh for Different Sites for trees in 5m",
     xaxt = "n", ylim = c(0, max(mean_dbh_5m_all_sites) + 5))
axis(1, at = 1:3, labels = c("AE10", "AV06", "TO04"))
dev.copy(png, "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Plots/dbh_site_5m.png")
dev.off()

#Here we can see the mean dbh for the trees around my camera. 

# DBH_ per camera (calculations fro good representation?) ---------------------------------------------------------

#DBH PER CAMERA
#This can be an indicator of how the tree where the camera was on, is representing the mean dbh?
#TO04 (5m)
mean_dbh_per_camera_to04 <- to04_data %>%
  group_by(Camera.number) %>%
  summarize(mean_dbh = mean(dbh.y, na.rm = TRUE))
# Calculate how far each 'Camera.number' is from the mean (5m)
variation_dbh_per_camera_to04 <- mean_dbh_per_camera_to04 %>%
  mutate(deviation_from_mean_to04 = to04_5m_meandbh - mean_dbh)
#variation_all_site
variation_dbh_per_camera_to04_all <- mean_dbh_per_camera_to04 %>%
  mutate(deviation_from_mean_to04 = to04_dbh - mean_dbh)
# Create a bar plot of deviation_from_mean (5m)
ggplot(variation_dbh_per_camera_to04, aes(x = factor(Camera.number), y = deviation_from_mean_to04)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "DBH variation from mean TO04 (5m)",
       x = "Camera Number",
       y = "Deviation from Mean 'dbh'") +
  theme_minimal()

#AV06
mean_dbh_per_camera_av06 <- av06_data %>%
  group_by(Camera.number) %>%
  summarize(mean_dbh = mean(dbh.y, na.rm = TRUE))
# Calculate how far each 'Camera.number' is from the mean (5m)
variation_dbh_per_camera_av06 <- mean_dbh_per_camera_av06 %>%
  mutate(deviation_from_mean_av06 = av06_5m_meandbh - mean_dbh)
#variation_all_site
variation_dbh_per_camera_av06_all <- mean_dbh_per_camera_av06 %>%
  mutate(deviation_from_mean_av06 = av06_dbh - mean_dbh)
# Create a bar plot of deviation_from_mean (5m)
ggplot(variation_dbh_per_camera_av06, aes(x = factor(Camera.number), y = deviation_from_mean_av06)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "DBH variation from mean av06 (5m)",
       x = "Camera Number",
       y = "Deviation from Mean 'dbh'") +
  theme_minimal()
#There is less variation for Av06

#AE10
mean_dbh_per_camera_ae10 <- ae10_data %>%
  group_by(Camera.number) %>%
  summarize(mean_dbh = mean(dbh.y, na.rm = TRUE))
# Calculate how far each 'Camera.number' is from the mean (5m)
variation_dbh_per_camera_ae10 <- mean_dbh_per_camera_ae10 %>%
  mutate(deviation_from_mean_ae10 = ae10_5m_meandbh - mean_dbh)
#variation_all_site
variation_dbh_per_camera_ae10_all <- mean_dbh_per_camera_ae10 %>%
  mutate(deviation_from_mean_ae10 = ae10_dbh - mean_dbh)
# Create a bar plot of deviation_from_mean (5m)
ggplot(variation_dbh_per_camera_ae10, aes(x = factor(Camera.number), y = deviation_from_mean_ae10)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "DBH variation from mean ae10 (5m) 'dbh'",
       x = "Camera Number",
       y = "Deviation from Mean 'dbh'") +
  theme_minimal()
# Create a bar plot of deviation_from_mean all site
ggplot(variation_dbh_per_camera_ae10_all, aes(x = factor(Camera.number), y = deviation_from_mean_ae10)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "DBH variation from mean ae10 'dbh'",
       x = "Camera Number",
       y = "Deviation from Mean 'dbh'") +
  theme_minimal()


#How is my mean bh related to the removal rate?
mean_dbh_removal<-ggplot(data_merged_6, aes(x = mean_dbh, y = removal_per_all)) +
  geom_point() +                    # Scatterplot
  geom_smooth(method = "lm") +      # Add a linear trendline
  labs(x = "Mean DBH", y = "Removal per All") +
  ggtitle("General Relationship between Mean DBH and Removal per All")
ggsave(filename = "mean_dbh_removal.png", plot =mean_dbh_removal,width = 5, height = 5, units = "in", dpi = 300, path = "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Plots/")

#How is the mean dbh related to the removal rate looking at sites:
mean_dbh_removal_site<-ggplot(data_merged_6, aes(x = mean_dbh, y = removal_per_all)) +
  geom_point() +                    # Scatterplot
  geom_smooth(method = "lm") +      # Add a linear trendline
  labs(x = "Mean DBH", y = "Removal per All") +
  facet_wrap(~ Stand) +              # Create separate plots for each site
  ggtitle("Relationship between Mean DBH and Removal per All by Site")
ggsave(filename = "mean_dbh_removal_site.png", plot =mean_dbh_removal_site,width = 5, height = 5, units = "in", dpi = 300, path = "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Plots/")

#How is the mean dbh related to the removal rate looking at seed_sp
mean_dbh_removal_speciesall<-ggplot(data_merged_6, aes(x = mean_dbh, y = removal_per_all)) +
  geom_point() +                    # Scatterplot
  geom_smooth(method = "lm") +      # Add a linear trendline
  labs(x = "Mean DBH", y = "Removal per All") +
  facet_wrap(~ Seed_sp, scales = "free") +  # Create separate plots for each seed_sp
  ggtitle("Relationship between Mean DBH and Removal per All by Seed Species")
ggsave(filename = "mean_dbh_removal_speciesall.png", plot =mean_dbh_removal_speciesall,width = 5, height = 5, units = "in", dpi = 300, path = "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Plots/")

# Create a summary data frame with mean values per species
summary_data <- data_merged_6 %>%
  group_by(Seed_sp) %>%
  summarize(mean_mean_dbh = mean(mean_dbh), mean_removal_per_all = mean(removal_per_all))
# Create a scatterplot with one point per species
mean_dbh_removal_species<-ggplot(summary_data, aes(x = mean_mean_dbh, y = mean_removal_per_all, color = Seed_sp)) +
  geom_point(size = 3, alpha = 0.8) +  # Adjust the size and transparency (alpha)
  labs(x = "Mean DBH", y = "Mean Removal per All") +
  ggtitle("Relationship between Mean DBH and Mean Removal per All by Species") +
  scale_color_manual(values = rainbow(length(unique(summary_data$Seed_sp))))  # Customize color contrast
ggsave(filename = "mean_dbh_removal_species.png", plot =mean_dbh_removal_species,width = 5, height = 5, units = "in", dpi = 300, path = "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Plots/")





