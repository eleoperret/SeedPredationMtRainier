####Code for CAMERA TRAP EXPERIMENT US 2017
####Autumn 2023
####ETHZ Eléonore Perret
####dataset containing infos for camera trap experiment US
##In this code, I want to look at the conspecific (with camera trap results)
#This code ends with a dataset call data_merged_6 which contains information on the tree density,species richness and presence or absence of conspecific and also the mean dbh per camera


#Loading the library needed
library(dplyr)
library(ggplot2)

#Loading the data to be treated
load("C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/data_merged_4.RData")
load("C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/data_cleaned_2.RData")
load("C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/all_tree_data.RData")


# Cleaning the dataset ----------------------------------------------------
#Cleaning the data set: 
#Creating one unique value for the dbh.y for the species around the camera (to have a mean per camera)
# Convert 'dbh.y' to numeric (remove commas and convert to numeric)
data_merged_4$dbh.y <- as.numeric(gsub(",", ".", as.character(data_merged_4$dbh.y)))

# Create a new column with the mean value of 'dbh.y' for each unique tag
data_with_mean_dbh <- data_merged_4 %>%
  group_by(tag) %>%
  mutate(mean_dbh_y = mean(dbh.y, na.rm = TRUE))

#As I have more than once species per tag (due to the replication), I want to take out the duplicates as they are not necessary
# Keep only one row for each unique tag
unique_data <- data_with_mean_dbh %>%
  distinct(tag, .keep_all = TRUE)
# Print or use the 'unique_data' data frame

#Removing the unecessary column
data_filtered <- unique_data[, !colnames(unique_data) %in% c("tag.y","species.y","year.y","dbh.y")]

# PARENT_TREE_AROUND (CAmera) ------------------------------------------------------
#AMOUNT OF seed SPECIES THAT ARE THE SAME THAT THE TREE THE CAMERA IS ATTACHED TO (PER SITE)

#For this, first, I will merge the dataset with the camera results dataset.
data_merged_5 <- merge(data_filtered, data_cleaned_2, by.x = "Camera.number", by.y = "Camera", all.x = TRUE)

#Presence or absence of conspecific
data_merged_6 <- data_merged_5 %>%
  mutate(Matches_Species = apply(data_merged_5, 1, function(row) {
    ifelse(grepl(row["Seed_sp"], row["SpeciesList"], ignore.case = TRUE), "Yes", "No")
  }))

#I don't know how to trust those results. As some seed sp. where never next to an adult tree of their species. 

save(data_merged_6, file = "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/data_merged_6.RData")

sum_yes<-sum(data_merged_6$Matches_Species == "Yes")
sum_no<-sum(data_merged_6$Matches_Species=="No")
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
# Save the plot to the specified file path
dev.copy(png, "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Plots/conspecfic_removal.png")
dev.off()

#I think the result is normal as I don't have a good representation (seed sp/adult tree)


# PARENT TREE AROUND (BY SPECIES-5M) --------------------------------------


#BY SPECIES

##NEW CODE 
unique(all_tree_data$species.y)
#As there is only abam, cano, psme and tshe that are in the 5m around my trap. So for those species I can test if when there is the presence of them, the amount of removal rate. 

#For ABAM
# Group the data by "Matches_Species"
abam_data <- data_merged_6 %>% filter(Seed_sp == "ABAM")
abam_removal_conspecific<-abam_data %>%
  group_by(Matches_Species) %>%
  summarize(Average_Removal_Rate = mean(removal_per_all))
conspecfic_removal_abam<-barplot(abam_removal_conspecific$Average_Removal_Rate, names.arg = abam_removal_conspecific$Matches_Species, 
                                 col = c("lightgreen", "red"),
                                 main = "Average Removal Rate by Matches_Species_ABAM",
                                 xlab = "Matches_Species",
                                 ylab = "Average Removal Rate",
                                 ylim = c(0, 100),
                                 mar = c(5, 5, 4, 2))
dev.copy(png, "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Plots/conspecfic_removal_abam.png")
dev.off()
#There is not a lot of difference for removal rate when ABAM is present or not close to the camera trap.
#Lets look at the site also.
abam_removal_conspecific_site<-abam_data %>%
  group_by(Matches_Species,Stand) %>%
  summarize(Average_Removal_Rate = mean(removal_per_all))
#It seems that for the sites AV06 and AE10, ABAM is always present in the 5m
#For TO04, there is no more removal rate when there is the presence of ABAM in the 5m.
conspecfic_removal_abam_site<-ggplot(abam_removal_conspecific_site, aes(x = Stand, y = Average_Removal_Rate, fill = Matches_Species)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Removal Rate ABAM",
    x = "Stand",
    y = "Average Removal Rate (%)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Yes" = "lightgreen", "No" = "red")) +
  ylim(0, 100)
print(conspecfic_removal_abam_site)
ggsave(filename = "conspecfic_removal_abam_site.png", plot = conspecfic_removal_abam_site, width = 5, height = 5, units = "in", dpi = 300,path = "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Plots/")

#For CANO
cano_data <- data_merged_6 %>% filter(Seed_sp == "CANO")
cano_removal_conspecific <- cano_data %>%
  group_by(Matches_Species) %>%
  summarize(Average_Removal_Rate = mean(removal_per_all))
conspecfic_removal_cano <- barplot(cano_removal_conspecific$Average_Removal_Rate, names.arg = cano_removal_conspecific$Matches_Species,
                                   col = c("lightgreen", "red"),
                                   main = "Average Removal Rate by Matches_Species_CANO",
                                   xlab = "Matches_Species",
                                   ylab = "Average Removal Rate",
                                   ylim = c(0, 100))
dev.copy(png, "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Plots/conspecfic_removal_cano.png")
dev.off()
#It seems that for CANO, there is a higher rate when there is conspecific around. 
#Lets look at the site also.
cano_removal_conspecific_site<-cano_data %>%
  group_by(Matches_Species,Stand) %>%
  summarize(Average_Removal_Rate = mean(removal_per_all))
#It seems that as expected there is no conspecific in the lower altitude sites. 
#Interestingly, when there was no conspecific it seems that the average removal rate is higher. 

conspecfic_removal_cano_site <- ggplot(cano_removal_conspecific_site, aes(x = Stand, y = Average_Removal_Rate, fill = Matches_Species)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Removal Rate CANO",
    x = "Stand",
    y = "Average Removal Rate (%)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Yes" = "lightgreen", "No" = "red")) +
  ylim(0, 100)

print(conspecfic_removal_cano_site)
ggsave(filename = "conspecfic_removal_cano_site.png", plot = conspecfic_removal_cano_site, width = 5, height = 5, units = "in", dpi = 300,path = "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Plots/")


#For PSME
psme_data <- data_merged_6 %>% filter(Seed_sp == "PSME")
psme_removal_conspecific <- psme_data %>%
  group_by(Matches_Species) %>%
  summarize(Average_Removal_Rate = mean(removal_per_all))
conspecfic_removal_psme <- barplot(psme_removal_conspecific$Average_Removal_Rate, names.arg = psme_removal_conspecific$Matches_Species,
                                   col = c("lightgreen", "red"),
                                   main = "Average Removal Rate by Matches_Species_PSME",
                                   xlab = "Matches_Species",
                                   ylab = "Average Removal Rate",
                                   ylim = c(0, 100))
dev.copy(png, "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Plots/conspecfic_removal_psme.png")
dev.off()
#It seems that for psme, there removal rate is higher when there is no conspecific, but I suspect that this is due to the high predation of psme at the high altitude site AE10.
#Lets look at the site also.
psme_removal_conspecific_site<-psme_data %>%
  group_by(Matches_Species,Stand) %>%
  summarize(Average_Removal_Rate = mean(removal_per_all))
#For the AE10, even if there is no conspecfic the removal rate is of 100%.
#For av06, the removal rate is higher when no conspecific are present.
#for too04, the removal rate is higher when conspecific are present. 
conspecfic_removal_psme_site <- ggplot(psme_removal_conspecific_site, aes(x = Stand, y = Average_Removal_Rate, fill = Matches_Species)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Removal Rate PSME",
    x = "Stand",
    y = "Average Removal Rate (%)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Yes" = "lightgreen", "No" = "red")) +
  ylim(0, 100)

print(conspecfic_removal_psme_site)
ggsave(filename = "conspecfic_removal_psme_site.png", plot = conspecfic_removal_psme_site, width = 5, height = 5, units = "in", dpi = 300,path = "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Plots/")

#Finally, TSHE
tshe_data <- data_merged_6 %>% filter(Seed_sp == "TSHE")
tshe_removal_conspecific<-tshe_data %>%
  group_by(Matches_Species) %>%
  summarize(Average_Removal_Rate = mean(removal_per_all))
conspecfic_removal_tshe <- barplot(tshe_removal_conspecific$Average_Removal_Rate, names.arg = tshe_removal_conspecific$Matches_Species,
                                   col = c("lightgreen", "red"),
                                   main = "Average Removal Rate by Matches_Species_TSHE",
                                   xlab = "Matches_Species",
                                   ylab = "Average Removal Rate",
                                   ylim = c(0, 100))
dev.copy(png, "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Plots/conspecfic_removal_tshe.png")
dev.off()
#Same as before, there is not a higher removal rate when parent tree are present
#Lets look at the site also.
tshe_removal_conspecific_site<-tshe_data %>%
  group_by(Matches_Species,Stand) %>%
  summarize(Average_Removal_Rate = mean(removal_per_all))
#There is not the possibility to compare, however, when looking at the predation rate, we can see that tshe which is not present at ae10 expereiences the highest removal rate there. 
conspecfic_removal_tshe_site <- ggplot(tshe_removal_conspecific_site, aes(x = Stand, y = Average_Removal_Rate, fill = Matches_Species)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Removal Rate TSHE",
    x = "Stand",
    y = "Average Removal Rate (%)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Yes" = "lightgreen", "No" = "red")) +
  ylim(0, 100)

print(conspecfic_removal_tshe_site)
ggsave(filename = "conspecfic_removal_tshe_site.png", plot = conspecfic_removal_tshe_site, width = 5, height = 5, units = "in", dpi = 300,path = "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Plots/")


# PARENT TREE AROUND_ SITES -----------------------------------------------

#Conspecific

#I don't know if I can use those results as there is some sites without the seeds sp of some species. 
#Now lets look at the sites:
to04_data <- data_merged_6 %>% filter(Stand == "TO04")
to04_removal_conspecific <- to04_data %>%
  group_by(Matches_Species) %>%
  summarize(Average_Removal_Rate = mean(removal_per_all))
conspecfic_removal_to04 <- barplot(to04_removal_conspecific$Average_Removal_Rate, names.arg = to04_removal_conspecific$Matches_Species,
                                   col = c("lightgreen", "red"),
                                   main = "conspecfic_removal_to04",
                                   xlab = "Matches_Species",
                                   ylab = "Average Removal Rate",
                                   ylim = c(0, 100))
dev.copy(png, "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Plots/conspecfic_removal_to04.png")
dev.off()


av06_data <- data_merged_6 %>% filter(Stand == "AV06")
av06_removal_conspecific <- av06_data %>%
  group_by(Matches_Species) %>%
  summarize(Average_Removal_Rate = mean(removal_per_all))
conspecfic_removal_av06 <- barplot(av06_removal_conspecific$Average_Removal_Rate, names.arg = av06_removal_conspecific$Matches_Species,
                                   col = c("lightgreen", "red"),
                                   main = "conspecfic_removal_av06",
                                   xlab = "Matches_Species",
                                   ylab = "Average Removal Rate",
                                   ylim = c(0, 100))
dev.copy(png, "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Plots/conspecfic_removal_av06.png")
dev.off()

ae10_data <- data_merged_6 %>% filter(Stand == "AE10")
ae10_removal_conspecific <- ae10_data %>%
  group_by(Matches_Species) %>%
  summarize(Average_Removal_Rate = mean(removal_per_all))
conspecfic_removal_ae10 <- barplot(ae10_removal_conspecific$Average_Removal_Rate, names.arg = ae10_removal_conspecific$Matches_Species,
                                   col = c("lightgreen", "red"),
                                   main = "conspecfic_removal_ae10",
                                   xlab = "Matches_Species",
                                   ylab = "Average Removal Rate",
                                   ylim = c(0, 100))
dev.copy(png, "C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Plots/conspecfic_removal_ae10.png")
dev.off()


