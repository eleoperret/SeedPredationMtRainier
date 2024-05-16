#Plots

load("C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/data_merged_6.RData")

unique(data_merged_6$Stand)

ABAM<- subset(data_merged_6,Seed_sp=="ABAM")
ABLA<- subset(data_merged_6,Seed_sp=="ABLA")
CANO<- subset(data_merged_6,Seed_sp=="CANO")
PSME<- subset(data_merged_6,Seed_sp=="PSME")
THPL<- subset(data_merged_6,Seed_sp=="THPL")
TSHE<- subset(data_merged_6,Seed_sp=="TSHE")
AE10_site<- subset(data_merged_6,Stand=="AE10")
AV06_site<- subset(data_merged_6, Stand=="AV06")
TO04_site<- subset(data_merged_6,Stand=="TO04")
Treatment_all<-subset(data_merged_6,Treatment=="All")
Treatment_high<-subset(data_merged_6,Treatment=="High")
Treatment_low<-subset(data_merged_6,Treatment=="Low")

plot(TSHE$Treatment,TSHE$removal_per_all)
plot(TSHE$Stand,TSHE$removal_per_all)
plot(ABAM$Treatment,ABAM$removal_per_all)
plot(ABAM$Stand,ABAM$removal_per_all)
plot(ABLA$Treatment,ABLA$removal_per_all)
plot(ABLA$Stand,ABLA$removal_per_all)
plot(PSME$Treatment,PSME$removal_per_all)
plot(PSME$Stand,PSME$removal_per_all)
plot(THPL$Treatment,THPL$removal_per_all)
plot(THPL$Stand,THPL$removal_per_all)
plot(CANO$Stand,CANO$removal_per_all)
plot(CANO$Treatment,CANO$removal_per_all)


plot(data_merged_6$Stand,data_merged_6$removal_per_all)
plot(data_merged_6$Treatment,data_merged_6$removal_per_all)
plot(data_merged_6$Seed_sp,data_merged_6$removal_per_all)
plot(data_merged_6$Matches_Species,data_merged_6$removal_per_all)
plot(data_merged_6$species_diversity_camera,data_merged_6$removal_per_all)
unique(data_merged_6$mean_dbh_y)
plot(data_merged_6$number_of_trees,data_merged_6$removal_per_all)
data_merged_6$dbhcat<-ifelse (data_merged_6$mean_dbh_y<20,"small",
                              ifelse(data_merged_6$mean_dbh_y<30,"medium","large"))
# Convert dbhcat to an ordered factor with specified levels
data_merged_6$dbhcat <- factor(data_merged_6$dbhcat, levels = c("small", "medium", "large"), ordered = TRUE)
plot(data_merged_6$dbhcat,data_merged_6$removal_per_all)

plot(AE10_site$Seed_sp,AE10_site$removal_per_all)
plot(AV06_site$Seed_sp,AV06_site$removal_per_all)
plot(TO04_site$Seed_sp,TO04_site$removal_per_all)

plot(AE10_site$dbhcat)
plot(AV06_site$dbhcat)
plot(TO04_site$dbhcat)

plot(AE10_site$Treatment,AE10_site$removal_per_all)
plot(AV06_site$Treatment,AV06_site$removal_per_all)
plot(TO04_site$Treatment,TO04_site$removal_per_all)

plot(AE10_site$number_of_trees,AE10_site$removal_per_all)
plot(AV06_site$number_of_trees,AV06_site$removal_per_all)
plot(TO04_site$number_of_trees,TO04_site$removal_per_all)

plot(AE10_site$species_diversity_camera,AE10_site$removal_per_all)
plot(AV06_site$species_diversity_camera,AV06_site$removal_per_all)
plot(TO04_site$species_diversity_camera,TO04_site$removal_per_all)

plot(Treatment_all$Seed_sp,Treatment_all$removal_per_all)




