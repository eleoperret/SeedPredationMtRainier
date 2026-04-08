####Code for CAMERA TRAP EXPERIMENT US 2017
####Last modified : 08.04.2026
####ETHZ Eléonore Perret
#### Code for measuring seed removal.  

# Loading libraries ------------------------------------------------------
library(readxl)
library(dplyr)
library(ggplot2)
library(glmmTMB)
library(lme4)
library(MASS)
library(DHARMa)
library(emmeans)
library(tidyr)





# Loading the data --------------------------------------------------------
# Set the working directory
getwd()
setwd("")

list.files("Datasets")

# Load datasets
#Datasets: camera data contains the information about the results of the camera traps
#Dataset: seed data contains all information about the cafeteria trial
#Dataset: SeedPredation_First_week contains the information about the experimental design (camera number, localisation etc..)
seed_predation <- read.csv("Datasets/SeedPredation_First_week.csv", sep=";")
all_data_seed<- read.csv("Datasets/seed_data.csv",sep= ";")
predators_data <- read_excel("Datasets/camera_data.xlsx")

# CAMERA TRAP DATA : Process_data ------------------------------------------------------------
#Because I have empty rows after the row 27 (this comes from excel), I will first delete all the rows below
seed_predation <- seed_predation %>% slice(1:27)

# SEED PREDATION: Process data -End Product (data_cleaned_2) ---------------------------------------------------
# Defining the number of seeds disposed for each species
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
#Creating a value for seed eaten
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

#Based on the amount of seeds disposed at the beginning
#Percentage of seeds eaten based on the amount offered
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

#Instead of a percentage, just a number  
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

#Preparing for my model
# Convert 'Seeds.Remaining' to numeric
data_cleaned_2$Seeds.Remaining <- as.numeric(data_cleaned_2$Seeds.Remaining)
# New variable 'Seeds.Placed' (seeds eaten + seeds remaining)
data_cleaned_2$Seeds.Placed <- data_cleaned_2$seeds_eaten + data_cleaned_2$Seeds.Remaining
# New variable 'Success' (seeds not eaten)
data_cleaned_2$Success <- data_cleaned_2$seeds_eaten

# Now 'Success' is the number of successes (seeds not eaten), and 'Seeds.Placed' is the number of trials.-- BINOMIAL RESPONSE

#Adding seed weights
species_weights <- c(ABAM = 0.024, ABLA = 0.02, CANO = 0.004, PSME = 0.007, THPL = 0.001, TSHE = 0.001)
data_cleaned_2$seed_weight <- species_weights[data_cleaned_2$Seed_sp]





# Overall check-------------------------------------------------------------------
#Removal
overall_removal <- data_cleaned_2 %>%
  mutate(removal_prop = seeds_eaten / Seeds.Placed) %>% 
  summarize(mean_removal= mean(removal_prop, na.rm = TRUE))


# Species
mean_se_data <- data_cleaned_2 %>%
  group_by(Seed_sp) %>%
  summarise(
    mean_seeds = mean(seeds, na.rm = TRUE),
    se_seeds = sd(seeds, na.rm = TRUE) / sqrt(n())
  )
print(mean_se_data)

# Stand
#Taking into account that there is not the same amount of weeks per stand
mean_se_data_stand <- data_cleaned_2 %>%
  group_by(Stand, Week) %>%
  summarise(mean_seeds_week = mean(seeds, na.rm = TRUE), .groups = "drop") %>%
  group_by(Stand) %>%
  summarise(
    mean_seeds = mean(mean_seeds_week, na.rm = TRUE),
    se_seeds = sd(mean_seeds_week, na.rm = TRUE) / sqrt(n())
  )

#Seed mix
mean_se_data_treatment <- data_cleaned_2 %>%
  group_by(Treatment, Stand, Seed_sp) %>%
  summarise(
    mean_seeds = mean(seeds, na.rm = TRUE),
    se_seeds = sd(seeds, na.rm = TRUE) / sqrt(n())
  )
ggplot(mean_se_data_treatment,aes(x = Treatment, y = mean_seeds, fill =   Treatment)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = mean_seeds - se_seeds,ymax = mean_seeds +      se_seeds),width = 0.2,position = position_dodge(width = 0.9)) +
  facet_grid(Seed_sp ~ Stand) +
  labs(x = "Treatment",y = "Mean number of seeds (± SE)") +
  theme_bw()
#There is no clear trend or impact of treatment on seed removal per species or stand

# Seed predator density analysis ------------------------------------------

# For Stand
predators_data$Stand <- factor(predators_data$Stand,levels = c("TO04", "AV06", "AE10"),labels = c("Low", "Mid", "High"))
# For Species_ID
predators_data$Species_ID <- factor(predators_data$Species_ID,  levels = c("Peromyscus maniculatus", "Tamias sp", "Lepus americanus", "Bird", "Flying squirrel", "Shrew?", "vole?", "Zapus?", "???"),  labels = c("P. maniculatus", "Tamias spp.", "L. americanus", "Birds", "G. oregonensis", "Sorex spp.", "Microtus spp.", "Zapus spp.", "Unidentified vertebrates"))

predator_data_clean <- predators_data %>%
  filter(Animal_presence == "Yes", !is.na(Species_ID), Species_ID != "NA")

predator_data_clean_tray <- predators_data %>%
  filter(Animal_presence == "Yes", !is.na(Species_ID), Species_ID != "NA")%>%
  filter(On_tray == "On")

# Summarize detections per species when the animal is "on the tray" - This makes it sure that the animal is interacting with the tray
detections_on_tray <- predator_data_clean_tray %>%
  group_by(Species_ID, Stand) %>%
  summarise(Total_Detections_On_Tray = n()) %>%
  arrange(desc(Total_Detections_On_Tray))

#Checking the dataset
#checking for the weeks
predator_data_clean_week<- predator_data_clean %>%
  group_by (Stand)%>%
  summarise (week= n_distinct(Week))
#AE10 only has 3 weeks... so I will make sure it is standartized
sampling_effort <- tibble(Stand = c("Low", "Mid", "High"), Weeks_sampled = c(4, 4, 3))


#Preparing data for model using all seed predators (not only if interacted with the tray)

#Predator detection per Stand 
predator_density_by_site <- predator_data_clean %>%
  group_by(Stand) %>%
  summarise(
    predator_detections = n(),                
    unique_species = n_distinct(Species_ID)   
  )
# Standardized with Weeks:
predator_density_by_site_standardized <- predator_density_by_site %>%
  left_join(sampling_effort, by = "Stand") %>%
  mutate(detections_per_week = predator_detections / Weeks_sampled)


#Predator detection per Stand and species
predator_density_site_species <- predator_data_clean %>%
  group_by(Stand, Species_ID) %>%
  summarise(
    predator_detections = n(),   # total number of images where this species was detected at this site
    .groups = "drop"
  )
# Standartized with Weeks:
predator_density_site_species_standardized <- predator_density_site_species %>%
  left_join(sampling_effort, by = "Stand") %>%
  mutate(detections_per_week = predator_detections / Weeks_sampled)


#Preparing for the plot
#Changing names for clarity and plots 

#Color for species
species_colors <- c(
  "P. maniculatus" = "#332288",  # dark blue
  "Tamias spp." = "#88CCEE",               # sky blue
  "L. americanus" = "#117733",         # green
  "Birds" = "#DDCC77",                    # mustard yellow
  "G. oregonensis" = "#CC6677",   # rosy pink
  "Sorex spp." = "#AA4499",                # purple
  "Microtus spp." = "#44AA99",                 # teal
  "Zapus spp." = "#999933",                # olive
  "Unidentified vertebrates" = "#DDDDDD"            # light gray
)

#Normal (not standardized per week)
ggplot(predator_density_site_species, aes(x = Stand, y = predator_detections, fill = Species_ID)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = species_colors) +
  labs(title = "Predator Detections per Site and Species",x = "Site",y = "Number of Detections",fill = "Species") +
  theme_minimal()
#Standardized
ggplot(predator_density_site_species_standardized, aes(x = Stand, y = detections_per_week, fill = Species_ID)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = species_colors) +
  labs(title = "Predator Detections per Site and Species",x = "Site",y = "Number of Detections",fill = "Species") +
  theme_minimal()
#Hudge difference!



#Model (dataset = predator_density_site_species_standartized)
#Trying a model to know if some species get detected more often than others and are some sites associated with more predator detections?


#I tried different model but either they could not converge or they showed overdispersion. Here is the model that makes the most sense out of the data and that is representing our data. 
model_nb <- glm.nb(predator_detections ~ Stand + Species_ID + offset(log(Weeks_sampled)),data = predator_density_site_species_standardized,control = glm.control(maxit = 50))
summary(model_nb)
#Checking for the residuals
sim_res_nb <- simulateResiduals(model_nb)
plot(sim_res_nb)
#Looks not super good... but the closest I could get. Still shows that there are differences in detections among species and that for the high site, the detections of Tamia are higher.  

em_camera_stand <- emmeans(model_nb,~ Stand, type = "response",at = list(Weeks_sampled = 1))  # fix all predictions to 1 week
pairwise_stand <- pairs(em_camera_stand)
pairwise_stand

#Plot for the manuscript
# Okabe-Ito colorblind-friendly palette (8 colors)
cb_palette <- c(
  "#0072B2", # blue
  "#E69F00", # orange
  "#009E73", # green
  "#F0E442", # yellow
  "#56B4E9", # light blue
  "#D55E00", # red
  "#CC79A7", # pink
  "#999999", # grey
  "#800080"  # new purple color
)

# Assign to your species manually (truncate or repeat if needed)
species_colors_cb <- c(
  "P. maniculatus" = "#0072B2",  # blue
  "Tamias spp."             = "#E69F00",  # orange
  "L. americanus"       = "#009E73",  # green
  "Birds"                  = "#F0E442",  # yellow
  "G. oregonensis"  = "#D55E00",  # red
  "Sorex spp."              = "#CC79A7",  # purple
  "Microtus spp."               = "#56B4E9",  # light blue
  "Zapus spp."              = "#999999",   # grey
  "Unidentified vertebrates"         = "#800080"
)

data_pie <- predator_density_site_species_standardized %>%
  #filter(Species_ID != "Not identified") %>%  # remove unidentified species
  group_by(Species_ID) %>%
  summarise(total_detections = sum(detections_per_week))

#Differences per site
data_doughnut <- predator_density_site_species_standardized %>%
  #filter(Species_ID != "Not identified") %>%  # remove unidentified species
  group_by(Stand, Species_ID) %>%
  summarise(total_detections = sum(detections_per_week), .groups = "drop")

# Normalize detections per site so each doughnut sums to 1
data_doughnut_norm <- data_doughnut %>%
  group_by(Stand) %>%
  mutate(perc = total_detections / sum(total_detections)) %>%
  ungroup()

data_doughnut_norm$Stand <- factor(data_doughnut_norm$Stand, levels = c("Low", "Mid", "High"))

ggplot(data_doughnut_norm, aes(x = 2, y = perc, fill = Species_ID)) +
  geom_col(color = "white", width = 1) +
  coord_polar(theta = "y", start = 0) +
  xlim(0.5, 2.5) +
  scale_fill_manual(values = species_colors_cb) +
  facet_wrap(~Stand) +
  theme_void() +
  labs(title = "Predator detections per site (doughnut chart)",fill = "Species") +
  theme(legend.position = "right",strip.text = element_text(size = 10, face = "bold"))

# MODEL FOR SEED REMOVAL------------------------------------------------------------------
#The models are based on ecological questions. I want them to follow my research questions. 

#adding an OBS as seeds are dependent from each other (not every seed is place seperately)
data_cleaned_2$ObsID <- factor(1:nrow(data_cleaned_2))

##**Question 2**
##WHAT ARE THE PREFERRED SEED AND IS THIS PREFERENCE CONSISTENT OVER ELEVATION? 
#Reload the data_cleaned_2 and then reorder based on the graphical results. PSME most eaten and AE10 most removal
data_cleaned_2$Seed_sp <- factor(data_cleaned_2$Seed_sp,levels = c("PSME","THPL", "TSHE", "ABAM", "CANO", "ABLA"),labels = c("Pseudotsuga menziesii","Thuja plicata", "Tsuga heterophylla","Abies amabilis", "Callitropsis nootkatensis","Abies lasiocarpa"))
data_cleaned_2$Stand <- factor(data_cleaned_2$Stand,levels = c("AE10", "AV06", "TO04"),labels = c("High", "Mid", "Low"))

#Based on previous testing this models is the best model as the residuals look good and it doesn't show a strong over or under-dispersion.
glmer_modelOLRE_New <- glmer(cbind(Success, Seeds.Placed - Success) ~ Stand + Seed_sp+(1 | Camera) + (1 | ObsID) + (1|Week),family = binomial,data = data_cleaned_2,control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

#Simulation the residuals to check for distribution of residuals
sim_res <- simulateResiduals(fittedModel = glmer_modelOLRE_New, plot = TRUE)
testDispersion(sim_res)#does not show overdispersion; was overdisperse without the ObsID
#Summary of the model
summary(glmer_modelOLRE_New)

#Result : 
##The species are not removed the same. There is some preferences.
##The seeds are not removed the same per elevation

#POST-HOC tests
em_stand <- emmeans(glmer_modelOLRE_New, ~ Stand , type = "response")
em_stand_df <- as.data.frame(em_stand)
stand_contrasts <- pairs(em_stand, adjust = "tukey")
summary(em_stand)
em_seeds <- emmeans(glmer_modelOLRE_New, ~ Seed_sp , type = "response")
em_seeds_df <- as.data.frame(em_seeds)
seed_contrasts <- pairs(em_seeds, adjust = "tukey")
summary(seed_contrasts)

em_all <- emmeans(glmer_modelOLRE_New, ~ Stand + Seed_sp , type = "response")
em_df <- as.data.frame(em_all)

em_df$Seed_sp <- factor(em_df$Seed_sp,levels = c("Thuja plicata","Tsuga heterophylla","Pseudotsuga menziesii","Abies amabilis","Callitropsis nootkatensis","Abies lasiocarpa"))
em_df$Stand <- factor(em_df$Stand,levels = c("Low", "Mid", "High"))

#Plot for manuscript
ggplot(em_df, aes(x = Stand, y = prob, color = Stand, shape = Seed_sp)) +
  # Species-level estimates
  geom_point(position = position_dodge(width = 0.6),size = 7) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),position = position_dodge(width = 0.6),width = 0.2) +
  # Stand means as dotted horizontal lines
  geom_hline(data = em_stand_df,aes(yintercept = prob, color = Stand),linetype = "dotted",linewidth = 1,inherit.aes   = FALSE) +
  # Colors per stand
  scale_color_manual(values = c("Low"  = "darkred","Mid"  = "darkorange","High" = "blue")) +
  # Shapes per species
  scale_shape_manual(values = c("Thuja plicata" = 16,"Tsuga heterophylla" = 17,"Pseudotsuga menziesii" = 15,"Abies lasiocarpa" = 3,"Callitropsis nootkatensis" = 7,"Abies amabilis" = 8)) +
  labs(title = "Estimated Probability of Seed Removal",y = "Probability of Removal",x = "Stand",color = "Stand",     shape = "Seed species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),legend.position = "right")


##**Question 3**
##IS THIS REMOVAL DEPENDENT OF HAVING CONSPECIFIC AROUND OR NOT?
#Does seed community composition influence seed removal? Specifically, are seeds more or less likely to be removed when presented in novel versus familiar seed communities??
#Checking by species group (low vs high species)
#Are species more removed when they are in their naturally occuring range?

#ELEVATION GROUP
#(low vs high species)
data_cleaned_2_grouped <- data_cleaned_2 %>%
  mutate(Elevation_group = case_when(
    Seed_sp %in% c("ABLA", "ABAM", "CANO") ~ "High",
    Seed_sp %in% c("THPL", "TSHE", "PSME") ~ "Low",
    TRUE ~ NA_character_
  ))


#This model solves this underdispersion problem.
model_bb <- glmmTMB(cbind(Success, Seeds.Placed - Success) ~ Elevation_group * Stand +(1 | Camera) + (1 | ObsID) + (1|Week),data = data_cleaned_2_grouped,family = betabinomial())

#Checking model
sim_res_2 <- simulateResiduals(fittedModel = model_bb, plot = TRUE)
testDispersion(model_bb)
summary(model_bb)

# POST-HOC Test
em2 <- emmeans(model_bb, ~ Stand * Elevation_group, type = "response")

pairs(em2, by = "Stand")
pairs(em2, by = "Elevation_group")
em_df2 <- as.data.frame(em2)

em_df2$Elevation_group <- factor(em_df2$Elevation_group,levels = c("Low", "High"))
em_df2$Stand <- factor(em_df2$Stand,levels = c("TO04", "AV06", "AE10"),labels = c("Low", "Mid", "High"))

# Plot of removal probability by stand and elevation group.
#Plot for the manuscript
pd <- position_dodge(width = 0.4)
ggplot(em_df2,aes(x = Stand,y = prob,color = Elevation_group,group = Elevation_group)) +
  geom_point(position = pd, size = 5) +
  geom_line(position = pd, linewidth = 1.5) +
  geom_errorbar(aes(ymin = asymp.LCL,ymax = asymp.UCL,group = Elevation_group),position = pd,width = 0.15) +
  scale_color_manual(values = c("Low" = "black", "High" = "grey")) +
  labs(title = "Predicted Seed Removal Probability by Elevation Group and Stand",y = "Predicted Probability of Seed   Removal"   ,x = "Forest Stand Elevation",color = "Seed Elevation Group") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_minimal() +
  theme(text = element_text(size = 14))


##**Question 3**Does seed community composition influence seed removal? Specifically, are seeds more or less likely to be removed when presented in novel versus familiar seed communities??

#I need to do that as there is not always all seeds presented everytime. Sometimes, I have only high elevation seed species etc... this made it almost impossible to converge a model (could be done maybe using bayesian statistics) but I decided to go like this by splitting my dataset in two. 

#Treatment Low
data_subset <- data_cleaned_2 %>% 
  filter(!(Treatment =="High"))
data_subset <- data_subset %>% 
  filter(!(Seed_sp %in% c("ABAM", "CANO","ABLA")))
#Treatment high
data_subset_2 <- data_cleaned_2 %>% 
  filter(!(Treatment =="Low"))
data_subset_2 <- data_subset_2 %>% 
  filter(!(Seed_sp %in% c("PSME", "THPL", "TSHE")))

#This is the best model as I can apply it to both subset data and make the results comparable. Due to the high predation rate of P. menziesii, it was impossible to create a model that we checking for residuals and overdispersion made sense. This is why I decided to look at overall if the treatment affected seed removal based on species. Looking also at the plots of the removal per treatment (see section above: Overall check), there was no clear distnctions between the treatments as which indicates that there is no a strong reason to do otherwise. 
data_subset$ObsID <- factor(1:nrow(data_subset))
data_subset_2$ObsID <- factor(1:nrow(data_subset_2))

#Model : Low elevation seed species vs all treatment
glmer_modelOLRE_New3 <- glmer(cbind(Success, Seeds.Placed - Success) ~ Treatment * Seed_sp +(1 | Camera) + (1 | ObsID) + (1 | Week),family = binomial,data = data_subset,control = glmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 2e5)))

#Checking the model for low elevation seed species
sim_res <- simulateResiduals(fittedModel = glmer_modelOLRE_New3, plot = TRUE)
testDispersion(sim_res)#does not show overdispersion 
summary(glmer_modelOLRE_New3)

# POST-HOC Test
# Estimated marginal means for Treatment for the low seed species
em_treatment_low <- emmeans(glmer_modelOLRE_New3, ~ Treatment | Seed_sp, type = "response")
em_treatment_low
treatment_contrasts_low <- pairs(em_treatment_low, adjust = "tukey")
treatment_contrasts_low

#there is no statistical difference between the treatments. Seeds are removed the same no matter if there are other seed species around or not. 

#Model : high elevation seed species vs all treatment
glmer_modelOLRE_New4 <- glmer(cbind(Success, Seeds.Placed - Success) ~ Treatment * Seed_sp+(1 | Camera) + (1 | ObsID) + (1|Week) ,family = binomial,data = data_subset_2,control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7)))

#Checking the model
sim_res <- simulateResiduals(fittedModel = glmer_modelOLRE_New4, plot = TRUE)
testDispersion(sim_res)#does not show overdispersion 
summary(glmer_modelOLRE_New4)

# POST-HOC Test
# Estimated marginal means for Treatment for the high seed species
em_treatment_high <- emmeans(glmer_modelOLRE_New4, ~ Treatment | Seed_sp, type = "response")
em_treatment_high
treatment_contrasts_high <- pairs(em_treatment_high, adjust = "tukey")
treatment_contrasts_high
#there is no statistical difference between the treatments. Seeds are removed the same no matter if there are other seed species around or not.


