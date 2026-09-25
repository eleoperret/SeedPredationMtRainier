####Code for CAMERA TRAP EXPERIMENT US 2017
####Autumn 2023
####ETHZ Eléonore Perret
#### Code for measuring seed removal.  

# Loading librairies ------------------------------------------------------
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("tidyr")
# install-packages("car")
#install.packages("ggpubr")
#install.packages("DHARMa")
#install.packages("emmeans")
#install.packages("ggpattern")
#install.packages("insight")
library(DHARMa)
library(dplyr)
library(ggplot2)
library(tidyr)
library(car)
library(lme4)
library(ggpubr)
library(emmeans)
library(ggpattern)
library(readxl)
library(MASS)
library(betareg)
library(sjPlot)
library(ggeffects)


# Loading the data --------------------------------------------------------
# Set the working directory
getwd()
setwd("C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets")
setwd ("C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets")
# List files in the main directory
list.files()

# Load datasets
seed_predation <- read.csv("SeedPredation_First_week.csv", sep=";")
all_data_seed<- read.csv("seed_data.csv",sep= ";")
predators_data <- read_excel("camera_data.xlsx")
load("data_cleaned_2.RData") #Done after cleaning the data. See Seed predation results

# CAMERA TRAP DATA : Process_data ------------------------------------------------------------
#Because I have empty rows after the row 27 (this comes from excel), I will first delete all the rows below
seed_predation <- seed_predation %>% slice(1:27)

# SEED PREDATION RESULTS ---------------------------------------------------
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

str(data_cleaned_2)

#Based on the amount of seeds disposed at the beginning
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
# New variable 'Success' (seeds eaten)
data_cleaned_2$Success <- data_cleaned_2$Seeds.Remaining

# Now 'Success' is the number of successes (seeds eaten), and 'Seeds.Placed' is the number of trials.-- BINOMIAL RESPONSE

#Adding seed weights
species_weights <- c(ABAM = 0.024, ABLA = 0.02, CANO = 0.004, PSME = 0.007, THPL = 0.001, TSHE = 0.001)
data_cleaned_2$seed_weight <- species_weights[data_cleaned_2$Seed_sp]

# PLOTS -------------------------------------------------------------------
str(data_cleaned_2)

# Basic scatter plot of Seeds.Placed vs Success
ggplot(data_cleaned_2, aes(x = Seeds.Placed, y = seeds)) +
  geom_point(alpha = 0.6, shape = 21, color = "black", fill = "skyblue", size = 3) +  # Nicer points
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed", size = 1) +  # Linear regression line
  labs(
    title = "Seeds Placed vs Standardized Seed Consumption",
    x = "Seeds Placed",
    y = "Standardized Success",
    caption = "Each point represents a seed station"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )



# Plot for species
ggplot(data_cleaned_2, aes(x = Seed_sp, y = seeds, fill = Seed_sp)) +
  geom_jitter(width = 0.3, alpha = 0.4, shape = 21, color = "black") +  # the point cloud
  stat_summary(fun = mean, geom = "point", size = 5, color = "black") +  # mean dot
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "black") +  # error bars
  scale_fill_manual(values = c(
    "THPL" = "darkred", "TSHE" = "indianred", "PSME" = "lightcoral", 
    "ABLA" = "blue", "CANO" = "lightblue", "ABAM" = "darkblue")) +
  labs(title = "Standardized Seed Consumption by Species",
       x = "Species", y = "Standardized Removal") +
  theme_minimal() +
  theme(legend.position = "none")
mean_se_data <- data_cleaned_2 %>%
  group_by(Seed_sp) %>%
  summarise(
    mean_seeds = mean(seeds, na.rm = TRUE),
    se_seeds = sd(seeds, na.rm = TRUE) / sqrt(n())
  )
print(mean_se_data)

# Plot for stand
ggplot(data_cleaned_2, aes(x = Stand, y = seeds, fill = Stand)) +
  geom_jitter(width = 0.2, alpha = 0.4, shape = 21, color = "black") +  # the "cloud"
  stat_summary(fun = mean, geom = "point", size = 4, color = "black") +  # mean
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "black") +  # SE bars
  scale_fill_manual(values = c("TO04" = "darkred", "AV06" = "orange", "AE10" = "lightblue")) +
  labs(title = "Standardized Seed Consumption by Stand",
       x = "Stand", y = "Standardized Removal") +
  theme_minimal()

#Plot for treatment
ggplot(data_cleaned_2, aes(x = Treatment, y = seeds, fill = Treatment)) +
  geom_jitter(width = 0.2, alpha = 0.4, shape = 21, color = "black") +  # the "cloud"
  stat_summary(fun = mean, geom = "point", size = 4, color = "black") +  # mean
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "black") +  # SE bars
    scale_fill_manual(values = c("Low" = "darkred", "All" = "darkgrey", "High" = "darkblue")) +
  labs(title = "Standardized Seed Consumption by Treatment",
       x = "Treatment", y = "Standardized Removal") +
  theme_minimal()


# Plot the average standardized success by Week
ggplot(data_cleaned_2, aes(x = as.factor(Week), y = seeds, fill = as.factor(Week))) +
  geom_jitter(width = 0.15, alpha = 0.4, shape = 21, color = "black", size = 2) +  # Individual points
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, fill = "darkgreen") +  # Mean
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "darkgreen") +  # Error bars (SE)
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Standardized Seed Consumption by Week",
    x = "Week",
    y = "Standardized Success"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )

# Plot the average standardized success by Species and Stand
ggplot(data_cleaned_2, aes(x = Seed_sp, y = seeds, color = Stand, fill = Stand)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.6), 
              alpha = 0.5, shape = 20, size = 2) +
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(width = 0.6),
               shape = 23, size = 5, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               position = position_dodge(width = 0.6), width = 0.2) +
  scale_fill_manual(values = c("TO04" = "darkred", "AV06" = "salmon", "AE10" = "darkblue")) +
  scale_color_manual(values = c("TO04" = "darkred", "AV06" = "salmon", "AE10" = "darkblue")) +
  labs(
    title = "Standardized Seed Consumption by Species and Stand",
    x = "Seed Species",
    y = "Standardized Removal"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
  )


# Plot the average standardized success by Species and Treatment
ggplot(all_treatment_data, aes(x = Seed_sp, y = seeds, color = Treatment, fill = Treatment)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.6),
              alpha = 0.5, shape = 20, size = 2) +
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(width = 0.6),
               shape = 21, size = 5, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               position = position_dodge(width = 0.6), width = 0.2) +
  scale_fill_manual(values = c("High" = "darkblue", "All" = "darkgrey", "Low" = "darkred")) +
  scale_color_manual(values = c("High" = "darkblue", "All" = "darkgrey", "Low" = "darkred")) +
  labs(
    title = "Standardized Seed Consumption by Species and Treatment",
    x = "Species",
    y = "Standardized Success"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
  )


# Plot the average standardized success by Treatment and Stand
ggplot(data_cleaned_2, aes(x = Stand, y = seeds, color = Treatment, fill = Treatment)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.6),
              alpha = 0.5, shape = 20, size = 2) +
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(width = 0.6),
               shape = 23, size = 5, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               position = position_dodge(width = 0.6), width = 0.2) +
  scale_fill_manual(values = c("Low" = "darkred", "All" = "black", "High" = "lightblue")) +
  scale_color_manual(values = c("Low" = "darkred", "All" = "black", "High" = "lightblue")) +
  labs(
    title = "Standardized Seed Consumption by Treatment and Stand",
    x = "Treatment",
    y = "Standardized Success"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
  )


# Create a new variable for seed weight per seed placed
data_cleaned_2$seed_weight_per_seed <- data_cleaned_2$seed_weight / data_cleaned_2$Seeds.Placed

# Calculate the removal rate
data_cleaned_2$removal_rate <- (data_cleaned_2$Seeds.Disposed - data_cleaned_2$Seeds.Remaining) / data_cleaned_2$Seeds.Disposed

# Fit a linear model to check if removal rate increases with seed weight per seed
model_removal <- lm(seeds ~ seed_weight_per_seed, data = data_cleaned_2)

# Summarize the model
summary(model_removal)

# Visualize the relationship
ggplot(data_cleaned_2, aes(x = seed_weight_per_seed, y = seeds)) +
  geom_point(color = "black", alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = TRUE) +
  labs(
    x = "Seed Weight per Seed Placed (g)",
    y = "Standardized Seed Removal",
    title = "Relationship Between Seed Weight and Removal Rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

#Checking by species group (low vs high species)
data_cleaned_2_grouped <- data_cleaned_2 %>%
  mutate(Elevation_group = case_when(
    Seed_sp %in% c("ABLA", "ABAM", "CANO") ~ "High",
    Seed_sp %in% c("THPL", "TSHE", "PSME") ~ "Low",
    TRUE ~ NA_character_
  ))

ggplot(data_cleaned_2_grouped, aes(x = Elevation_group, y = seeds, fill = Elevation_group)) +
  geom_jitter(width = 0.2, alpha = 0.4, shape = 21, color = "black") +  # individual points
  stat_summary(fun = mean, geom = "point", size = 5, color = "black") +  # group means
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "black") +  # error bars
  scale_fill_manual(values = c("High" = "steelblue", "Low" = "indianred")) +
  labs(title = "Standardized Seed Consumption by Elevation Group",
       x = "Elevation Group", y = "Standardized Removal") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(data_cleaned_2_grouped, aes(x = Elevation_group, y = seeds, fill = Elevation_group)) +
  geom_jitter(width = 0.2, alpha = 0.4, shape = 21, color = "black") +
  stat_summary(fun = mean, geom = "point", size = 5, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "black") +
  scale_fill_manual(values = c("High" = "steelblue", "Low" = "indianred")) +
  labs(title = "Standardized Seed Consumption by Elevation Group and Stand",
       x = "Elevation Group", y = "Standardized Removal") +
  facet_wrap(~Stand) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(data_cleaned_2_grouped, aes(x = Elevation_group, y = seeds, fill = Stand)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(0.8), width = 0.6, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_dodge(0.8), width = 0.2, color = "black") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Seed Removal by Elevation Group and Stand",
       x = "Elevation Group", y = "Standardized Removal") +
  theme_minimal()


# MODEL FOR OVERALL ------------------------------------------------------------------
data_cleaned_2$Treatment <- factor(data_cleaned_2$Treatment, levels = c("All", "Low", "High"), labels = c("All species", "Low elevation sp.", "High elevation sp."))

data_cleaned_2$Seed_sp <- factor(data_cleaned_2$Seed_sp, levels = c("PSME", "THPL", "TSHE","ABLA","CANO","ABAM"), labels = c( "P.menziesii", "T.plicata", "T.heterophylla", "A.lasiocarpa", "C.nootkatensis","A.amabilis"))

data_cleaned_2$Stand <- factor(data_cleaned_2$Stand, levels = c("TO04","AV06","AE10"), labels = c("Low", "Mid", "High"))


# glmer_model <- glmer(cbind(Success, Seeds.Placed - Success) ~ Stand * Seed_sp * Treatment + (1|Camera) + (1|Week) , 
#                  data = data_cleaned_2, 
#                  family = binomial)
#Rank defficient and cannot converge. 
#let's try another tactic. 

#Using an OLRE for removing the overdispersion
#Because I have overdipersion (more variability in my data than the model expects)
data_cleaned_2$ObsID <- factor(1:nrow(data_cleaned_2))

# #Testing new model
# glmer_modelOLRE <- glmer(cbind(Success, Seeds.Placed - Success) ~ Stand * Seed_sp * Treatment + (1|Camera) + (1|Week) +                  (1 | ObsID),
#                  family = binomial,
#                  data = data_cleaned_2)
# #Doesn't converge = not able to find the perfect estimate parameter: Need to do something
# #Using a optimizer helps R take its time to find the estimate and help the model to find estimate parameters.
# glmer_modelOLRE <- glmer(cbind(Success, Seeds.Placed - Success) ~ Stand * Seed_sp * Treatment + (1|Camera) + (1|Week) +                  (1 | ObsID),
#                  family = binomial,
#                 data = data_cleaned_2,
#                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
# 
# #Rank deficient because there is treatment high and low where not all species have it.
# 
# #other approach based on research questions.

##**Question 1**
##WHAT ARE THE PREFERRED SEED AND IS THIS PREFERENCE CONSISTENT OVER ELEVATION? or
##WHICH SPECIES IS REMOVED THE MOST AND WHERE THE MOST?

#**Model 1**: Are the species removed all equally? Are the seeds removed equally at each stand? Are the species removed equally no matter if they are surrounded with more seeds or not and from different community?
glmer_modelOLRE_New <- glmer(
  cbind(Success, Seeds.Placed - Success) ~ Stand + Seed_sp+
     (1 | Camera) + (1 | ObsID) + (1|Week),
  family = binomial,
  data = data_cleaned_2,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

#Simulation the residuals to check for distribution of residuals
sim_res <- simulateResiduals(fittedModel = glmer_modelOLRE_New, plot = TRUE)
#Summary of the model
summary(glmer_modelOLRE_New)

em_stand <- emmeans(glmer_modelOLRE_New, ~ Stand , type = "response")
em_stand_df <- as.data.frame(em_stand)
stand_contrasts <- pairs(em_stand, adjust = "tukey")
em_seeds <- emmeans(glmer_modelOLRE_New, ~ Seed_sp , type = "response")
em_seeds_df <- as.data.frame(em_stand)
seed_contrasts <- pairs(em_seeds, adjust = "tukey")
summary(seed_contrasts)

em_both <- emmeans(glmer_modelOLRE_New, ~ Stand + Seed_sp, type = "response")
all_contrasts <- pairs(em_both, adjust = "tukey")
em_df <- as.data.frame(em_both)

# Pairwise comparisons *within* Stand across species:
pairs(em_both, by = "Stand")
pairs(em_both, by = "Seed_sp")

#Changing Variable placement and names
em_df$StandLabel <- factor(em_df$Stand,
                           levels = c("TO04", "AV06", "AE10"),
                           labels = c("Low", "Mid", "High"))

em_stand_df$StandLabel <- factor(em_stand_df$Stand,
                                 levels = c("TO04", "AV06", "AE10"),
                                 labels = c("Low", "Mid", "High"))
em_df$Seed_sp <- factor(em_df$Seed_sp, levels = c("THPL", "TSHE", "PSME", "ABAM", "CANO", "ABLA"))
em_df$Seed_sp <- factor(em_df$Seed_sp,
                           levels = c("THPL", "TSHE", "PSME", "ABAM", "CANO", "ABLA"),
                           labels = c("Thuja plicata", "Tsuga heterophylla", "Pseudotsuga menziesii","Abies amabilis", "Callitropsis nootkatensis","Abies lasiocarpa"))


# Custom dodge width
dodge_width <- 0.5

#OVERALL PLOT
ggplot(em_df, aes(x = StandLabel, y = prob, color = Seed_sp, shape = StandLabel)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.7),
                width = 0.2) +
  
  # Add stand-level means (black diamonds)
  geom_point(data = em_stand_df, aes(x = StandLabel, y = prob),
             color = "black", size = 4, shape = 18) +
  geom_errorbar(data = em_stand_df,
                aes(x = StandLabel, ymin = asymp.LCL, ymax = asymp.UCL),
                color = "black",linetype="dotted", width = 0.1) +
  
  # Custom color for species
  scale_color_manual(values = c(
    "THPL" = "#D55E00",    # vermilion
    "TSHE" = "#E69F00",    # orange
    "PSME" = "#F0E442",    # bluish green
    "ABLA" = "#0072B2",    # yellow
    "CANO" = "#56b4e9",    # sky blue
    "ABAM" = "#009E73"     # blue
  )) +
  
  # Optional: specify custom shapes for consistency
  scale_shape_manual(values = c("Low" = 16, "Mid" = 17, "High" = 15)) +
  
  labs(
    title = "Estimated Probability of Seed Removal",
    y = "Probability of Removal",
    x = "Stand",
    color = "Seed Species",
    shape = "Stand"
  ) +
  theme_minimal()



ggplot(em_df, aes(x = Seed_sp, y = prob, color = StandLabel, shape = StandLabel)) +
  geom_point(position = position_dodge(width = dodge_width), size = 5) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = dodge_width),
                width = 0.2) +
  
  # Custom color for stands (now used for StandLabel)
  scale_color_manual(values = c(
    "Low" = "orange",   # Blue
    "Mid" = "forestgreen",   # Green
    "High" = "darkblue"   # Vermilion
  )) +
  
  scale_shape_manual(values = c("Low" = 16, "Mid" = 17, "High" = 15)) +
  
  labs(
    title = "Estimated Probability of Seed Removal by Species and Stand",
    x = "Seed Species",
    y = "Probability of Removal",
    color = "Stand",
    shape = "Stand"
  ) +
  theme_minimal()






#Result : 
##The species are not removed the same. There is some preferences.
##The seeds are not removed the same per elevation


#Other 
# Get predictions across seed species and stand
pred <- ggpredict(glmer_modelOLRE_New, terms = c("Seed_sp", "Stand"))

# Make sure levels match your existing labeling
pred$x <- factor(pred$x, 
                 levels = c("P.menziesii", "T.plicata", "T.heterophylla", "A.lasiocarpa", "C.nootkatensis","A.amabilis"))

# Filter to the 3 species you want to show
species_to_plot <- c("P.menziesii", "T.plicata", "T.heterophylla", "A.lasiocarpa", "C.nootkatensis","A.amabilis" )
pred_filtered <- pred %>% filter(x %in% species_to_plot)

# Plot
ggplot(pred_filtered, aes(x = x, y = predicted, color = group)) +
  geom_point(position = position_dodge(0.5), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.2, position = position_dodge(0.5)) +
  labs(
    title = "Predicted Seed Removal Probability by Species and Stand",
    x = "Seed Species",
    y = "Probability of Removal",
    color = "Stand"
  ) +
  scale_color_manual(values = c(
    "Low" = "#E69F00",
    "Mid" = "#009E73",
    "High" = "#56B4E9"
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#End of other




##IS THIS REMOVAL DEPENDENT OF HAVING CONSPECIFIC AROUND OR NOT?
#Checking by species group (low vs high species)
data_cleaned_2_grouped <- data_cleaned_2 %>%
  mutate(Elevation_group = case_when(
    Seed_sp %in% c("A.lasiocarpa", "C.nootkatensis","A.amabilis") ~ "High",
    Seed_sp %in% c("P.menziesii", "T.plicata", "T.heterophylla") ~ "Low",
    TRUE ~ NA_character_
  ))

# Plot the average standardized success by Group and Stand
ggplot(data_cleaned_2_grouped, aes(x = Elevation_group, y = seeds, color = Stand, fill = Stand)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.6),
              alpha = 0.5, shape = 20, size = 2) +
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(width = 0.6),
               shape = 23, size = 5, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               position = position_dodge(width = 0.6), width = 0.2) +
  scale_fill_manual(values = c("Low" = "#E69F00", "Mid" = "#009E73", "High" = "#56B4E9")) +
  scale_color_manual(values = c("Low" = "#E69F00", "Mid" = "#009E73", "High" = "#56B4E9")) +
  labs(
    title = "Standardized Seed Consumption by Treatment and Stand",
    x = "Elevation Group",
    y = "Standardized Success"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
  )

#**Model 2**: Are species more removed when they are in their naturally occuring range?
glmer_modelOLRE_New2 <- glmer(
  cbind(Success, Seeds.Placed - Success) ~ Elevation_group * Stand+
    (1 | Camera) + (1 | ObsID) + (1|Week) ,
  family = binomial,
  data = data_cleaned_2_grouped,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
overdisp_fun(glmer_modelOLRE_New2)
sim_res <- simulateResiduals(fittedModel = glmer_modelOLRE_New2, plot = TRUE)
summary(glmer_modelOLRE_New2)



###New
# Create a new data frame with all combinations of Elevation_group and Stand
newdata <- expand.grid(
  Elevation_group = c("Low", "High"),
  Stand = c("Low", "Mid", "High")
)
# Get predicted probabilities and confidence intervals from the model
emm <- emmeans(glmer_modelOLRE_New2, ~ Elevation_group * Stand, type = "response")
colnames(pred_df)

# Convert emmeans object to data frame
pred_df <- as.data.frame(emm)
# Optional: clean up names for plotting
pred_df <- pred_df %>%
  rename(Elevation = Elevation_group, Stand = Stand, Predicted_Prob = prob) 
# Plot predicted probabilities with 95% confidence intervals
ggplot(pred_df, aes(x = Stand, y = Predicted_Prob, color = Elevation, group = Elevation)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_line(position = position_dodge(width = 0.3), size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, position = position_dodge(width = 0.3)) +
  scale_color_manual(values = c("Low" = "black", "High" = "grey")) +
  labs(
    title = "Predicted Seed Removal Probability by Elevation Group and Stand",
    y = "Predicted Probability of Seed Removal",
    x = "Forest Stand Elevation",
    color = "Seed Elevation Group"
  ) +
  theme_minimal() +
  theme(text = element_text(size = 14))


####End new


# # Run post-hoc comparisons
# emmeans_result <- emmeans(glmer_modelOLRE_New3, pairwise ~ Stand| Elevation_group)
# 
# # View the results
# summary(emmeans_result)


#**Model 3**: Are species more removed when they are around conspecific?
data_subset <- data_cleaned_2 %>% 
  filter(!(Treatment =="High"))
data_subset <- data_cleaned_2 %>% 
  filter(!(Seed_sp %in% c("A.lasiocarpa", "C.nootkatensis","A.amabilis")))

ggplot(data_subset, aes(x = Treatment, y = seeds, color = Stand, fill = Stand)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.6),
              alpha = 0.5, shape = 20, size = 2) +  # Adds jittered points
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(width = 0.6),
               shape = 23, size = 5, color = "black") +  # Adds mean points
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               position = position_dodge(width = 0.6), width = 0.2) +  # Adds error bars
  facet_wrap(~ Seed_sp, scales = "free_y") +  # Facets by species
  scale_fill_manual(values = c("Low" = "#E69F00", "Mid" = "#009E73", "High" = "#56B4E9")) +
  scale_color_manual(values = c("Low" = "#E69F00", "Mid" = "#009E73", "High" = "#56B4E9")) +
  labs(
    title = "Standardized Seed Consumption by Treatment, Stand, and Species",
    x = "Treatment",
    y = "Standardized Seed Consumption"
  ) +
  theme_minimal(base_size = 14) + 
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
    strip.text = element_text(size = 12)  # Adjusts the facet label size
  )


glmer_modelOLRE_New3 <- glmer(
  cbind(Success, Seeds.Placed - Success) ~ Treatment * Seed_sp +
    (1 | Camera) + (1 | ObsID) + (1 | Week),
  family = binomial,
  data = data_subset,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
overdisp_fun(glmer_modelOLRE_New3)
sim_res <- simulateResiduals(fittedModel = glmer_modelOLRE_New3, plot = TRUE)
summary(glmer_modelOLRE_New3)



####NEW
# emmeans for interaction between Treatment and Seed_sp
emm <- emmeans(glmer_modelOLRE_New3, ~ Treatment * Seed_sp, type = "response")
# Convert to dataframe for plotting
pred_df <- as.data.frame(emm)
ggplot(pred_df, aes(x = Treatment, y = prob, fill = Seed_sp)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black") +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.8), width = 0.2) +
  scale_fill_viridis(discrete = TRUE, option = "B") +  # discrete palette, option D is nice
  labs(
    title = "Predicted Seed Removal Probability by Treatment and Seed Species",
    x = "Treatment",
    y = "Predicted Probability of Seed Removal",
    fill = "Seed Species"
  ) +
  theme_minimal() +
  theme(text = element_text(size = 14))


#####End new

# Run post-hoc comparisons
emmeans_result_low <- emmeans(glmer_modelOLRE_New2, pairwise ~ Treatment|Seed_sp)
# View the results
summary(emmeans_result_low)
# 
# # Create a data frame from the emmeans results
# emmeans_df <- data.frame(
#   Species = rep(c("PSME", "THPL", "TSHE"), each = 2),
#   Treatment = rep(c("All", "Low"), times = 3),
#   emmean = c(-3.78, -4.32, -1.14, -1.08, -2.41, -3.43),
#   SE = c(0.733, 0.745, 0.660, 0.665, 0.675, 0.685)
# )
# 
# # Add lower and upper confidence intervals
# emmeans_df <- emmeans_df %>%
#   mutate(
#     lower = emmean - 1.96 * SE,
#     upper = emmean + 1.96 * SE
#   )
# 
# # Plot
# ggplot(emmeans_df, aes(x = Species, y = emmean, fill = Treatment)) +
#   geom_col(position = position_dodge(0.8), width = 0.6, color = "black") +
#   geom_errorbar(aes(ymin = lower, ymax = upper),
#                 position = position_dodge(0.8), width = 0.2) +
#   labs(title = "Estimated Seed Removal by Species and Treatment",
#        y = "Logit (Seed Removal Probability)",
#        x = "Species") +
#   theme_minimal() +
#   scale_fill_brewer(palette = "Set2")

unique(data_subset_2$Camera)

#**Model 4**: Are species more removed when they are around conspecific?
data_subset_2 <- data_cleaned_2 %>% 
  filter(!(Treatment =="Low"))
data_subset_2 <- data_cleaned_2 %>% 
  filter(!(Seed_sp %in% c("P.menziesii", "T.plicata", "T.heterophylla")))
unique(data_subset_2$Seed_sp)
glmer_modelOLRE_New4 <- glmer(
  cbind(Success, Seeds.Placed - Success) ~ Treatment * Seed_sp+
    (1 | Camera) + (1 | ObsID) + (1|Week) ,
  family = binomial,
  data = data_subset_2,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7))
)
overdisp_fun(glmer_modelOLRE_New4)

sim_res <- simulateResiduals(fittedModel = glmer_modelOLRE_New4, plot = TRUE)
summary(glmer_modelOLRE_New4)

##New
# emmeans for interaction between Treatment and Seed_sp
emm <- emmeans(glmer_modelOLRE_New4, ~ Treatment * Seed_sp, type = "response")
# Convert to dataframe for plotting
pred_df <- as.data.frame(emm)
ggplot(pred_df, aes(x = Treatment, y = prob, fill = Seed_sp)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black") +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.8), width = 0.2) +
  scale_fill_viridis(discrete = TRUE, option = "D") +  # discrete palette, option D is nice
  labs(
    title = "Predicted Seed Removal Probability by Treatment and Seed Species",
    x = "Treatment",
    y = "Predicted Probability of Seed Removal",
    fill = "Seed Species"
  ) +
  theme_minimal() +
  theme(text = element_text(size = 14))

###End of new


# Saving dataset ----------------------------------------------------------------

# Save the cleaned data as an RData file
# save(data_cleaned_2, file = "C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/data_cleaned_2.RData")