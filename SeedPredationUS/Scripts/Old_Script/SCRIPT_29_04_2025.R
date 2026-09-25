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
library(glmmTMB)
library(sjPlot)
library(ggeffects)


# Loading the data --------------------------------------------------------
# Set the working directory
getwd()
setwd("C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets")

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
#Checking for overdispersion
overdisp_fun(glmer_modelOLRE_New)
#Simulation the residuals to check for distribution of residuals
sim_res <- simulateResiduals(fittedModel = glmer_modelOLRE_New, plot = TRUE)
#Summary of the model
summary(glmer_modelOLRE_New)

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

# To remove maybe?
# # Run post-hoc comparisons
# emmeans_result_model <- emmeans(glmer_modelOLRE_New, pairwise ~ Stand| Seed_sp)
# summary(emmeans_result_model)
# 
# 
# # Create a data frame based on the pairwise results
# data <- data.frame(
#   Species = rep(c('Abies amabilis', 'Abies lasiocarpa', 'Callitropsis nootkatensis', 
#                   'Pseudotsuga menziesii', 'Thuja plicata', 'Tsuga heterophylla'), each = 3),
#   Comparison = rep(c('High Elevation (AE10) vs Mid Elevation (AV06)', 
#                      'High Elevation (AE10) vs Low Elevation (TO04)', 
#                      'Mid Elevation (AV06) vs Low Elevation (TO04)'), 6),
#   Estimate = c(-1.765, -1.896, -0.131, -1.765, -1.896, -0.131, 
#                -1.765, -1.896, -0.131, -1.765, -1.896, -0.131,
#                -1.765, -1.896, -0.131, -1.765, -1.896, -0.131),
#   SE = rep(0.643, 18),
#   p_value = c(0.0168, 0.0089, 0.9749, 0.0168, 0.0089, 0.9749, 
#               0.0168, 0.0089, 0.9749, 0.0168, 0.0089, 0.9749, 
#               0.0168, 0.0089, 0.9749, 0.0168, 0.0089, 0.9749)
# )
# 
# # Plotting the results with patterns
# ggplot(data, aes(x = Species, y = Estimate, fill = Comparison, pattern = Comparison)) +
#   geom_bar_pattern(stat = 'identity', position = position_dodge(width = 0.8), 
#                    width = 0.6, pattern_density = 0.1, pattern_spacing = 0.02, 
#                    pattern_angle = 45) +
#   geom_errorbar(aes(ymin = Estimate - SE, ymax = Estimate + SE), 
#                 position = position_dodge(width = 0.8), width = 0.25) +
#   labs(title = 'Pairwise Comparisons of Seed Removal Odds by Stand and Species',
#        x = 'Species', y = 'Estimate (log odds)', fill = 'Comparison') +
#   scale_x_discrete(labels = c('Abies amabilis', 'Abies lasiocarpa', 
#                               'Callitropsis nootkatensis', 'Pseudotsuga menziesii',
#                               'Thuja plicata', 'Tsuga heterophylla')) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
#   scale_pattern_manual(values = c('High Elevation (AE10) vs Mid Elevation (AV06)' = 'stripe',
#                                   'High Elevation (AE10) vs Low Elevation (TO04)' = 'circle', 
#                                   'Mid Elevation (AV06) vs Low Elevation (TO04)' = 'crosshatch')) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   scale_fill_brewer(palette = "Na")




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




# # Run post-hoc comparisons
# emmeans_result_high <- emmeans(glmer_modelOLRE_New3, pairwise ~ Treatment|Seed_sp)
# # View the results
# summary(emmeans_result_high)
# 
# # Create a data frame from the provided results
# emmeans_results <- data.frame(
#   Species = rep(c('Abies amabilis', 'Abies lasiocarpa', 'Callitropsis nootkatensis'), each = 2),
#   Treatment = rep(c('All', 'High'), 3),
#   emmean = c(-0.9975, -0.8685, -0.0843, 0.1327, -3.3828, -3.9693),
#   SE = c(0.716, 0.712, 0.667, 0.670, 0.732, 0.740),
#   LCL = c(-2.40, -2.26, -1.39, -1.18, -4.82, -5.42),
#   UCL = c(0.405, 0.527, 1.222, 1.447, -1.948, -2.518),
#   p_value = c(0.8537, 0.8537, 0.7206, 0.7206, 0.4174, 0.4174)
# )
# 
# # Plot the results
# ggplot(emmeans_results, aes(x = Species, y = emmean, fill = Treatment, group = Treatment)) +
#   geom_bar(stat = 'identity', position = position_dodge(width = 0.8), width = 0.6) +
#   geom_errorbar(aes(ymin = LCL, ymax = UCL), position = position_dodge(width = 0.8), width = 0.25) +
#   labs(title = 'Estimated Marginal Means for Treatments by Species',
#        x = 'Species', y = 'Estimate (log odds)', fill = 'Treatment') +
#   scale_x_discrete(labels = c('Abies amabilis', 'Abies lasiocarpa', 'Callitropsis nootkatensis')) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   scale_fill_manual(values = c('All' = 'skyblue', 'High' = 'orange'))

# Other model possibilites (Mikko's suggestions) ------------------------------------------------

#For the sake of it I will always keep the name of the best model from before and keep also the same name for the new model (for easyness)
##1) Random effects differences

#Removing Week as random effect (always based on my best model from before: glmer_modelOLRE_3 )
glmer_modelOLRE_4 <- glmer(
  cbind(Success, Seeds.Placed - Success) ~ Stand + Seed_sp +
    (1 | Camera) + (1 | ObsID),
  family = binomial,
  data = data_cleaned_2
)

anova (glmer_modelOLRE_3,glmer_modelOLRE_4)
#Doesn't improve the fit and also I don't think it makes sense. 

#Week as ordinal factor
data_cleaned_2$WeekOrdinal <- factor(data_cleaned_2$Week, 
                              levels = c("First", "Second", "Third", "Fourth", "Last"),
                              ordered = TRUE)
glmer_modelOLRE_4 <- glmer(
  cbind(Success, Seeds.Placed - Success) ~ Stand + Seed_sp +
    (1 | Camera) + (1|WeekOrdinal)+ (1 | ObsID),
  family = binomial,
  data = data_cleaned_2
)
anova (glmer_modelOLRE_3,glmer_modelOLRE_4)
summary(glmer_modelOLRE_4)
#Doesn't change anything but maybe I did it wrong

#Treatment as a random effect
glmer_modelOLRE_4 <- glmer(
  cbind(Success, Seeds.Placed - Success) ~ Stand + Seed_sp +
    (1 | Camera) + (1|Week)+ (1 | ObsID) + (1|Treatment),
  family = binomial,
  data = data_cleaned_2
)
anova (glmer_modelOLRE_3,glmer_modelOLRE_4)
#Doesn't improve either and not sure it make sense either

#Species as a random effect
glmer_modelOLRE_4 <- glmer(
  cbind(Success, Seeds.Placed - Success) ~ Stand + Seed_sp +
    (1 | Camera) + (1|Week)+ (1 | ObsID) + (1|Seed_sp),
  family = binomial,
  data = data_cleaned_2
)
anova (glmer_modelOLRE_3,glmer_modelOLRE_4)
#Not better 
#Removing species as explanatory variable
glmer_modelOLRE_4 <- glmer(
  cbind(Success, Seeds.Placed - Success) ~ Stand  +
    (1 | Camera) + (1|Week)+ (1 | ObsID),
  family = binomial,
  data = data_cleaned_2
)
anova (glmer_modelOLRE_3,glmer_modelOLRE_4)
#Still did not improve. Best model still Stand + Species

#Testing nested random effects
glmer_modelOLRE_4 <- glmer(
  cbind(Success, Seeds.Placed - Success) ~ Stand + Seed_sp+
    (1 | Camera/Week/ObsID),
  family = binomial,
  data = data_cleaned_2
)
anova (glmer_modelOLRE_3,glmer_modelOLRE_4)
summary(glmer_modelOLRE_4)
#Improves slightly but model fails to converge so not sure if this is relevant or not

##b) Testing new explantory variables

#Week as an explanatory variable (testing if animal learns from previous week maybe)
glmer_modelOLRE_4 <- glmer(
  cbind(Success, Seeds.Placed - Success) ~ Stand + Seed_sp + Week +
    (1 | Camera) + (1 | ObsID),
  family = binomial,
  data = data_cleaned_2
)
anova (glmer_modelOLRE_3,glmer_modelOLRE_4)
summary(glmer_modelOLRE_4)
#So the fit is slightly better but the model fails to converge. I think that it just shows that Week 2 is indeed different than the others week (see graphs in plot section) but not sure it is relevant for my scope. Because it is not really what I want to test for in my opinion but we could dig into that maybe :)


## c) testing for the full model (model that is the most biologically relevant where Stand interacts with Seed species and with Treatment)
# Mikko also suggested I do this with the nested random variable but I think it becomes too difficult of a model. 
glmer_modelOLRE_4 <- glmer(
  cbind(Success, Seeds.Placed - Success) ~ Stand * Seed_sp * Treatment +
    (1 | Camera) + (1 | Week) + (1 | ObsID),
  family = binomial,
  data = data_cleaned_2
)
anova (glmer_modelOLRE_3,glmer_modelOLRE_4)
#Doesn't improves the fit at all. 

#Still model Stand + Seed Sp + (Week, Obs ID and Camera number as random variable) the best model

#There would be the idea also of testing a zero infladed model but might not be necessary for the moment and I tried and did not manage to have good fit or had convergence issues



# Other models - Elo Ideas (DBH and only high/low treatment) ------------------------------------------------

## a)  Checking with dbh. 
seed_predation <- load("Datasets/seed_predation_with_dbh.Rdata")
#Adding the data from the neighbourghing trees
str(data_cleaned_2)
data_cleaned_2dbh <- data_cleaned_2 %>%
  left_join(seed_predation_with_dbh %>% select(Camera.number, mean_dbh), 
            by = c("Camera" = "Camera.number"))
#By dbh
ggplot(data_cleaned_2dbh, aes(x = mean_dbh, y = seeds)) +
  geom_point(color = "black", alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = TRUE) +
  labs(
    x = "Mean dbh",
    y = "Standardized Seed Removal",
    title = "Relationship Between Seed Weight and Removal Rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

#Doesn't seem like dbh has an effect on the seed removal

#Model
glmer_modelOLRE_4 <- glmer(
  cbind(Success, Seeds.Placed - Success) ~ Stand + Seed_sp + mean_dbh +
    (1 | Camera) + (1 | ObsID) + (1|Week),
  family = binomial,
  data = data_cleaned_2dbh
)

anova (glmer_modelOLRE_3,glmer_modelOLRE_4)

#Doesn't improve the fit

#What about mean dbh as a random factor? instead of camera
glmer_modelOLRE_4 <- glmer(
  cbind(Success, Seeds.Placed - Success) ~ Stand + Seed_sp +
    (1 | mean_dbh) + (1 | ObsID) + (1|Week),
  family = binomial,
  data = data_cleaned_2dbh
)

anova (glmer_modelOLRE_3,glmer_modelOLRE_4)

#Doesn't work. 
#Also I checked and the seed trays were sometimes placed at the edge of the plots so could be biasing using dbh in general.
#Should I do the dbh per species?
# Plot: Removal rate vs DBH by Species
ggplot(data_cleaned_2dbh, aes(x = mean_dbh, y = seeds)) +
  geom_point(aes(color = Seed_sp), alpha = 0.7) +  # Scatter plot of points
  geom_smooth(method = "lm", aes(color = Seed_sp), se = FALSE) +  # Smoothed line per species
  facet_wrap(~ Seed_sp, scales = "free") +  # Facet by species
  labs(
    title = "Removal Rate by DBH per Species",
    x = "DBH (cm)",
    y = "Removal Rate (%)",
    color = "Species"
  ) +
  theme_minimal() +
  theme(legend.position = "top")
#I can change the geom_smooth method to "loess" if I expect a non-linear relationship
#In general we see an increase of removal with bigger dbh which should be expected if bigger trees produce more seeds so seed predator might hang around more. 
# Plot: Removal rate vs DBH by Stand
ggplot(data_cleaned_2dbh, aes(x = mean_dbh, y = seeds)) +
  geom_point(aes(color = Stand), alpha = 0.7) +  # Scatter plot of points
  geom_smooth(method = "lm", aes(color = Stand), se = FALSE) +  # Smoothed line per species
  facet_wrap(~ Stand, scales = "free") +  # Facet by species
  labs(
    title = "Removal Rate by DBH per Stand ",
    x = "DBH (cm)",
    y = "Removal Rate (%)",
    color = "Stand"
  ) +
  theme_minimal() +
  theme(legend.position = "top")
#Interesting because we see a different pattern for the stand where there is an decrease in removal based on mean dbh (except for too04)
#I don't know what to interpret from those results. 


##b) now could be interesting to see if the patterns hold when we have only high or low treatment. 
data_cleaned_3<-data_cleaned_2%>%
  filter(Treatment%in% c("High","Low"))
glmer_modelOLRE_3b <- glmer(
  cbind(Success, Seeds.Placed - Success) ~ Stand + Seed_sp + Treatment +
    (1 | Camera) + (1 | ObsID) + (1|Week),
  family = binomial,
  data = data_cleaned_3
)

summary(glmer_modelOLRE_3b)
#I'm not sure if this works
#And also if it makes sense...

# Seed predator density analysis ------------------------------------------

# Filter to rows where an animal was present and the Species_ID is not missing or "NA" and when the animal is on the tray
predator_data_clean <- predators_data %>%
  filter(Animal_presence == "Yes", !is.na(Species_ID), Species_ID != "NA")%>%
  filter(On_tray=="On")

#Checking the dataset
#checking for the weeks
predator_data_clean_week<- predator_data_clean %>%
  group_by(Week, Stand)%>%
  summarise (week= n_distinct(Week))
#AE10 only has 3 weeks... so I will make sure it is standartized
sampling_effort <- tibble(
  Stand = c("AE10", "AV06", "TO04"),
  Weeks_sampled = c(3, 4, 4)
)


#Preparing data for model 
#Predator detection per Stand 
predator_density_by_site <- predator_data_clean %>%
  group_by(Stand) %>%
  summarise(
    predator_detections = n(),                # total number of predator detections
    unique_species = n_distinct(Species_ID)   # number of species
  )
# Standartized with Weeks:
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


#Changing names for clarity and plots 
# For Stand
predator_density_site_species_standardized$Stand <- factor(
  predator_density_site_species_standardized$Stand, 
  levels = c("TO04", "AV06", "AE10"), 
  labels = c("Low", "Mid", "High")
)
# For Species_ID
predator_density_site_species_standardized$Species_ID <- factor(
  predator_density_site_species_standardized$Species_ID,
  levels = c("Peromyscus maniculatus", "Tamias sp", "Lepus americanus", "Bird", "Flying squirrel", "Shrew?", "vole?", "Zapus?", "???"),
  labels = c("Peromyscus maniculatus", "Tamias sp.", "Lepus americanus", "Birds", "Glaucomys oregonensis", "Shrew sp.", "Vole sp.", "Zapus sp.", "Not identified")
)


#Plots : 
#Color for species
species_colors <- c(
  "Peromyscus maniculatus" = "#332288",  # dark blue
  "Tamias sp." = "#88CCEE",               # sky blue
  "Lepus americanus" = "#117733",         # green
  "Birds" = "#DDCC77",                    # mustard yellow
  "Glaucomys oregonensis" = "#CC6677",   # rosy pink
  "Shrew sp." = "#AA4499",                # purple
  "Vole sp." = "#44AA99",                 # teal
  "Zapus sp." = "#999933",                # olive
  "Not identified" = "#DDDDDD"            # light gray
)

#Normal
ggplot(predator_density_site_species, aes(x = Stand, y = predator_detections, fill = Species_ID)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Predator Detections per Site and Species",
       x = "Site (Stand)",
       y = "Number of Detections",
       fill = "Species") +
  theme_minimal()
#Standartized
ggplot(predator_density_site_species_standardized, aes(x = Stand, y = detections_per_week, fill = Species_ID)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = species_colors) +
  labs(
    title = "Predator Detections per Site and Species",
    x = "Stand",
    y = "Number of Detections",
    fill = "Species"
  ) +
  theme_minimal()
#Hudge difference!

#Model (dataset = predator_density_site_species_standartized)
#Trying a model to know if some species get detected more often than others and are some sites associated with more predator detections?
#Doesn't take into account weeks
model_pois <- glm(detections_per_week ~ Stand + Species_ID,
                  data = predator_density_site_species_standardized,
                  family = poisson())
#Doesn't work
#Takes into account the fact that there is not the same amount of week per site. 
model_pois_Week<-glm(predator_detections ~ Stand + Species_ID,
    data = predator_density_site_species_standardized,
    family = poisson(),
    offset = log(Weeks_sampled)) #Adding the offset so I take account of the fact not all stands have the same weeks
summary(model_pois_Week)

#Coefficient represent the changes in detection rate per week (expected number of detections per week)
predator_density_site_species_standardized <- predator_density_site_species_standardized %>%
  mutate(
    predicted_log_rate = predict(model_pois_Week, type = "link"),          # log(rate)
    predicted_rate = exp(predicted_log_rate)                                 # rate = detections per week
  )

# Calculate residual deviance / degrees of freedom to check if my model is overdispersed.
deviance(model_pois_Week) / df.residual(model_pois_Week)
#It is.. so I will try a negative binomial model 

model_nb <- glm.nb(predator_detections ~ Stand + Species_ID + offset(log(Weeks_sampled)),
                   data = predator_density_site_species_standardized,
                    control = glm.control(maxit = 50))
summary(model_nb)
#Checking for the residuals
sim_res_nb <- simulateResiduals(model_nb)
plot(sim_res_nb)
#Looks not super good... but I guess it is ok? 


glmmTMB(predator_detections ~ Stand + Species_ID + offset(log(Weeks_sampled)),
        ziformula = ~1,  # zero-inflation intercept
        family = nbinom2(),
        data = predator_density_site_species_standardized)

#Plotting with the error bars
preds_nb <- predict(model_nb, type = "link", se.fit = TRUE)
predictions_nb <- predator_density_site_species_standardized %>%
  mutate(
    predicted_log_rate = preds_nb$fit,
    se_log_rate = preds_nb$se.fit,
    lower_log = predicted_log_rate - 1.96 * se_log_rate,
    upper_log = predicted_log_rate + 1.96 * se_log_rate,
    predicted_rate = exp(predicted_log_rate),
    lower_rate = exp(lower_log),
    upper_rate = exp(upper_log)
  )
ggplot(predictions_nb, aes(x = Species_ID, y = predicted_rate, fill = Stand)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = lower_rate, ymax = upper_rate),
                position = position_dodge(width = 0.9),
                width = 0.2) +
  labs(
    title = "Predicted Predator Detection Rates per Week (Negative Binomial Model)",
    x = "Species",
    y = "Detections per Week"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Plot with dots instead of bars
ggplot(predictions_nb, aes(x = Species_ID, y = predicted_rate, color = Stand)) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(aes(ymin = lower_rate, ymax = upper_rate),
                position = position_dodge(width = 0.6),
                width = 0.2) +
  labs(
    title = "Predicted Predator Detection Rates per Week",
    x = "Species",
    y = "Detections per Week"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Plot on a log scale because some individuals have almost 0 detections. 
ggplot(predictions_nb, aes(x = Species_ID, y = predicted_rate, color = Stand)) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(aes(ymin = lower_rate, ymax = upper_rate),
                position = position_dodge(width = 0.6),
                width = 0.2) +
  scale_y_log10() +
  scale_color_manual(values = c(
    "Low" = "#E69F00",  # orange
    "High" = "#56B4E9",  # sky blue
    "Mid" = "#009E73"   # green
  )) +
  labs(
    title = "Predicted Predator Detection Rates per Week (log scale)",
    x = "Species",
    y = "Detections per Week (log scale)",
    color = "Stand (Site)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


unique(predator_density_site_species_standardized$Species_ID)




# PERO AND TAMIA ----------------------------------------------------------

#Combining the dataset with the seed data to see if I have correlations between seed removal and predator activity
predator_summary <- predator_data_clean %>%
  group_by(Stand, Week, Camera, Species_ID) %>%
  summarise(detections = n(), .groups = "drop") %>%
  group_by(Stand, Week, Camera) %>%
  summarise(
    total_detections = sum(detections),
    predator_species = list(unique(Species_ID)),
    .groups = "drop"
  )
seed_summary <- data_cleaned_2 %>%
  group_by(Stand, Week, Camera, Treatment) %>%
  summarise(
    seed_removal = mean(seeds, na.rm = TRUE), 
    .groups = "drop"
  )
combined_data <- left_join(seed_summary, predator_summary, by = c("Stand", "Week", "Camera")) %>%
  mutate(total_detections = ifelse(is.na(total_detections), 0, total_detections),
         predator_species = ifelse(is.na(total_detections), list(NA), predator_species)
  )

#Plot removal by predator detection on the tray!
ggplot(combined_data, aes(x = total_detections, y = seed_removal)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Total Predator Detections (per camera-week)",
    y = "Seed Removal Proportion",
    title = "Seed Removal vs. Predator Detections"
  ) +
  theme_minimal()
cor.test(combined_data$total_detections, combined_data$seed_removal)
# Fit the model
model <- lm(seed_removal ~ total_detections, data = combined_data)
# Plot residuals vs fitted
plot(model$fitted.values, residuals(model))
abline(h=0, col="red")
# Q-Q plot of residuals
qqnorm(residuals(model))
qqline(residuals(model))
# Shapiro-Wilk test for normality of residuals
shapiro.test(residuals(model))
#To use later for the beta_regression as I cannot have 1 or 0's
combined_data$seed_removal_adj <- with(combined_data, ifelse(seed_removal == 0, 0.001,
                                                             ifelse(seed_removal == 1, 0.999, seed_removal)))
# Fit the model
model2 <- betareg(seed_removal_adj ~ total_detections, data = combined_data)
# Plot residuals vs fitted
plot(model2$fitted.values, residuals(model))
abline(h=0, col="red")
# Q-Q plot of residuals
qqnorm(residuals(model2))
qqline(residuals(model2))
# Shapiro-Wilk test for normality of residuals
shapiro.test(residuals(model2))
summary(model2)



# Get predictions and CIs from your beta regression model
pred_df <- ggpredict(model2, terms = "total_detections")

# Plot the predicted curve with confidence intervals
ggplot(pred_df, aes(x = x, y = predicted)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "blue") +
  geom_point(data = combined_data, aes(x = total_detections, y = seed_removal), alpha = 0.6) +
  labs(
    x = "Total Predator Detections (per camera-week)",
    y = "Seed Removal Proportion",
    title = "Seed Removal vs. Predator Detections (Beta Regression)"
  ) +
  theme_minimal()






#How much Peromyscus and Tamia influence seed removal:


model_data <- combined_data %>%
  unnest(predator_species) %>%
  mutate(present = 1) %>%
  pivot_wider(
    names_from = predator_species,
    values_from = present,
    values_fill = list(present = 0)
  )






###As interested in why there is more removal at AE10, I'm going to look at removal per depression for the species Peromyscus and Tamia to see if there is a relationship between removal and species ID.

predator_data_clean2 <- predator_data_clean %>%
  filter(On_depression == "Yes")

predator_summary_depression <- predator_data_clean2 %>%
  mutate(Depression_number = as.factor(Depression_number)) %>%
  group_by(Stand, Week, Camera,Depression_number, Species_ID) %>%
  summarise(detections = n(), .groups = "drop") %>%
  group_by(Stand, Week, Camera, Depression_number) %>%
  summarise(
    total_detections = sum(detections),
    predator_species = list(unique(Species_ID)),
    .groups = "drop"
  )
seed_summary_depression <- data_cleaned_2 %>%
  mutate(Depression_number = as.factor(Depression_number)) %>%
  group_by(Stand, Week, Camera, Treatment, Depression_number, Seed_sp, Seeds.Remaining, Seeds.Placed) %>%
  summarise(
    seed_removal = mean(seeds, na.rm = TRUE), 
    .groups = "drop"
  )


combined_data_depression <- left_join(
  seed_summary_depression,
  predator_summary_depression,
  by = c("Week","Stand", "Camera", "Depression_number")
) %>%
  mutate(
    total_detections = ifelse(is.na(total_detections), 0, total_detections),
    predator_species = ifelse(is.na(total_detections), list(NA), predator_species)
  )
combined_data_depression$seed_removal_adj <- with(combined_data_depression, ifelse(seed_removal == 0, 0.001,
                                                             ifelse(seed_removal == 1, 0.999, seed_removal)))


#I have to do this by site
#But I wonder how accuratecan this be when I did not personally analyzed all those pictures and videos. 
#AE10
combined_data_depression_ae10<-combined_data_depression%>%
  filter(Stand=="AE10")
# Step 1: Unnest predator_species list column
df_long <- combined_data_depression_ae10 %>%
  filter(!is.null(predator_species)) %>%  # Remove rows with NULL predator_species if needed
  unnest(predator_species)
# Step 2: Summarize mean seed_removal and total_detections by predator_species
summary_df <- df_long %>%
  group_by(predator_species) %>%
  summarize(
    mean_seed_removal = mean(seed_removal, na.rm = TRUE),
    mean_total_detections = mean(total_detections, na.rm = TRUE),
    mean_seed_removal_proportionate= mean_seed_removal/mean_total_detections,
    count = n()
  ) %>%
  arrange(desc(mean_seed_removal))
# Step 3: Plot mean seed removal vs predator species with total detections as size
ggplot(summary_df, aes(x = reorder(predator_species, mean_seed_removal), y = mean_seed_removal)) +
  geom_point(aes(size = mean_total_detections), color = "steelblue", alpha = 0.7) +
  geom_text(aes(label = round(mean_seed_removal, 2)), vjust = -1, size = 3) +
  scale_size_continuous(name = "Mean Total Detections") +
  labs(
    x = "Predator Species",
    y = "Mean Seed Removal",
    title = "Seed Removal by Predator Species and Total Detections"
  ) +
  theme_minimal() +
  coord_flip()  # Flip to make species labels readable

combined_data_depression_ae10

#How much Peromyscus and Tamia influence seed removal:
species_contributions_dep <- combined_data_depression %>%
  unnest(predator_species) %>%  # expand predator_species list column
  group_by(predator_species) %>%
  summarise(
    total_removal = sum(seed_removal, na.rm = TRUE),
    count = n()
  ) %>%
  arrange(desc(total_removal))

model_data_dep <- combined_data_depression_ae10 %>%
  unnest(predator_species) %>%
  mutate(present = 1) %>%
  pivot_wider(
    names_from = predator_species,
    values_from = present,
    values_fill = list(present = 0)
  )

str(model_data_dep)

model_lm2 <- lm(seed_removal ~ `Peromyscus maniculatus` + `Tamias sp`, data = model_data_dep)
summary(model_lm)
plot(model_lm)


model_beta2 <- betareg(seed_removal_adj^2 ~ `Peromyscus maniculatus` + `Tamias sp`, data = model_data_dep)
summary(model_beta2)
par(mfrow = c(2, 2))
plot(model_lm)
plot(model_beta)
shapiro.test(residuals(model_lm))
shapiro.test(residuals(model_beta))
qqnorm(residuals(model_lm)); qqline(residuals(model_lm))
hist(residuals(model_lm), breaks = 20)
qqnorm(residuals(model_beta)); qqline(residuals(model_beta))
hist(residuals(model_beta), breaks = 20)
# Not Normally distribute for any model

glm_dep<-glmer(cbind(Seeds.Remaining, Seeds.Placed - Seeds.Remaining) ~ `Peromyscus maniculatus` + `Tamias sp` + (1|Week), family=binomial, data=model_data_dep)
summary(glm_dep)
simulationOutput <- simulateResiduals(fittedModel = zi_model)
plot(simulationOutput)
testUniformity(simulationOutput)
testDispersion(simulationOutput)


#Changing names for clarity and plots 
# For Stand
model_data_dep$Stand <- factor(
  model_data_dep$Stand, 
  levels = c("TO04", "AV06", "AE10"), 
  labels = c("Low", "Mid", "High")
)
# For Species_ID
model_data_dep$Species_ID <- factor(
  model_data_dep$Species_ID,
  levels = c("Peromyscus maniculatus", "Tamias sp", "Lepus americanus", "Bird", "Flying squirrel", "Shrew?", "vole?", "Zapus?", "???"),
  labels = c("Peromyscus maniculatus", "Tamias sp.", "Lepus americanus", "Birds", "Glaucomys oregonensis", "Shrew sp.", "Vole sp.", "Zapus sp.", "Not identified")
)



zi_model <- glmmTMB(cbind(Seeds.Remaining, Seeds.Placed - Seeds.Remaining) ~ 
    `Tamias sp` + `Peromyscus maniculatus` + Seed_sp + 
    (1 | Camera) + (1 | Week),
  ziformula = ~ `Tamias sp` + `Peromyscus maniculatus`, # zero-inflation depends on predator presence
  family = binomial,
  data = model_data_dep
)
summary(zi_model)

# Get most common factor levels to hold constant
mode_seed <- model_data_dep %>%
  count(Seed_sp) %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  pull(Seed_sp)
mode_camera <- model_data_dep %>%
  count(Camera) %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  pull(Camera)
mode_week <- model_data_dep %>%
  count(Week) %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  pull(Week)
# Create prediction grid
newdata <- expand.grid(
  `Peromyscus maniculatus` = seq(min(model_data_dep$`Peromyscus maniculatus`), max(model_data_dep$`Peromyscus maniculatus`), length.out = 100),
  `Tamias sp` = c(0, 1),
  Seed_sp = mode_seed,
  Camera = mode_camera,
  Week = mode_week
)
# Predict from model
newdata$fit <- predict(zi_model, newdata = newdata, type = "response")
ggplot(newdata, aes(x = `Peromyscus maniculatus`, y = fit, color = factor(`Tamias sp`))) +
  geom_line(size = 1.2) +
  scale_color_manual(
    values = c("0" = "#999999", "1" = "#88CCEE"),
    labels = c("Absent", "Present")
  ) +
  labs(
    x = "Peromyscus maniculatus (detections per week)",
    y = "Predicted Seed Removal",
    color = "Tamias sp.",
    title = paste("Predicted Seed Removal for", mode_seed)
  ) +
  theme_minimal()

# 1) Which predator was most present?  
# Sum total detections or presence counts across rows for each predator column
predator_presence <- model_data_dep %>%
  summarise(
    Tamias_sp = sum(`Tamias sp`),
    Peromyscus_maniculatus = sum(`Peromyscus maniculatus`),
    Unknown_predator = sum(!!!rlang::syms(names(model_data_dep)[14])) # if last column is another predator
  ) %>%
  pivot_longer(everything(), names_to = "Predator", values_to = "Total_Presence") %>%
  arrange(desc(Total_Presence))

print(predator_presence)

ggplot(predator_presence, aes(x = Predator, y = Total_Presence, fill = Predator)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Total Presence of Seed Predators",
       y = "Sum of Detections",
       x = "Predator Species") +
  theme_minimal()

# 2) Which predator contributed the most to seed removal?
# Weight removal by predator presence per row, then sum by predator

predator_removal <- model_data_dep %>%
  mutate(
    Tamias_removal = `Tamias sp` * seed_removal,
    Peromyscus_removal = `Peromyscus maniculatus` * seed_removal,
    Unknown_removal = `???` * seed_removal  # adjust column name accordingly
  ) %>%
  summarise(
    Tamias_total = sum(Tamias_removal),
    Peromyscus_total = sum(Peromyscus_removal),
    Unknown_total = sum(Unknown_removal)
  ) %>%
  pivot_longer(everything(), names_to = "Predator", values_to = "Total_Removal") %>%
  arrange(desc(Total_Removal))

print(predator_removal)

ggplot(predator_removal, aes(x = Predator, y = Total_Removal, fill = Predator)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Total Seed Removal Attributed to Predators",
       y = "Sum of Removal Index",
       x = "Predator Species") +
  theme_minimal()

# 3) Which seeds were preferably removed by each predator?
# Calculate sum of removal per predator per seed species

seed_predator_preference <- model_data_dep %>%
  mutate(
    Tamias_removal = `Tamias sp` * seed_removal,
    Peromyscus_removal = `Peromyscus maniculatus` * seed_removal,
    Unknown_removal = `???` * seed_removal  # adjust column name accordingly
  ) %>%
  group_by(Seed_sp) %>%
  summarise(
    Tamias = sum(Tamias_removal),
    Peromyscus = sum(Peromyscus_removal),
    Unknown = sum(Unknown_removal)
  ) %>%
  pivot_longer(cols = c(Tamias, Peromyscus, Unknown),
               names_to = "Predator",
               values_to = "Removal") %>%
  arrange(Predator, desc(Removal))

print(seed_predator_preference)

ggplot(seed_predator_preference, aes(x = Seed_sp, y = Removal, fill = Predator)) +
  geom_col(position = "dodge") +
  labs(title = "Seed Removal by Predator Species",
       x = "Seed Species",
       y = "Sum of Removal Index") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Table for paper ---------------------------------------------------------

library(car)

# Fit your model (example)
# model <- glmer(...)

# Run Type II ANOVA on the model
Anova(glmer_modelOLRE_New, type = 2)
Anova(glmer_modelOLRE_New2, type = 2)
Anova(glmer_modelOLRE_New3, type = 2)
Anova(glmer_modelOLRE_New4, type = 2)
Anova(model_nb, type = 2)
Anova(model2, type = 2)
Anova(model_beta, type = 2)
summary(model_beta)
summary(model_nb)
# Saving dataset ----------------------------------------------------------------

# Save the cleaned data as an RData file
# save(data_cleaned_2, file = "C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/data_cleaned_2.RData")