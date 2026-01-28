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

# SEED PREDATION: Process data ---------------------------------------------------
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

# PLOTS(seed removal per species, stand, treatment and elevation group)-------------------------------------------------------------------
# Plot standardized based on the amount of seeds at the beginning (see Seed predation results)
ggplot(data_cleaned_2, aes(x = Seeds.Placed, y = seeds)) +
  geom_point(alpha = 0.6, shape = 21, color = "black", fill = "skyblue", size = 3) +  # Nicer points
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed", size = 1) +  # Linear regression line
  labs(
    title = "Seeds Placed vs Standardized Seed Consumption",
    x = "Seeds Placed",
    y = "Standardized Removal",
    caption = "Each point represents a seed station"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )



# Plot for species
mean_se_data <- data_cleaned_2 %>%
  group_by(Seed_sp) %>%
  summarise(
    mean_seeds = mean(seeds, na.rm = TRUE),
    se_seeds = sd(seeds, na.rm = TRUE) / sqrt(n())
  )
print(mean_se_data)
# Set factor levels in desired order
data_cleaned_2$Seed_sp <- factor(data_cleaned_2$Seed_sp,
                                 levels = c("THPL", "TSHE", "PSME", "ABAM", "CANO", "ABLA"))

# Also update in the summary data if using it
mean_se_data$Seed_sp <- factor(mean_se_data$Seed_sp,
                               levels = c("THPL", "TSHE", "PSME", "ABAM", "CANO", "ABLA"))
# Latin name mapping
latin_names <- c(
  "THPL" = "Thuja plicata",
  "TSHE" = "Tsuga heterophylla",
  "PSME" = "Pseudotsuga menziesii",
  "ABLA" = "Abies lasiocarpa",
  "CANO" = "Callitropsis nootkatensis",
  "ABAM" = "Abies amabilis"
)
# Plot
ggplot(data_cleaned_2, aes(x = Seed_sp, y = seeds, fill = Seed_sp)) +
  geom_jitter(width = 0.3, alpha = 0.4, shape = 21, color = "black") +
  geom_pointrange(data = mean_se_data,
                  aes(x = Seed_sp, y = mean_seeds,
                      ymin = mean_seeds - se_seeds,
                      ymax = mean_seeds + se_seeds),
                  color = "black", size = 0.8) +
  scale_fill_manual(values = c(
    "THPL" = "darkred", "TSHE" = "indianred", "PSME" = "lightcoral", 
    "ABLA" = "blue", "CANO" = "lightblue", "ABAM" = "darkblue")) +
  scale_x_discrete(labels = latin_names) +
  labs(title = "Standardized Seed Consumption by Species",
       x = "Species", y = "Standardized Removal") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1))



# Plot for stand
# Reorder Stand and assign readable labels
data_cleaned_2$Stand <- factor(data_cleaned_2$Stand,
                               levels = c("TO04", "AV06", "AE10"),
                               labels = c("Low", "Mid", "High"))
#Taking into account that there is not the same amount of weeks per stand
mean_se_data_stand <- data_cleaned_2 %>%
  group_by(Stand, Week) %>%
  summarise(mean_seeds_week = mean(seeds, na.rm = TRUE), .groups = "drop") %>%
  group_by(Stand) %>%
  summarise(
    mean_seeds = mean(mean_seeds_week, na.rm = TRUE),
    se_seeds = sd(mean_seeds_week, na.rm = TRUE) / sqrt(n())
  )
ggplot(data_cleaned_2, aes(x = Stand, y = seeds, fill = Stand)) +
  geom_jitter(width = 0.2, alpha = 0.4, shape = 21, color = "black") +
  geom_pointrange(data = mean_se_data_stand,
                  aes(x = Stand, y = mean_seeds,
                      ymin = mean_seeds - se_seeds,
                      ymax = mean_seeds + se_seeds),
                  color = "black", size = 0.8) +
  scale_fill_manual(values = c("Low" = "darkred", "Mid" = "orange", "High" = "lightblue")) +
  labs(title = "Standardized Seed Consumption by Stand",
       x = "Elevation", y = "Standardized Removal") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "none")


#Plot for treatment
# Reorder Treatment
data_cleaned_2$Treatment <- factor(data_cleaned_2$Treatment,
                                   levels = c("Low", "High", "All"))
mean_se_data_treatment <- data_cleaned_2 %>%
  group_by(Treatment) %>%
  summarise(
    mean_seeds = mean(seeds, na.rm = TRUE),
    se_seeds = sd(seeds, na.rm = TRUE) / sqrt(n())
  )
ggplot(data_cleaned_2, aes(x = Treatment, y = seeds, fill = Treatment)) +
  geom_jitter(width = 0.2, alpha = 0.4, shape = 21, color = "black") +
  geom_pointrange(data = mean_se_data_treatment,
                  aes(x = Treatment, y = mean_seeds,
                      ymin = mean_seeds - se_seeds,
                      ymax = mean_seeds + se_seeds),
                  color = "black", size = 0.8) +
  scale_fill_manual(values = c("Low" = "darkred", "High" = "darkblue", "All" = "darkgrey")) +
  labs(title = "Standardized Seed Consumption by Treatment",
       x = "Treatment", y = "Standardized Removal") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "none")

# Plot the average standardized success by Week
data_cleaned_2$Week <- factor(data_cleaned_2$Week,
                              levels = c("First", "Second", "Third", "Fourth"))
ggplot(data_cleaned_2, aes(x = Week, y = seeds, fill = Week)) +
  geom_jitter(width = 0.15, alpha = 0.4, shape = 21, color = "black", size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, fill = "black") +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Standardized Seed Consumption by Week",
    x = "Week",
    y = "Standardized Success"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

# Plot the average standardized success by Species and Stand
ggplot(data_cleaned_2, aes(x = Seed_sp, y = seeds, color = Stand, fill = Stand)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.6), 
              alpha = 0.5, shape = 20, size = 2) +
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(width = 0.6),
               shape = 23, size = 5, color = "black") +
    scale_fill_manual(values = c("Low" = "darkred", "Mid" = "salmon", "High" = "darkblue")) +
  scale_color_manual(values = c("Low" = "darkred", "Mid" = "salmon", "High" = "darkblue")) +
  scale_x_discrete(labels = latin_names) +
  labs(
    title = "Standardized Seed Consumption by Species and Stand",
    x = "Seed Species",
    y = "Standardized Removal"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )


# Plot the average standardized success by Species and Treatment
ggplot(data_cleaned_2, aes(x = Seed_sp, y = seeds, color = Treatment, fill = Treatment)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.6),
              alpha = 0.5, shape = 20, size = 2) +
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(width = 0.6),
               shape = 21, size = 5, color = "black") +
  scale_fill_manual(values = c("Low" = "darkred", "High" = "darkblue", "All" = "darkgrey")) +
  scale_color_manual(values = c("Low" = "darkred", "High" = "darkblue", "All" = "darkgrey")) +
  scale_x_discrete(labels = latin_names) +
  labs(
    title = "Standardized Seed Consumption by Species and Treatment",
    x = "Species",
    y = "Standardized Success"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

# Plot the average standardized success by Treatment and Stand
ggplot(data_cleaned_2, aes(x = Stand, y = seeds, color = Treatment, fill = Treatment)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.6),
              alpha = 0.5, shape = 20, size = 2) +
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(width = 0.6),
               shape = 23, size = 5, color = "black") +
  scale_fill_manual(values = c("Low" = "darkred", "All" = "black", "High" = "lightblue")) +
  scale_color_manual(values = c("Low" = "darkred", "All" = "black", "High" = "lightblue")) +
  labs(
    title = "Standardized Seed Consumption by Treatment and Stand",
    x = "Stand",
    y = "Standardized Success"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )



#ELEVATION GROUP
#Checking by species group (low vs high species)
data_cleaned_2_grouped <- data_cleaned_2 %>%
  mutate(Elevation_group = case_when(
    Seed_sp %in% c("ABLA", "ABAM", "CANO") ~ "High",
    Seed_sp %in% c("THPL", "TSHE", "PSME") ~ "Low",
    TRUE ~ NA_character_
  ))

data_cleaned_2_grouped$Elevation_group <- factor(data_cleaned_2_grouped$Elevation_group,
                              levels = c("Low", "High"))

#Plot the removal per elevation group
ggplot(data_cleaned_2_grouped, aes(x = Elevation_group, y = seeds, fill = Elevation_group)) +
  geom_jitter(width = 0.2, alpha = 0.4, shape = 21, color = "black") +  # individual points
  stat_summary(fun = mean, geom = "point", size = 5, color = "black") +  # group means
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "black") +  # error bars
  scale_fill_manual(values = c("High" = "steelblue", "Low" = "indianred")) +
  labs(title = "Standardized Seed Consumption by Elevation Group",
       x = "Elevation Group", y = "Standardized Removal") +
  theme_minimal() +
  theme(legend.position = "none")

weeks_per_stand <- data_cleaned_2_grouped %>%
  group_by(Stand) %>%
  summarise(n_weeks = n_distinct(Week), .groups = "drop")
data_standardized <- data_cleaned_2_grouped %>%
  left_join(weeks_per_stand, by = "Stand") %>%
  mutate(seeds_per_week = seeds / n_weeks)

data_standardized <- data_cleaned_2_grouped %>%
  group_by(Stand,Week,Elevation_group)%>%
  summarise(mean= mean(seeds))

data_standardized2 <- data_standardized %>%
  group_by(Stand,Elevation_group)%>%
  summarise(mean= mean(mean))

data_standardized3 <- data_cleaned_2_grouped %>%
  group_by(Stand,Elevation_group)%>%
  summarise(mean= mean(seeds))

ggplot(data_standardized, aes(x = Elevation_group, y = seeds_per_week, color = Stand, fill = Stand)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.6),
              alpha = 0.5, shape = 20, size = 2) +
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(width = 0.6),
               shape = 23, size = 5, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               position = position_dodge(width = 0.6), width = 0.2) +
  scale_fill_manual(values = c("TO04" = "#E69F00", "AV06" = "#009E73", "AE10" = "#56B4E9")) +
  scale_color_manual(values = c("TO04" = "#E69F00", "AV06" = "#009E73", "AE10" = "#56B4E9")) +
  labs(
    title = "Mean Weekly Seed Removal by Elevation Group and Stand",
    x = "Elevation Group",
    y = "Mean Seeds Removed per Week"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
  )

#Plot removal per stand and elevation group
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

#plot per elevation group and stand
ggplot(data_cleaned_2_grouped, aes(x = Elevation_group, y = seeds, fill = Stand)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(0.8), width = 0.6, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_dodge(0.8), width = 0.2, color = "black") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Seed Removal by Elevation Group and Stand",
       x = "Elevation Group", y = "Standardized Removal") +
  theme_minimal()

# Same as before but different style
data_standardized <- data_cleaned_2_grouped |>
  group_by(Stand, Elevation_group, Week) |>  # adjust grouping as needed
  summarise(
    mean_seeds = mean(seeds, na.rm = TRUE),
    .groups = "drop"
  )
ggplot(data_standardized, aes(x = Elevation_group, y = mean_seeds, color = Stand, fill = Stand)) +
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


# MODEL FOR SEED REMOVAL------------------------------------------------------------------
#The models are based on ecological questions. I want them to follow my research questions. 

#adding an OBS as seeds are dependent from each other (not every seed is place seperately)
data_cleaned_2$ObsID <- factor(1:nrow(data_cleaned_2))

##**Question 2**
##WHAT ARE THE PREFERRED SEED AND IS THIS PREFERENCE CONSISTENT OVER ELEVATION? or
##WHICH SPECIES IS REMOVED THE MOST AND WHERE THE MOST?

#**Model 1***Research question : 2.	How limiting is seed predation (as measured by seed removal), and how does this vary among dominant conifer species and across elevations?
#*
#*
#Reload the data_cleaned_2 and then reorder based on the graphical results. PSME most eaten and AE10 most removal
data_cleaned_2$Seed_sp <- factor(data_cleaned_2$Seed_sp,
                        levels = c("PSME","THPL", "TSHE", "ABAM", "CANO", "ABLA"),
                        labels = c("Pseudotsuga menziesii","Thuja plicata", "Tsuga heterophylla","Abies amabilis", "Callitropsis nootkatensis","Abies lasiocarpa"))
data_cleaned_2$Stand <- factor(data_cleaned_2$Stand,
                               levels = c("AE10", "AV06", "TO04"),
                               labels = c("High", "Mid", "Low"))

glmer_modelOLRE_New <- glmer(
  cbind(Success, Seeds.Placed - Success) ~ Stand + Seed_sp+
     (1 | Camera) + (1 | ObsID) + (1|Week),
  family = binomial,
  data = data_cleaned_2,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

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

# Custom dodge width
dodge_width <- 0.5

#OVERALL PLOT
#Reordering for plots
em_df$Stand <- factor(em_df$Stand,
                               levels = c("Low", "Mid", "High"))
em_df$Seed_sp <- factor(em_df$Seed_sp,
                                 levels = c("Thuja plicata", "Tsuga heterophylla","Pseudotsuga menziesii","Abies amabilis", "Callitropsis nootkatensis","Abies lasiocarpa"))

#Plot of estimated probability of removal by stand and species
ggplot(em_df, aes(x = Stand, y = prob, color = Seed_sp, shape = Stand)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.7),
                width = 0.2) +
  # Add stand-level means 
  geom_point(data = em_stand_df, aes(x = Stand, y = prob),
             color = "black", size = 4, shape = 18) +
  geom_errorbar(data = em_stand_df,
                aes(x = Stand, ymin = asymp.LCL, ymax = asymp.UCL),
                color = "black",linetype="dotted", width = 0.1) +
  # Custom color for species
  scale_color_manual(values = c(
    "Thuja plicata" = "#D55E00",    # vermilion
    "Tsuga heterophylla" = "#E69F00",    # orange
    "Pseudotsuga menziesii" = "#F0E442",    # bluish green
    "Abies lasiocarpa" = "#0072B2",    # yellow
    "Callitropsis nootkatensis" = "#56b4e9",    # sky blue
    "Abies amabilis" = "#009E73"     # blue
  )) +
  # specify custom shapes for consistency
  scale_shape_manual(values = c("Low" = 18, "Mid" = 18, "High" = 18)) +
  labs(
    title = "Estimated Probability of Seed Removal",
    y = "Probability of Removal",
    x = "Stand",
    color = "Seed Species",
    shape = "Stand Mean"
  )+
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "right"
  )


#Plot of estimated probability of removal by seed species and stand
ggplot(em_df, aes(x = Seed_sp, y = prob, color = Stand, shape = Stand)) +
  geom_point(position = position_dodge(width = dodge_width), size = 5) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = dodge_width),
                width = 0.2) +
  # Custom color for stands (now used for StandLabel)
  scale_color_manual(values = c(
    "Low" = "orange",   
    "Mid" = "forestgreen",   
    "High" = "darkblue"  
  )) +
  scale_shape_manual(values = c("Low" = 16, "Mid" = 17, "High" = 15)) +
  labs(
    title = "Estimated Probability of Seed Removal by Species and Stand",
    x = "Seed Species",
    y = "Probability of Removal",
    color = "Stand",
    shape = "Stand"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "right"
  )



##**Question 3**
##IS THIS REMOVAL DEPENDENT OF HAVING CONSPECIFIC AROUND OR NOT?

#**Model 2***Research question 3.	Does seed community composition influence seed removal? Specifically, are seeds more or less likely to be removed when presented in novel versus familiar seed communities??

#Checking by species group (low vs high species)
#Using data_cleaned_2_grouped (see Plot section)

#Are species more removed when they are in their naturally occuring range?
# data_cleaned_2_grouped$ObsID <- factor(1:nrow(data_cleaned_2_grouped)) #Maybe not necessary depending on how the code is run

glmer_modelOLRE_New2 <- glmer(
  cbind(Success, Seeds.Placed - Success) ~ Elevation_group * Stand+
    (1 | Camera) + (1 | ObsID) + (1|Week) ,
  family = binomial,
  data = data_cleaned_2_grouped,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

sim_res_2 <- simulateResiduals(fittedModel = glmer_modelOLRE_New2, plot = TRUE)
testDispersion(sim_res_2)#does not show overdispersion but underdispersion
summary(glmer_modelOLRE_New2)


#This model solves this underdispersion problem.
model_bb <- glmmTMB(
  cbind(Success, Seeds.Placed - Success) ~ Elevation_group * Stand +
    (1 | Camera) + (1 | ObsID) + (1|Week),
  data = data_cleaned_2_grouped,
  family = betabinomial()
)
sim_res_2 <- simulateResiduals(fittedModel = model_bb, plot = TRUE)
testDispersion(model_bb)


# POST-HOC Test
em2 <- emmeans(model_bb, ~ Elevation_group * Stand, type = "response")
pairs(em2, by = "Stand")
pairs(em2, by = "Elevation_group")
pairs(em2)
em_df2 <- as.data.frame(em2)

em_df2$Stand <- factor(em_df2$Stand,
                       levels = c("AE10", "AV06", "TO04"),
                       labels = c("High", "Mid", "Low"))
em_df2$Stand <- factor(em_df2$Stand,
                       levels = c("Low", "Mid", "High"))
em_df2$Elevation_group <- factor(em_df2$Elevation_group,
                       levels = c("Low", "High"))


# Plot of removal probability by stand and elevation group. 
ggplot(em_df2, aes(x = Stand, y = prob, color = Elevation_group, group = Elevation_group)) +
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
 ylim(0, 1)+
  theme_minimal() +
  theme(text = element_text(size = 14))

#Checking between elevation group is there is a statistical differemce
em_elev <- emmeans(glmer_modelOLRE_New2, ~ Elevation_group, type = "response")
pairs(em_elev)#There is a statistical difference between the groups. The group Low in general is more removed than the group high

em_stand <- emmeans(glmer_modelOLRE_New2, ~ Stand, type = "response")
pairs(em_stand)
#Samre results as the previous model (model 1: Question2)



##**Question 3**
##Are species more removed when they are around conspecific?

#Plot for removal for each Low elevation seed specie per treatment and stand.
ggplot(data_subset, aes(x = Treatment, y = seeds, color = Stand, fill = Stand)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.6),
              alpha = 0.5, shape = 20, size = 2) +  # Adds jittered points
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(width = 0.6),
               shape = 23, size = 5, color = "black") +  # Adds mean points
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               position = position_dodge(width = 0.6), width = 0.2) +  # Adds error bars
  facet_wrap(~ Seed_sp, scales = "free_y") +  # Facets by species
  scale_fill_manual(values = c("TO04" = "#E69F00", "AV06" = "#009E73", "AE10" = "#56B4E9")) +
  scale_color_manual(values = c("TO04" = "#E69F00", "AV06" = "#009E73", "AE10" = "#56B4E9")) +
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

#**Model 3***Research question 3.	Does seed community composition influence seed removal? Specifically, are seeds more or less likely to be removed when presented in novel versus familiar seed communities??

data_subset <- data_cleaned_2 %>% 
  filter(!(Treatment =="High"))
data_subset <- data_cleaned_2 %>% 
  filter(!(Seed_sp %in% c("ABAM", "CANO","ABLA")))


#Model
data_subset$ObsID <- factor(1:nrow(data_subset))
glmer_modelOLRE_New3 <- glmer(
  cbind(Success, Seeds.Placed - Success) ~ Treatment * Seed_sp +
    (1 | Camera) + (1 | ObsID) + (1 | Week),
  family = binomial,
  data = data_subset,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
sim_res <- simulateResiduals(fittedModel = glmer_modelOLRE_New3, plot = TRUE)
testDispersion(sim_res)#does not show overdispersion 
summary(glmer_modelOLRE_New3)

# POST-HOC Test
em3 <- emmeans(glmer_modelOLRE_New3, ~ Treatment * Seed_sp, type = "response")
pairs(em3, by = "Treatment")
pairs(em3, by = "Seed_sp")
pairs(em3)
em_df3 <- as.data.frame(em3)

em_df3$Seed_sp <- factor(em_df3$Seed_sp,
                       levels = c("PSME", "THPL", "TSHE"),
                       labels = c("Pseudotsuga menziesii", "Thuja plicata", "Tsuga heterophylla"))
em_df3$Seed_sp <- factor(em_df3$Seed_sp,
                       levels = c("Thuja plicata", "Tsuga heterophylla", "Pseudotsuga menziesii"))
em_df3$Treatment <- factor(em_df3$Treatment,
                                 levels = c("Low", "All"))


# Plot of removal probability by stand and elevation group. 
ggplot(em_df3, aes(x = Seed_sp, y = prob, color = Treatment, group = Treatment)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_line(position = position_dodge(width = 0.3), size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, position = position_dodge(width = 0.3)) +
  scale_color_manual(values = c("Low" = "red", "All" = "grey")) +
  labs(
    title = "Predicted Seed Removal Probability by Species and presence or absence of conspecifics",
    y = "Predicted Probability of Seed Removal",
    x = "Forest Stand Elevation",
    color = "Seed Elevation Group"
  ) +
  ylim(0, 1)+
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "right"
  )

#Checking between elevation group is there is a statistical differemce
em_species <- emmeans(glmer_modelOLRE_New3, ~ Seed_sp, type = "response")
pairs(em_species)#There is a statistical difference between all seed species. 

em_treatment <- emmeans(glmer_modelOLRE_New3, ~ Treatment, type = "response")
pairs(em_treatment)
#there is no statistical difference between the treatments. Seeds are removed the same no matter if there are other seed species around or not. 




#**Model 4**: Are species more removed when they are around conspecific?
data_subset_2 <- data_cleaned_2 %>% 
  filter(!(Treatment =="Low"))
data_subset_2 <- data_cleaned_2 %>% 
  filter(!(Seed_sp %in% c("PSME", "THPL", "TSHE")))

glmer_modelOLRE_New4 <- glmer(
  cbind(Success, Seeds.Placed - Success) ~ Treatment * Seed_sp+
    (1 | Camera) + (1 | ObsID) + (1|Week) ,
  family = binomial,
  data = data_subset_2,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7))
)
overdisp_fun(glmer_modelOLRE_New4)

sim_res <- simulateResiduals(fittedModel = glmer_modelOLRE_New4, plot = TRUE)
testDispersion(sim_res)#does not show overdispersion 
summary(glmer_modelOLRE_New4)

# POST-HOC Test
em4 <- emmeans(glmer_modelOLRE_New4, ~ Treatment * Seed_sp, type = "response")
pairs(em4, by = "Treatment")
pairs(em4, by = "Seed_sp")
pairs(em4)
em_df4 <- as.data.frame(em4)

em_df4$Seed_sp <- factor(em_df4$Seed_sp,
                         levels = c("ABAM", "ABLA", "CANO"),
                         labels = c("Abies amabilis", "Abies lasiocarpa", "Callitropsis nootkatensis"))
em_df4$Seed_sp <- factor(em_df4$Seed_sp,
                         levels = c("Abies amabilis", "Callitropsis nootkatensis", "Abies lasiocarpa"))
em_df4$Treatment <- factor(em_df4$Treatment,
                           levels = c("High", "All"))


# Plot of removal probability by stand and elevation group. 
ggplot(em_df4, aes(x = Seed_sp, y = prob, color = Treatment, group = Treatment)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_line(position = position_dodge(width = 0.3), size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, position = position_dodge(width = 0.3)) +
  scale_color_manual(values = c("High" = "blue", "All" = "grey")) +
  labs(
    title = "Predicted Seed Removal Probability by Species and presence or absence of conspecifics",
    y = "Predicted Probability of Seed Removal",
    x = "Forest Stand Elevation",
    color = "Seed Elevation Group"
  ) +
  ylim(0, 1)+
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "right"
  )




#Checking between elevation group is there is a statistical differemce
em_species2 <- emmeans(glmer_modelOLRE_New4, ~ Seed_sp, type = "response")
pairs(em_species2)#There is a statistical difference between CAno and the rest 

em_treatment2 <- emmeans(glmer_modelOLRE_New4, ~ Treatment, type = "response")
pairs(em_treatment2)
#there is no statistical difference between the treatments. Seeds are removed the same no matter if there are other seed species around or not. 

###PLotting both plots side by side 
em_df3$Group <- "Low vs All"
em_df4$Group <- "High vs All"
em_merged <- rbind(em_df3, em_df4)
ggplot(em_merged, aes(x = Seed_sp, y = prob, color = Treatment, group = interaction(Treatment, Group))) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_line(position = position_dodge(width = 0.3), size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, position = position_dodge(width = 0.3)) +
  scale_color_manual(values = c("Low" = "red", "High" = "blue", "All" = "grey")) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "Predicted Seed Removal by Seed Species and Treatment",
    y = "Predicted Probability of Seed Removal",
    x = "Seed Species",
    color = "Treatment"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "right"
  )

# Seed predator density analysis ------------------------------------------

# Filter to rows where an animal was present and the Species_ID is not missing or "NA" and were the animal was on the tray
predator_data_clean <- predators_data %>%
  filter(Animal_presence == "Yes", !is.na(Species_ID), Species_ID != "NA")%>%
  filter(On_tray == "On")

# Summarize detections per species when the animal is "on the tray" - This makes it sure that the animal is interacting with the tray
detections_on_tray <- predator_data_clean %>%
  group_by(Species_ID, Stand) %>%
  summarise(Total_Detections_On_Tray = n()) %>%
  arrange(desc(Total_Detections_On_Tray))

#Checking the dataset
#checking for the weeks
predator_data_clean_week<- predator_data_clean %>%
  group_by (Stand)%>%
  summarise (week= n_distinct(Week))
#AE10 only has 3 weeks... so I will make sure it is standartized
sampling_effort <- tibble(
  Stand = c("TO04", "AV06", "AE10"),
  Weeks_sampled = c(4, 4, 3)
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
#Also different when considering only species on the tray. 

ggplot(predator_density_site_species_standardized, aes(x = Species_ID, y = detections_per_week, fill = Stand)) +
  geom_col(position = "dodge") +
  labs(y = "Mean detections per Week", x = "Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



str(predator_data_clean)


#“Is the number of different species detected per camera different across forest stands?”
species_per_camera <- predator_data_clean %>%
  group_by(Stand, Camera) %>%
  summarise(species_richness = n_distinct(Species_ID), .groups = "drop")
ggplot(species_per_camera, aes(x = Stand, y = species_richness, fill = Stand)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.2, height = 0, size = 2, alpha = 0.7) +
  labs(y = "Number of species per camera", x = "Stand") +
  theme_minimal()

kruskal.test(species_richness ~ Stand, data = species_per_camera)
install.packages("FSA")
library(FSA)

dunnTest(species_richness ~ Stand, data = species_per_camera, method = "bh")  # Benjamini-Hochberg correction
ggplot(species_per_camera, aes(x = Stand, y = species_richness, fill = Stand)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.5) +
  labs(
    title = "Species richness per camera across forest stands",
    x = "Stand",
    y = "Species richness"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_brewer(palette = "Set2") +
  annotate("text", x = 1, y = max(species_per_camera$species_richness) + 0.5, label = "a", size = 6) +
  annotate("text", x = 2, y = max(species_per_camera$species_richness) + 0.5, label = "b", size = 6) +
  annotate("text", x = 3, y = max(species_per_camera$species_richness) + 0.5, label = "b", size = 6)


# Poisson model
glm_model <- glm(species_richness ~ Stand, data = species_per_camera, family = poisson)
summary(glm_model)
sim_res_nb <- simulateResiduals(glm_model)
plot(sim_res_nb)

emm <- emmeans(glm_model, ~ Stand)
pairwise <- pairs(emm, adjust = "tukey")
emm_df <- as.data.frame(emm)
ggplot(emm_df, aes(x = Stand, y = emmean)) +
  geom_col(fill = "skyblue", width = 0.6) +
  labs(
    title = "Estimated Species Richness per Camera by Stand",
    y = "Estimated Species Richness",
    x = "Stand"
  ) +
  theme_minimal(base_size = 14)

pm_data <- predator_data_clean[predator_data_clean$Species_ID == "Peromyscus maniculatus", ]
pm_per_camera <- pm_data %>%
  group_by(Stand, Camera) %>%
  summarise(pm_detections = n(), .groups = "drop")
ggplot(pm_per_camera, aes(x = Stand, y = pm_detections, fill = Stand)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.5) +
  labs(
    title = "Peromyscus maniculatus detections per camera",
    x = "Stand",
    y = "Number of detections"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_brewer(palette = "Set2")

kruskal.test(pm_detections ~ Stand, data = pm_per_camera)
dunnTest(pm_detections ~ Stand, data = pm_per_camera, method = "bh")

# PERO AND TAMIA ----------------------------------------------------------

data_ae10 <- predator_data_clean[predator_data_clean$Stand == "AE10", ]
str(data_ae10)
# 1. Filter for Tamias and Peromyscus only
data_subset <- data_ae10 %>%
  filter(Species_ID %in% c("Tamias sp", "Peromyscus maniculatus"))

# 2. Count detections per species
counts <- data_subset %>%
  count(Species_ID)

# 3. Bar plot
ggplot(counts, aes(x = Species_ID, y = n, fill = Species_ID)) +
  geom_col(width = 0.6) +
  labs(
    title = "Detections at AE10: Tamias vs Peromyscus",
    x = "Species",
    y = "Number of Detections"
  ) +
  scale_fill_manual(values = c("Tamias sp" = "#FFA07A", "Peromyscus maniculatus" = "#87CEFA")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")


# Filter for Tamias and Peromyscus, and where the animal was on a depression
on_depression_counts <- data_ae10 %>%
  filter(Species_ID %in% c("Tamias sp", "Peromyscus maniculatus"),
         On_depression == "Yes") %>%
  count(Species_ID, name = "n_detections_on_depression") %>%
  arrange(desc(n_detections_on_depression))

ggplot(on_depression_counts, aes(x = Species_ID, y = n_detections_on_depression, fill = Species_ID)) +
  geom_col(width = 0.6) +
  labs(
    title = "Detections on Seed Depressions at AE10",
    x = "Species",
    y = "Number of Detections on Depressions"
  ) +
  scale_fill_manual(values = c("Tamias sp" = "#FFA07A", "Peromyscus maniculatus" = "#87CEFA")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")




library(lubridate)
library(forcats)

# Step 1: Combine Date and Time into a single datetime column
predator_data_ae10 <- predator_data_ae10 %>%
  mutate(DateTime = as.POSIXct(paste(as.Date(Date), format(Time, "%H:%M:%S")), 
                               format = "%Y-%m-%d %H:%M:%S"))

# Step 2: Arrange data for comparison
predator_filtered_ae10 <- predator_data_ae10 %>%
  arrange(Stand, Camera, Species_ID, DateTime) %>%
  group_by(Stand, Camera, Species_ID) %>%
  mutate(TimeDiff = as.numeric(difftime(DateTime, lag(DateTime), units = "mins")),
         Keep = is.na(TimeDiff) | TimeDiff > 5) %>%
  filter(Keep) %>%
  ungroup()

ggplot(predator_filtered_ae10, aes(x = fct_infreq(Species_ID))) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Predator Detections per Species",
    x = "Species",
    y = "Number of Unique Detections"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


predator_filtered <- predator_filtered_ae10 %>%
  mutate(Hour = hour(DateTime))

ggplot(predator_filtered, aes(x = Hour)) +
  geom_histogram(binwidth = 1, fill = "darkgreen", color = "white") +
  theme_minimal() +
  labs(
    title = "Hourly Detection Activity",
    x = "Hour of Day",
    y = "Number of Detections"
  )

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




# Not sure what do with this :  -------------------------------------------

###From plots
#Checking for seed weight
# Fit a linear model to check if removal rate increases with seed weight per seed
model_removal <- lm(seeds ~ seed_weight, data = data_cleaned_2)
# Summarize the model
summary(model_removal)
plot(model_removal)
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



###From model Overall
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

####NEW
# emmeans for interaction between Treatment and Seed_sp
emm <- emmeans(glmer_modelOLRE_New3, ~ Treatment * Seed_sp, type = "response")
# Convert to dataframe for plotting
pred_df <- as.data.frame(emm)
ggplot(pred_df, aes(x = Treatment, y = prob, fill = Seed_sp)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black") +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.8), width = 0.2) +
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

# Saving dataset ----------------------------------------------------------------

# Save the cleaned data as an RData file
# save(data_cleaned_2, file = "C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/data_cleaned_2.RData")