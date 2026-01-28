####Code for CAMERA TRAP EXPERIMENT US 2017
####Last modified : 28.01.2025
####ETHZ Eléonore Perret
####Code for measuring seed removal.  

# Loading librairies ------------------------------------------------------
# install.packages("dplyr")
# install.packages("ggplot2")
#install.packages("DHARMa")
#install.packages("emmeans")


library(readxl)
library(ggplot2)
library(dplyr)
library(lme4)
library(DHARMa)
library(emmeans)
library(glmmTMB)
library(MASS)



# Loading the data --------------------------------------------------------
# Set the working directory
getwd()
setwd("C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets")

# List files in the main directory
list.files()

# Load datasets
seed_predation <- read.csv("SeedPredation_First_week.csv", sep=";") #Seed trays information
all_data_seed<- read.csv("seed_data.csv",sep= ";") #Removal rates informations
predators_data <- read_excel("camera_data.xlsx") #Camera data results
load("data_cleaned_2.RData") #Done after cleaning the seed data. See Seed predation results later in the script

# CAMERA TRAP DATA Information : Process_data ------------------------------------------------------------
#Because I have empty rows after the row 27 (this comes from excel), I will first delete all the rows below
seed_predation <- seed_predation %>% slice(1:27)

# SEED PREDATION: Process data -End Product (data_cleaned_2.RData) ---------------------------------------------------
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

save(data_cleaned_2, file = "data_cleaned_2.RData")


# PLOTS(seed removal per species, stand, treatment and elevation group)-------------------------------------------------------------------
#Overall removal
overall_removal <- data_cleaned_2 %>%
  mutate(removal_prop = Success / Seeds.Placed) %>% 
  summarize(mean_removal= mean(removal_prop, na.rm = TRUE))
overall_removal

#Removal per species
mean_se_data <- data_cleaned_2 %>%
  group_by(Seed_sp) %>%
  summarise(
    mean_seeds = mean(seeds, na.rm = TRUE),
    se_seeds = sd(seeds, na.rm = TRUE) / sqrt(n())
  )
print(mean_se_data)

#Adding correctenness to my plot
# Factor level for the right order
data_cleaned_2$Seed_sp <- factor(data_cleaned_2$Seed_sp,
                                 levels = c("THPL", "TSHE", "PSME", "ABAM", "CANO", "ABLA"))
# also in the summary data
mean_se_data$Seed_sp <- factor(mean_se_data$Seed_sp,
                               levels = c("THPL", "TSHE", "PSME", "ABAM", "CANO", "ABLA"))
# correct names
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
# Reorder Stand and label
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

#WEEK
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

#SPECIES + STAND
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

#SPECIES + TREATMENT
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

#STAND + TREATMENT
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

##**Question 2** Based on the manuscript
##WHAT ARE THE PREFERRED SEED AND IS THIS PREFERENCE CONSISTENT OVER ELEVATION? or
##WHICH SPECIES IS REMOVED THE MOST AND WHERE THE MOST?

#**Model 1***Research question : 2.	How limiting is seed predation (as measured by seed removal), and how does this vary among dominant conifer species and across elevations?
#*
#*
#Reload the data_cleaned_2 and then reorder based on the graphical results. PSME most eaten and AE10 most removal: I have done this so that those two are the reference points and when using summary I can see which ones are statistically different from my references.
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
em_seeds_df <- as.data.frame(em_seeds)
seed_contrasts <- pairs(em_seeds, adjust = "tukey")
summary(seed_contrasts)

#Plotting species and stand contrasts
df_contrasts <- as.data.frame(summary(seed_contrasts, infer = c(TRUE, TRUE), type = "response"))
ggplot(df_contrasts, aes(x = contrast, y = odds.ratio)) +
  geom_point() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed") +   # reference line (no difference)
  coord_flip() +                                      # flip for readability
  theme_minimal() +
  labs(y = "Odds ratio (with 95% CI)",
       x = "Contrast",
       title = "Pairwise contrasts between species")

df_contrasts_2 <- as.data.frame(summary(stand_contrasts, infer = c(TRUE, TRUE), type = "response"))
ggplot(df_contrasts_2, aes(x = contrast, y = odds.ratio)) +
  geom_point() +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed") +   # reference line (no difference)
  coord_flip() +                                      # flip for readability
  theme_minimal() +
  labs(y = "Odds ratio (with 95% CI)",
       x = "Contrast",
       title = "Pairwise contrasts between Stand")

# Pairwise comparisons *within* Stand across species:
pairs(em_both, by = "Stand")
pairs(em_both, by = "Seed_sp")


#Plotting both
em_both <- emmeans(glmer_modelOLRE_New, ~ Stand + Seed_sp, type = "response")
all_contrasts <- pairs(em_both, adjust = "tukey")
em_df <- as.data.frame(em_both)

# Custom dodge width
dodge_width <- 0.5

#OVERALL PLOT
#NEW PLOTS FROM REVISION SECOND DRAFT
# Define color-blind friendly palette for stands
stand_colors_cb <- c(
  "Low" = "#E69F00",   # orange
  "Mid" = "#009E73",   # bluish green
  "High" = "#0072B2"   # blue
)

# Define shapes for species
species_shapes <- c(16, 17, 15, 18, 8, 3, 4)  # 7 distinct, clear shapes
species_names <- unique(em_df$Seed_sp)
species_shapes <- setNames(species_shapes[seq_along(species_names)], species_names)

# Defining the stand and species order
em_df <- em_df %>%
  mutate(Stand = factor(Stand, levels = c("Low", "Mid", "High")))
em_df <- em_df %>%
  mutate(Seed_sp = factor(Seed_sp, levels = c("Thuja plicata","Tsuga heterophylla","Pseudotsuga menziesii","Abies amabilis","Callitropsis nootkatensis","Abies lasiocarpa")))

# Compute mean probability per stand (horizontal line across species)
stand_means <- em_df %>%
  group_by(Stand) %>%
  summarise(mean_prob = mean(prob, na.rm = TRUE))

# Plot
ggplot(em_df, aes(x = Stand, y = prob, color = Stand, shape = Seed_sp)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0, dodge.width = 0.6),
              size = 5, alpha = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.6), width = 0.2) +
  # Add mean line per stand
  geom_hline(data = stand_means, aes(yintercept = mean_prob, color = Stand),
             linetype = "dashed", size = 0.9, alpha = 0.7, show.legend = FALSE) +
  scale_color_manual(values = stand_colors_cb) +
  scale_shape_manual(values = species_shapes) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1),
    labels = scales::percent_format(accuracy = 10)
  ) +
  labs(
    title = "Estimated Probability of Seed Removal by Elevation and Species",
    x = "Elevation (Stand)",
    y = "Probability of Removal",
    color = "Elevation",
    shape = "Seed Species"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.6),
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "right"
  )
#To add manually, the different stand on the lines because I feed the color is not superbe
#END OF NEW PLOT REVISION SECOND DRAFT



##**Question 3**
##IS THIS REMOVAL DEPENDENT OF HAVING CONSPECIFIC AROUND OR NOT?

#**Model 2***Research question 3.	Does seed community composition influence seed removal? Specifically, are seeds more or less likely to be removed when presented in novel versus familiar seed communities??

#Checking by species group (low vs high species)
#Using data_cleaned_2_grouped (see Plot section)

#Are species more removed when they are in their naturally occuring range?
data_cleaned_2_grouped$ObsID <- factor(1:nrow(data_cleaned_2_grouped)) 


#ELEVATION GROUP
#Checking by species group (low vs high species)
data_cleaned_2_grouped <- data_cleaned_2 %>%
  mutate(Elevation_group = case_when(
    Seed_sp %in% c("Abies lasiocarpa", "Abies amabilis", "Callitropsis nootkatensis") ~ "High",
    Seed_sp %in% c("Thuja plicata", "Tsuga heterophylla", "Pseudotsuga menziesii") ~ "Low",
    TRUE ~ NA_character_
  ))

data_cleaned_2_grouped$Elevation_group <- factor(data_cleaned_2_grouped$Elevation_group,
                                                 levels = c("Low", "High"))

#Building my model
glmer_modelOLRE_New2 <- glmer(
  cbind(Success, Seeds.Placed - Success) ~ Elevation_group * Stand+
    (1 | Seed_sp) + (1 | ObsID) + (1|Week) + (1|Camera) ,
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
    (1 | Camera) + (1 | ObsID) + (1|Week) + (1|Seed_sp),
  data = data_cleaned_2_grouped,
  family = betabinomial()
)
sim_res_2 <- simulateResiduals(fittedModel = model_bb, plot = TRUE)
testDispersion(model_bb)

summary(model_bb)

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
#Same results as the previous model (model 1: Question2)



##**Question 3** Same question as before
##Are species more removed when they are around species that are naturally present in their environement?
#In other words: Is there more removal when the treatment is all than when it is just the high/low treatment?


#**Model 3***Research question 3.	Does seed community composition influence seed removal? Specifically, are seeds more or less likely to be removed when presented in novel versus familiar seed communities??

data_subset <- data_cleaned_2 %>% 
  filter(!(Treatment =="High"))
data_subset <- data_cleaned_2 %>% 
  filter(!(Seed_sp %in% c("ABAM", "CANO","ABLA")))


#Model
data_subset$ObsID <- factor(1:nrow(data_subset))
glmer_modelOLRE_New3 <- glmer(
  cbind(Success, Seeds.Placed - Success) ~ Treatment * Seed_sp + Stand +
    (1 | Camera) + (1 | ObsID) + (1 | Week),
  family = binomial,
  data = data_subset,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
sim_res <- simulateResiduals(fittedModel = glmer_modelOLRE_New3, plot = TRUE)
testDispersion(sim_res)#does not show overdispersion 
summary(glmer_modelOLRE_New3)

# POST-HOC Test
em3 <- emmeans(glmer_modelOLRE_New3, ~ Treatment * Seed_sp + Stand, type = "response")
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


# Data
df <- tribble(
  ~Treatment, ~contrast,      ~odds.ratio, ~SE,  ~z.ratio, ~p.value,
  "All", "PSME / THPL", 14.0290, 9.1212,  4.062, 0.0001,
  "All", "PSME / TSHE",  3.9273, 2.5817,  2.081, 0.0938,
  "All", "THPL / TSHE",  0.2799, 0.1640, -2.173, 0.0759,
  "Low", "PSME / THPL", 25.5575,17.1008,  4.844, 0.0001,
  "Low", "PSME / TSHE",  2.4221, 1.6397,  1.307, 0.3914,
  "Low", "THPL / TSHE",  0.0948, 0.0571, -3.913, 0.0003
)

# Compute 95% CI on log-odds scale
df <- df %>%
  mutate(
    logOR = log(odds.ratio),
    lower = logOR - 1.96*SE,
    upper = logOR + 1.96*SE,
    signif = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ "ns"
    )
  )

# Plot
ggplot(df, aes(x = contrast, y = logOR, ymin = lower, ymax = upper,
               color = Treatment, shape = signif)) +
  geom_pointrange(position = position_dodge(width = 0.6)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = signif), 
            position = position_dodge(width = 0.6), 
            vjust = -1, size = 5, show.legend = FALSE) +
  coord_flip() +
  labs(y = "Log Odds Ratio (95% CI)", x = "Contrast",
       shape = "Significance") +
  theme_minimal(base_size = 14)


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


#NEW PLOT
# Color-blind-friendly palette for treatments
treatment_colors <- c(
  "All"  = "#999999",  # grey
  "High" = "#0072B2",  # blue
  "Low"  = "#E69F00"   # orange
)

em_df3 <- em_df3 %>%
  mutate(Stand = factor(Stand, levels = c("TO04", "AV06", "AE10")))

# Custom labels for x-axis
stand_labels <- c(
  "TO04" = "Low Elevation",
  "AV06" = "Mid Elevation",
  "AE10" = "High Elevation"
)
# Plot
plot1<-ggplot(em_df3, aes(x = Stand, y = prob, color = Treatment, group = Treatment)) +
  geom_point(position = position_dodge(width = 0.3), size = 4) +
  geom_line(position = position_dodge(width = 0.3), size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.2,
                position = position_dodge(width = 0.3)) +
  scale_color_manual(values = treatment_colors) +
  scale_x_discrete(labels = stand_labels) +
  facet_wrap(~Seed_sp, ncol = 1, scales = "free_y") +
  scale_y_continuous(limits = c(0,1), labels = scales::percent_format(accuracy = 10)) +
  labs(
    title = "Predicted Seed Removal Probability by Species, Elevation, and Treatment",
    x = "Forest Stand (Elevation)",
    y = "Predicted Probability of Seed Removal",
    color = "Seed Elevation Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )
#END OF NEW PLOT


#**Model 4**: Are species more removed when they are around conspecific?
data_subset_2 <- data_cleaned_2 %>% 
  filter(!(Treatment =="Low"))
data_subset_2 <- data_cleaned_2 %>% 
  filter(!(Seed_sp %in% c("PSME", "THPL", "TSHE")))

glmer_modelOLRE_New4 <- glmer(
  cbind(Success, Seeds.Placed - Success) ~ Treatment * Seed_sp +Stand+
    (1 | Camera) + (1 | ObsID) + (1|Week) ,
  family = binomial,
  data = data_subset_2,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e7))
)

sim_res <- simulateResiduals(fittedModel = glmer_modelOLRE_New4, plot = TRUE)
testDispersion(sim_res)#does not show overdispersion 
summary(glmer_modelOLRE_New4)

# POST-HOC Test
em4 <- emmeans(glmer_modelOLRE_New4, ~ Treatment * Seed_sp + Stand, type = "response")
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



# Color-blind-friendly palette for treatments
treatment_colors <- c(
  "All"  = "#999999",  # grey
  "High" = "#0072B2",  # blue
  "Low"  = "#E69F00"   # orange
)

em_df4 <- em_df4 %>%
  mutate(Stand = factor(Stand, levels = c("TO04", "AV06", "AE10")))

# Custom labels for x-axis
stand_labels <- c(
  "TO04" = "Low Elevation",
  "AV06" = "Mid Elevation",
  "AE10" = "High Elevation"
)
# Plot
plot2<-ggplot(em_df4, aes(x = Stand, y = prob, color = Treatment, group = Treatment)) +
  geom_point(position = position_dodge(width = 0.3), size = 4) +
  geom_line(position = position_dodge(width = 0.3), size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.2,
                position = position_dodge(width = 0.3)) +
  scale_color_manual(values = treatment_colors) +
  scale_x_discrete(labels = stand_labels) +
  facet_wrap(~Seed_sp, ncol = 1, scales = "free_y") +
  scale_y_continuous(limits = c(0,1), labels = scales::percent_format(accuracy = 10)) +
  labs(
    title = "Predicted Seed Removal Probability by Species, Elevation, and Treatment",
    x = "Forest Stand (Elevation)",
    y = "Predicted Probability of Seed Removal",
    color = "Seed Elevation Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )



library(cowplot)

combined_plot <- plot_grid(plot1, plot2, nrow = 1, rel_widths = c(1, 1))
combined_plot


# Example: update species names
species_labels <- c(
  "ABAM" = "A. amabilis",
  "ABLA" = "A. lasiocarpa",
  "CANO" = "C. nootkatensis",
  "PSME"= "P. menziesii", 
  "THPL"= "T. plicata", 
  "TSHE"= "T. heterophylla"
)

# Modify left plot (species stacked)
plot1_mod <- plot1 +
  facet_wrap(~Seed_sp, ncol = 1, scales = "free_y", labeller = labeller(Seed_sp = species_labels)) +
  theme(legend.position = "right")  # keep legend on left

# Modify right plot (treatment summary, for example)
plot2_mod <- plot2 +
  facet_wrap(~Seed_sp, ncol = 1, scales = "free_y", labeller = labeller(Seed_sp = species_labels)) +
  scale_y_continuous(limits = c(0,1), labels = scales::percent_format(accuracy = 10), position = "right") +
  theme(legend.position = "right")  # hide duplicate legend

# Combine plots side by side
combined <- plot_grid(
  plot1_mod, plot2_mod,
  nrow = 1,
  rel_widths = c(1, 1)  # adjust width ratio to balance appearance
)

# Function to extract the legend from a ggplot
get_legend <- function(myplot) {
  tmp <- ggplotGrob(myplot)
  leg <- gtable::gtable_filter(tmp, "guide-box")
  return(leg)
}

# Extract legend from plot1
shared_legend <- get_legend(plot1_mod)

# Remove legends from the individual plots
plot1_noleg <- plot1_mod + theme(legend.position = "none")
plot2_noleg <- plot2_mod + theme(legend.position = "none")

# Combine plots side by side with the shared legend on the right
combined <- plot_grid(
  plot1_noleg, plot2_noleg, shared_legend,
  nrow = 1,
  rel_widths = c(3, 3, 1)  # adjust widths: first two plots vs legend
)

combined

# Add a common title
final_plot <- ggdraw() +
  draw_label("Predicted Seed Removal Probability by Species, Elevation, and Treatment",
             fontface = 'bold', x = 0.5, hjust = 0.5, size = 16) +
  draw_plot(combined, y = -0,05, height = 1)  # adjust y if needed

final_plot


# Seed predator density analysis ------------------------------------------

# Filter to rows where an animal was present and the Species_ID is not missing or "NA" and were the animal was on the tray
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



#New plots based on David's comment (2nd Draft revisions)
# Okabe-Ito colorblind-friendly palette (8 colors)
cb_palette <- c(
  "#0072B2", # blue
  "#E69F00", # orange
  "#009E73", # green
  "#F0E442", # yellow
  "#56B4E9", # light blue
  "#D55E00", # red
  "#CC79A7", # pink
  "#999999"  # grey
)

# Assign to your species manually (truncate or repeat if needed)
species_colors_cb <- c(
  "Peromyscus maniculatus" = "#0072B2",  # blue
  "Tamias sp."             = "#E69F00",  # orange
  "Lepus americanus"       = "#009E73",  # green
  "Birds"                  = "#F0E442",  # yellow
  "Glaucomys oregonensis"  = "#D55E00",  # red
  "Shrew sp."              = "#CC79A7",  # purple
  "Vole sp."               = "#56B4E9",  # light blue
  "Zapus sp."              = "#999999"   # grey
)

data_pie <- predator_density_site_species_standardized %>%
  filter(Species_ID != "Not identified") %>%  # remove unidentified species
  group_by(Species_ID) %>%
  summarise(total_detections = sum(detections_per_week))
ggplot(data_pie, aes(x = 2, y = total_detections, fill = Species_ID)) +
  geom_col(color = "white") +
  coord_polar(theta = "y", start = 0) +
  xlim(0.5, 2.5) +
  scale_fill_manual(values = species_colors_cb) +
  theme_void() +
  labs(title = "Predator composition (doughnut chart)", fill = "Species") +
  theme(legend.position = "right")

#New plot with differences per site
data_doughnut <- predator_density_site_species_standardized %>%
  filter(Species_ID != "Not identified") %>%  # remove unidentified species
  group_by(Stand, Species_ID) %>%
  summarise(total_detections = sum(detections_per_week), .groups = "drop")
ggplot(data_doughnut, aes(x = 2, y = total_detections, fill = Species_ID)) +
  geom_col(color = "white") +
  coord_polar(theta = "y", start = 0) +
  xlim(0.5, 2.5) +
  scale_fill_manual(values = species_colors_cb) +
  facet_wrap(~Stand) +
  theme_void() +
  labs(title = "Predator detections per site (doughnut chart)", fill = "Species") +
  theme(
    legend.position = "right",
    strip.text = element_text(size = 10, face = "bold")
  )


# Normalize detections per site so each doughnut sums to 1
data_doughnut_norm <- data_doughnut %>%
  group_by(Stand) %>%
  mutate(perc = total_detections / sum(total_detections)) %>%
  ungroup()

ggplot(data_doughnut_norm, aes(x = 2, y = perc, fill = Species_ID)) +
  geom_col(color = "white", width = 1) +
  coord_polar(theta = "y", start = 0) +
  xlim(0.5, 2.5) +
  scale_fill_manual(values = species_colors_cb) +
  facet_wrap(~Stand) +
  theme_void() +
  labs(title = "Predator detections per site (doughnut chart)",
       fill = "Species") +
  theme(
    legend.position = "right",
    strip.text = element_text(size = 10, face = "bold")
  )


#With percentages. 
data_doughnut_norm <- data_doughnut_norm %>%
  mutate(label = ifelse(perc > 0.05, paste0(round(perc * 100), "%"), ""))

ggplot(data_doughnut_norm, aes(x = 2, y = perc, fill = Species_ID)) +
  geom_col(color = "white", width = 1) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3) +
  coord_polar(theta = "y", start = 0) +
  xlim(0.5, 2.5) +
  scale_fill_manual(values = species_colors_cb) +
  facet_wrap(~Stand) +
  theme_void() +
  labs(title = "Predator detections per site (doughnut chart)",
       fill = "Species") +
  theme(legend.position = "right")


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

#THis doesn't make sense as not all species appear in the same locations; so in results I take only the difference between Pero and the rest
emm_species <- emmeans(model_nb, ~ Species_ID)
summary(emm_species, type = "response")
pairs(emm_species, adjust = "tukey")

emm_stand <- emmeans(model_nb, ~ Stand)
summary(emm_stand, type = "response")
pairs(emm_stand, adjust = "tukey")

drop1(model_nb, test = "Chisq")

# Saving dataset ----------------------------------------------------------------

# Save the cleaned data as an RData file
# save(data_cleaned_2, file = "C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/data_cleaned_2.RData")



