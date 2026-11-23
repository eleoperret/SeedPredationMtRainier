####Code for CAMERA TRAP EXPERIMENT US 2017
####Last modified : 28.01.2025
####ETHZ Eléonore Perret
####Code for measuring seed removal.  


##Where I stopped. I looked at a new way to model my data. 
#now I will have new models for q1, q2 and q3 where for q3 I can have the low treatment and the species one to answer a) there is difference in the removal for that specie per stand and b) there is no difference in the treatment removal. 
#I did not check for the camera data. 
#need to clean the code and prepare more plots. 

# Loading librairies ------------------------------------------------------
# install.packages("dplyr")
# install.packages("ggplot2")
#install.packages("DHARMa")
#install.packages("emmeans")
#install.packages("extrafont")

library(readxl)
library(ggplot2)
library(dplyr)
library(lme4)
library(DHARMa)
library(emmeans)
library(glmmTMB)
library(MASS)
library(extrafont)
font_import()

windowsFonts(Times=windowsFont("Times New Roman"))

# Loading the data --------------------------------------------------------
# Set the working directory
getwd()
setwd("C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_US_Manuscript/")

# List files in the main directory
list.files()

# Load datasets
seed_predation <- read.csv("SeedPredationUS/Datasets/SeedPredation_First_week.csv", sep=";") #Seed trays information
all_data_seed<- read.csv("SeedPredationUS/Datasets/seed_data.csv",sep= ";") #Removal rates informations
predators_data <- read_excel("SeedPredationUS/Datasets/camera_data.xlsx") #Camera data results

load("SeedPredationUS/Datasets/data_cleaned_2.RData") #Done after cleaning the seed data. See Seed predation results later in the script


# CAMERA TRAP DATA Information : Process_data ------------------------------------------------------------
#Because I have empty rows after the row 27 (this comes from excel), I will first delete all the rows below
seed_predation <- seed_predation %>% slice(1:27)

# SEED PREDATION: Process data -End Product (data_cleaned_2.RData) ---------------------------------------------------

# Data Cleaning 

# Correct misspelled species names
all_data_seed$Seed_sp[all_data_seed$Seed_sp == "THPLE"] <- "THPL"
# Replace 'to count' with NA in the Seeds.Remaining column
all_data_seed$Seeds.Remaining[all_data_seed$Seeds.Remaining == 'to count'] <- NA

#seeds at the beginning
seed_disposed <- c(ABAM = 5, ABLA = 20, CANO = 30, PSME = 25, THPL = 90, TSHE = 90)
all_data_seed$seed_disposed<-seed_disposed[all_data_seed$Seed_sp]

#adding weight
species_weights <- c(ABAM = 0.024, ABLA = 0.02, CANO = 0.004, PSME = 0.007, THPL = 0.001, TSHE = 0.001)
all_data_seed$seed_weight <- species_weights[all_data_seed$Seed_sp]


# Remove rows with NA values that were added in the original dataset 
data_cleaned_2 <- na.omit(all_data_seed)

data_cleaned_2$Seeds.Remaining <- as.numeric(data_cleaned_2$Seeds.Remaining)
data_cleaned_2$Week<-as.factor(data_cleaned_2$Week)
data_cleaned_2$Camera<-as.factor(data_cleaned_2$Camera)
data_cleaned_2$Treatment<-as.factor(data_cleaned_2$Treatment)


data_cleaned_2$Seed_sp <- factor(data_cleaned_2$Seed_sp,
                                 levels = c("PSME","THPL", "TSHE", "ABAM", "CANO", "ABLA"),
                                 labels = c("Pseudotsuga menziesii","Thuja plicata", "Tsuga heterophylla","Abies amabilis", "Callitropsis nootkatensis","Abies lasiocarpa"))
data_cleaned_2$Stand <- factor(data_cleaned_2$Stand,
                               levels = c("AE10", "AV06", "TO04"),
                               labels = c("High", "Mid", "Low"))


# Data treatment

data_cleaned_2$mass_placed<-data_cleaned_2$seed_disposed*data_cleaned_2$seed_weight
data_cleaned_2$mass_remaining<-data_cleaned_2$Seeds.Remaining*data_cleaned_2$seed_weight

data_cleaned_2$mass_removed <- data_cleaned_2$mass_placed-data_cleaned_2$mass_remaining

data_cleaned_2$seed_removed <- data_cleaned_2$seed_disposed-data_cleaned_2$Seeds.Remaining

# New variable 'Success' (seeds eaten)
data_cleaned_2$Success <- data_cleaned_2$seed_removed

data_cleaned_2$proportion_removed_inmass<-data_cleaned_2$mass_removed/data_cleaned_2$mass_placed

# Now 'Success' is the number of successes (seeds eaten), and 'Seeds.Placed' is the number of trials.-- BINOMIAL RESPONSE


save(data_cleaned_2, file = "SeedPredationUS/Datasets/data_cleaned_2.RData")


# PLOTS(seed removal per species, stand, treatment and elevation group)-------------------------------------------------------------------
str(data_cleaned_2)

#Overall removal
overall_removal <- data_cleaned_2 %>%
  mutate(removal_prop = Success / seed_disposed) %>% 
  summarize(mean_removal= mean(removal_prop, na.rm = TRUE))
overall_removal

#Species removal
species_removal <- data_cleaned_2 %>%
  group_by(Seed_sp)%>%
  mutate(removal_prop = Success / seed_disposed) %>% 
  summarize(mean_removal= mean(removal_prop, na.rm = TRUE))%>%
  arrange(mean_removal)
species_removal

species_removal_weight <- data_cleaned_2 %>%
  group_by(Seed_sp)%>%
  summarize(mean_weight_removal= mean(proportion_removed_inmass, na.rm = TRUE))%>%
  arrange(mean_weight_removal)
species_removal_weight

share_compare<-data_cleaned_2%>%
  group_by(Seed_sp)%>%
  summarize(seeds_removed= sum (seed_removed), mass_removed= sum(mass_removed)) %>%
  mutate(share_count= seeds_removed/sum(seeds_removed), share_mass= mass_removed/sum(mass_removed))%>%
  arrange(share_mass)
share_compare

share_compare_all<-data_cleaned_2%>%
  filter(Treatment=="All")%>%
  group_by(Seed_sp)%>%
  summarize(seeds_removed= sum (seed_removed), mass_removed= sum(mass_removed)) %>%
  mutate(share_count= seeds_removed/sum(seeds_removed), share_mass= mass_removed/sum(mass_removed))%>%
  arrange(share_mass)
share_compare_all

share_compare_low<-data_cleaned_2%>%
  filter(Treatment=="Low")%>%
  group_by(Seed_sp)%>%
  summarize(seeds_removed= sum (seed_removed), mass_removed= sum(mass_removed)) %>%
  mutate(share_count= seeds_removed/sum(seeds_removed), share_mass= mass_removed/sum(mass_removed))%>%
  arrange(share_mass)
share_compare_low

share_compare_high<-data_cleaned_2%>%
  filter(Treatment=="High")%>%
  group_by(Seed_sp)%>%
  summarize(seeds_removed= sum (seed_removed), mass_removed= sum(mass_removed)) %>%
  mutate(share_count= seeds_removed/sum(seeds_removed), share_mass= mass_removed/sum(mass_removed))%>%
  arrange(share_mass)
share_compare_high

#Stand removal
stand_removal <- data_cleaned_2 %>%
  group_by(Stand)%>%
  mutate(removal_prop = Success / seed_disposed) %>% 
  summarize(mean_removal= mean(removal_prop, na.rm = TRUE))%>%
  arrange(mean_removal)
stand_removal

species_stand_removal<- data_cleaned_2 %>%
  group_by(Stand, Seed_sp)%>%
  mutate(removal_prop = Success / seed_disposed) %>% 
  summarize(mean_removal= mean(removal_prop, na.rm = TRUE))%>%
  arrange(Seed_sp)
species_stand_removal

species_treatment_removal<- data_cleaned_2 %>%
  group_by( Seed_sp, Treatment)%>%
  mutate(removal_prop = Success / seed_disposed) %>% 
  summarize(mean_removal= mean(removal_prop, na.rm = TRUE))%>%
  arrange(Seed_sp)
species_treatment_removal

species_treatment_removal<- data_cleaned_2 %>%
  group_by( Seed_sp, Treatment, Stand)%>%
  mutate(removal_prop = Success / seed_disposed) %>% 
  summarize(mean_removal= mean(removal_prop, na.rm = TRUE))%>%
  arrange(Seed_sp)
species_treatment_removal

ggplot(species_treatment_removal, aes(x = Treatment, y = mean_removal, color = Stand, group = Stand)) +
  geom_point(size = 3) +
  geom_line() +
  facet_wrap(~Seed_sp) +
  scale_color_manual(values = c("Low" = "black", "Mid" = "grey50", "High" = "grey80")) +
  labs(
    title = "Seed Removal by Species, Treatment, and Stand",
    y = "Mean Removal Proportion",
    x = "Treatment",
    color = "Stand"
  ) +
  ylim(0, 1) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman", size = 12))

week_removal <- data_cleaned_2 %>%
  group_by(Week)%>%
  mutate(removal_prop = Success / seed_disposed) %>% 
  summarize(mean_removal= mean(removal_prop, na.rm = TRUE))%>%
  arrange(mean_removal)
week_removal

camera_removal <- data_cleaned_2 %>%
  group_by(Camera)%>%
  mutate(removal_prop = Success / seed_disposed) %>% 
  summarize(mean_removal= mean(removal_prop, na.rm = TRUE))%>%
  arrange(mean_removal)
camera_removal


#Removal per species
mean_se_data <- data_cleaned_2 %>%
  group_by(Seed_sp) %>%
  summarise(
    mean_seeds = mean(proportion_removed, na.rm = TRUE),
    se_seeds = sd(proportion_removed, na.rm = TRUE) / sqrt(n())
  )
print(mean_se_data)

# Plot
ggplot(data_cleaned_2, aes(x = Seed_sp, y = proportion_removed, fill = Seed_sp)) +
  geom_jitter(width = 0.3, alpha = 0.4, shape = 21, color = "black") +
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
  summarise(mean_seeds_week = mean(proportion_removed, na.rm = TRUE), .groups = "drop") %>%
  group_by(Stand) %>%
  summarise(
    mean_seeds = mean(mean_seeds_week, na.rm = TRUE),
    se_seeds = sd(mean_seeds_week, na.rm = TRUE) / sqrt(n())
  )
ggplot(data_cleaned_2, aes(x = Stand, y = proportion_removed, fill = Stand)) +
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
    mean_seeds = mean(proportion_removed, na.rm = TRUE),
    se_seeds = sd(proportion_removed, na.rm = TRUE) / sqrt(n())
  )
ggplot(data_cleaned_2, aes(x = Treatment, y = proportion_removed, fill = Treatment)) +
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

str(data_cleaned_2)

##**Question 2** Based on the manuscript
##WHAT ARE THE PREFERRED SEED AND IS THIS PREFERENCE CONSISTENT OVER ELEVATION? or
##WHICH SPECIES IS REMOVED THE MOST AND WHERE THE MOST?

#**Model 1***Research question : 2.	How limiting is seed predation (as measured by seed removal), and how does this vary among dominant conifer species and across elevations?
#*
#*
#Reload the data_cleaned_2 and then reorder based on the graphical results. PSME most eaten and AE10 most removal: I have done this so that those two are the reference points and when using summary I can see which ones are statistically different from my references.


model_removal_species_full<- glmmTMB(cbind(Success, seed_disposed - Success)~
                                       Stand + Seed_sp + 
                                       (1 | Camera)  + (1|Week),
                                     family = betabinomial(),   data= data_cleaned_2)

summary(model_removal_species_full)

#checking now if it would make sense to have a simpler model based on AIC score

model_removal_species_null<-glmmTMB(cbind(Success, seed_disposed - Success)~1,
                                    family = betabinomial(),   data= data_cleaned_2)

model_removal_species_simple<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand + Seed_sp ,
                                family = betabinomial(), 
                                data= data_cleaned_2)

AIC(model_removal_species_null,model_removal_species_simple)#no,  so explanatory variables create a better model fit
#anova(model_removal_species_null, model_removal_species_simple) 

model_removal_species_simple2<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand + Seed_sp 
                                        + (1|Camera) ,
                                       family = betabinomial(), 
                                       data= data_cleaned_2)

AIC(model_removal_species_simple,model_removal_species_simple2) #improved
#anova(model_removal_species_simple, model_removal_species_simple2) 


model_removal_species_simple3<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand + Seed_sp 
                                        + (1|Week) ,
                                        family = betabinomial(), 
                                        data= data_cleaned_2)
AIC(model_removal_species_simple2,model_removal_species_simple3) #imporved
#anova(model_removal_species_simple2, model_removal_species_simple3)

AIC(model_removal_species_simple3,model_removal_species_full) #both together improved the model fit as well as makes the most sense ecologically as both control for spatial and temporal heterogeinity. 

table_AIC_model_1<- AIC(model_removal_species_null, model_removal_species_simple, model_removal_species_simple2, model_removal_species_simple3, model_removal_species_full ) %>%
  arrange(AIC)

#the best model still remains the full one : 
model_removal_species<- glmmTMB(cbind(Success, seed_disposed - Success)~
                                  Stand + Seed_sp + 
                                  (1 | Camera)  + (1|Week),
                                family = betabinomial(),  data= data_cleaned_2)

##New thing
model_removal_species_treatment<- glmmTMB(cbind(Success, seed_disposed - Success)~
                                  Stand * Treatment + Seed_sp * Treatment + 
                                  (1 | Camera)  + (1|Week),
                                family = betabinomial(),  data= data_cleaned_2)

emm_weird<-emmeans(model_removal_species_treatment, ~Treatment |Seed_sp)
pairs(emm_weird)

#Simulation the residuals to check for distribution of residuals
SimOUtput<-simulateResiduals(model_removal_species)
plot(SimOUtput)
testDispersion(SimOUtput)#not coming from overdispersion
testZeroInflation(SimOUtput) #zero inflated

plotResiduals(model_removal_species, form = ~.)

summary(model_removal_species)



#Result : 
##The species are not removed the same. There is some preferences.
##The seeds are not removed the same per elevation


#POST-HOC tests
em_stand <- emmeans(model_removal_species, ~ Stand , type = "response")
em_stand_df <- as.data.frame(em_stand)
stand_contrasts <- pairs(em_stand, adjust = "tukey")
em_seeds <- emmeans(model_removal_species, ~ Seed_sp , type = "response")
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


#Plotting both
em_both <- emmeans(model_removal_species, ~ Stand + Seed_sp, type = "response")
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

#change plot according to reviews: axis font sizes, thicken barplot lines, un-italicize “spp.” (throughout the manuscript where needed). We also suggest reconsidering the colour scheme for Figure 2. The bars are very faint (thickening the lines will help) but also the colours could be improved. Yellow/orange is hard to read, and all three are unintuitive - do they represent something related to position? 

# Plot
g1<-ggplot(em_df, aes(x = Stand, y = prob, shape = Seed_sp)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0, dodge.width = 0.6),
              size = 5, alpha = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.6), width = 0.2) +
  # Add mean line per stand
  geom_hline(data = stand_means, aes(yintercept = mean_prob, linetype =  Stand, color= Stand),
             linewidth= 1.2, alpha = 0.7, show.legend = TRUE) +
  scale_linetype_manual(values = c("High" = "solid", "Mid" = "solid", "Low"= "dotted")) +
  scale_color_manual(values = c("High" = "black", "Mid" = "grey60", "Low"= "black"))+
  scale_shape_manual(values = species_shapes) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    #title = "Estimated Probability of Seed Removal by Elevation and Species",
    x = "Elevation (Stand)",
    y = "Probability of Removal",
    shape = "Seed Species"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.6),
    axis.text.x = element_text(family = "Times New Roman",size = 18, face = "bold"),
    axis.text.y = element_text(family = "Times New Roman", size = 18, face= "bold"),
    axis.title = element_text(family = "Times New Roman",size = 24, face = "bold"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "right"
  )+
  guides(shape=guide_legend(theme=theme(legend.text = element_text(face="italic", size= 12))))
plot(g1)
#To add manually, the different stand on the lines because I feel the color is not superbe
#END OF NEW PLOT REVISION SECOND DRAFT

#all
#to think maybe adding the interaction there
model_removal_species_beta_all<- glmmTMB(cbind(Success, seed_disposed-Success)~Stand + Seed_sp + (1 | Camera) +  (1|Week), family = betabinomial(), data= data_cleaned_2[data_cleaned_2$Treatment=="All",])


#Plotting both
em_both <- emmeans(model_removal_species_beta_all, ~ Stand + Seed_sp, type = "response")
all_contrasts <- pairs(em_both, adjust = "tukey")
em_df <- as.data.frame(em_both)
stand_means <- em_df %>%
  group_by(Stand) %>%
  summarise(mean_prob = mean(prob, na.rm = TRUE))
em_df <- em_df %>%
  mutate(Stand = factor(Stand, levels = c("Low", "Mid", "High")))
em_df <- em_df %>%
  mutate(Seed_sp = factor(Seed_sp, levels = c("Thuja plicata","Tsuga heterophylla","Pseudotsuga menziesii","Abies amabilis","Callitropsis nootkatensis","Abies lasiocarpa")))
gall<-ggplot(em_df, aes(x = Stand, y = prob, shape = Seed_sp)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0, dodge.width = 0.6),
              size = 5, alpha = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.6), width = 0.2) +
  # Add mean line per stand
  geom_hline(data = stand_means, aes(yintercept = mean_prob, linetype =  Stand, color= Stand),
             linewidth= 1.2, alpha = 0.7, show.legend = TRUE) +
  scale_linetype_manual(values = c("High" = "solid", "Mid" = "solid", "Low"= "dotted")) +
  scale_color_manual(values = c("High" = "black", "Mid" = "grey60", "Low"= "black"))+
  scale_shape_manual(values = species_shapes) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    #title = "Estimated Probability of Seed Removal by Elevation and Species",
    x = "Elevation (Stand)",
    y = "Probability of Removal",
    shape = "Seed Species"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.6),
    axis.text.x = element_text(family = "Times New Roman",size = 18, face = "bold"),
    axis.text.y = element_text(family = "Times New Roman", size = 18, face= "bold"),
    axis.title = element_text(family = "Times New Roman",size = 24, face = "bold"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "right"
  )+
  guides(shape=guide_legend(theme=theme(legend.text = element_text(face="italic", size= 12))))
plot(gall)

#low
model_removal_species_beta_low<- glmmTMB(cbind(Success, seed_disposed-Success)~Stand + Seed_sp + (1 | Camera) +  (1|Week),family = betabinomial(), data= data_cleaned_2[data_cleaned_2$Treatment=="Low",])
resid_low<-simulateResiduals(model_removal_species_beta_low)
plot(resid_low)
testDispersion(resid_low)
testZeroInflation(resid_low)
summary(model_removal_species_beta_low)
#Plotting both
em_both <- emmeans(model_removal_species_beta_low, ~ Stand * Seed_sp, type = "response")
all_contrasts <- pairs(em_both, adjust = "tukey")
em_df <- as.data.frame(em_both)
stand_means <- em_df %>%
  group_by(Stand) %>%
  summarise(mean_prob = mean(prob, na.rm = TRUE))
glow<-ggplot(em_df, aes(x = Stand, y = prob, shape = Seed_sp)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0, dodge.width = 0.6),
              size = 5, alpha = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.6), width = 0.2) +
  # Add mean line per stand
  geom_hline(data = stand_means, aes(yintercept = mean_prob, linetype =  Stand, color= Stand),
             linewidth= 1.2, alpha = 0.7, show.legend = TRUE) +
  scale_linetype_manual(values = c("High" = "solid", "Mid" = "solid", "Low"= "dotted")) +
  scale_color_manual(values = c("High" = "black", "Mid" = "grey60", "Low"= "black"))+
  scale_shape_manual(values = species_shapes) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    #title = "Estimated Probability of Seed Removal by Elevation and Species",
    x = "Elevation (Stand)",
    y = "Probability of Removal",
    shape = "Seed Species"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.6),
    axis.text.x = element_text(family = "Times New Roman",size = 18, face = "bold"),
    axis.text.y = element_text(family = "Times New Roman", size = 18, face= "bold"),
    axis.title = element_text(family = "Times New Roman",size = 24, face = "bold"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "right"
  )+
  guides(shape=guide_legend(theme=theme(legend.text = element_text(face="italic", size= 12))))
plot(glow)


#high
model_removal_species_beta_high<- glmmTMB(cbind(Success, seed_disposed-Success)~Stand * Seed_sp + (1 | Camera) +  (1|Week),family = betabinomial(), data= data_cleaned_2[data_cleaned_2$Treatment=="High",])
summary(model_removal_species_beta_high)

resid_high<-simulateResiduals(model_removal_species_beta_high)
plot(resid_high)
testDispersion(resid_high)
testZeroInflation(resid_high)
#Plotting both
em_both <- emmeans(model_removal_species_beta_high, ~ Stand + Seed_sp, type = "response")
all_contrasts <- pairs(em_both, adjust = "tukey")
em_df <- as.data.frame(em_both)
stand_means <- em_df %>%
  group_by(Stand) %>%
  summarise(mean_prob = mean(prob, na.rm = TRUE))

ghigh<-ggplot(em_df, aes(x = Stand, y = prob, color = Stand, shape = Seed_sp)) +
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
    title = "Estimated Probability of Seed Removal by Elevation and Species Elevation Group",
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
plot(ghigh)


#stopped here (where I added the interaction in the treatments : To think do I need to separate the treatments like this? is it a similar analysis?) but what about the treatments? Now I have with all species and all treatment (the first one), then I tested when we look at only the all treatmeent, is the pattern similar? what about the other treatments (did that too) but is it correct? Also should I look into which treatment was preferred and at each elevation. For this would be *Treatement as explanatory variable. But what I did before is basically the elevation group I think. No because the other one is depending on if there are in the all or just single treatment. So what did I do with the low/high separation?I checked in the low treatment which species is prefered and if this varies by elevation. So I think my last one but still not testing if there are overall difference in removal by treatment. to think about then. 








##**Question 3**
##IS THIS REMOVAL DEPENDENT OF HAVING CONSPECIFIC AROUND OR NOT?

#**Model 2***Research question 3.	Does seed community composition influence seed removal? Specifically, are seeds more or less likely to be removed when presented in novel versus familiar seed communities??

#Checking by species group (low vs high species)
#Using data_cleaned_2_grouped (see Plot section)


#ELEVATION GROUP
#Checking by species group (low vs high species)
data_cleaned_2_grouped <- data_cleaned_2 %>%
  mutate(Elevation_group = case_when(
    Seed_sp %in% c("Abies lasiocarpa", "Abies amabilis", "Callitropsis nootkatensis") ~ "High",
    Seed_sp %in% c("Thuja plicata", "Tsuga heterophylla", "Pseudotsuga menziesii") ~ "Low",
    TRUE ~ NA_character_
  ))

data_cleaned_2_grouped$Elevation_group <- factor (data_cleaned_2_grouped$Elevation_group, levels = c("Low", "High"))

str(data_cleaned_2_grouped)

#Are species more removed when they are in their naturally occuring range?

#Building my model
model_elevation_null <- glmmTMB(
  cbind(Success, seed_disposed - Success) ~ 1 ,
  family = betabinomial,
  data = data_cleaned_2_grouped
)

model_elevation_simple <- glmmTMB(
  cbind(Success, seed_disposed - Success) ~ Elevation_group + Stand  ,
  family = betabinomial(),
  data = data_cleaned_2_grouped
)

model_elevation_simple_int <- glmmTMB(
  cbind(Success, seed_disposed - Success) ~ Elevation_group * Stand  ,
  family = betabinomial(),
  data = data_cleaned_2_grouped
)

AIC(model_elevation_null, model_elevation_simple) #model with explanatory variable has a better fit
#anova(model_elevation_null, model_elevation_simple)
AIC(model_elevation_null, model_elevation_simple_int)#the interaction also has a better fit, which makes sense as we could previously see in the plots that for each sites, PSME and TSHE were more removed which are the low elevation species
#so we keep the interaction

model_elevation_simple2 <- glmmTMB(
  cbind(Success, seed_disposed - Success) ~ Elevation_group * Stand +
    (1 | Seed_sp),
  family = betabinomial (),
  data = data_cleaned_2_grouped
)

AIC(model_elevation_simple,model_elevation_simple2) #adding species as random variable improved hte model fit which is expected again as above

model_elevation_simple3 <- glmmTMB(
  cbind(Success, seed_disposed - Success) ~ Elevation_group * Stand +
    (1 | Seed_sp) + (1|Week),
  family = betabinomial(),
  data = data_cleaned_2_grouped
)

AIC(model_elevation_simple2,model_elevation_simple3) #adding week also improved the model fit

model_elevation_simple4 <- glmmTMB(
  cbind(Success, seed_disposed - Success) ~ Elevation_group * Stand +
     (1|Week) + (1|Camera) ,
  family = betabinomial(),
  data = data_cleaned_2_grouped
)

AIC(model_elevation_simple3,model_elevation_simple4)#interestingly removinf the species improved the model fit

model_elevation_simple5 <- glmmTMB(
  cbind(Success, seed_disposed - Success) ~ Elevation_group * Stand +
    (1|Seed_sp) + (1|Camera) ,
  family = betabinomial(),
  data = data_cleaned_2_grouped
)

AIC(model_elevation_simple5,model_elevation_simple4)

model_elevation_full <- glmmTMB(
  cbind(Success, seed_disposed - Success) ~ Elevation_group * Stand +
    (1 | Seed_sp) + (1|Week) + (1|Camera) ,
  family = betabinomial(),
  data = data_cleaned_2_grouped
)

AIC(model_elevation_simple4,model_elevation_full);AIC(model_elevation_simple3,model_elevation_full);AIC(model_elevation_simple5,model_elevation_full)#this shows that the best model is still the full model



#best model based on AIC scoring is the full one, but does it fit?

sim_res <- simulateResiduals(model_elevation_full)
plot(sim_res)#looks good
testDispersion(sim_res)
testZeroInflation(sim_res)
plotResiduals(model_elevation_full, form= ~.)

summary(model_elevation_full)



# POST-HOC Test
em2 <- emmeans(model_elevation_full, ~ Elevation_group * Stand, type = "response")
pairs(em2, by = "Stand")
pairs(em2, by = "Elevation_group")
pairs(em2)
em_df2 <- as.data.frame(em2)

em_df2$Stand <- factor (em_df2$Stand, levels = c("Low", "Mid","High"))



# Plot of removal probability by stand and elevation group. 
ggplot(em_df2, aes(x = as.factor(Stand), y = prob, color = as.factor(Elevation_group), group = as.factor(Elevation_group))) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_line(position = position_dodge(width = 0.3), size = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, position = position_dodge(width = 0.3)) +
  scale_color_manual(values = c("Low" = "black", "High" = "grey")) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1),
    labels = scales::percent_format(accuracy = 10))+
  labs(
    title = "Predicted Seed Removal Probability by Elevation Group and Stand",
    y = "Predicted Probability of Seed Removal",
    x = "Forest Stand Elevation",
    color = "Seed Elevation Group"
  ) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman",size = 14))

#Checking between elevation group is there is a statistical differemce
em_elev <- emmeans(model_elevation, ~ Elevation_group, type = "response")
pairs(em_elev)#There is a statistical difference between the groups. The group Low in general is more removed than the group high

em_stand <- emmeans(model_elevation, ~ Stand, type = "response")
pairs(em_stand)
#Same results as the previous model (model 1: Question2)



##**Question 3** Same question as before
##Are species more removed when they are around species that are naturally present in their environement?
#In other words: Is there more removal when the treatment is all than when it is just the high/low treatment?


#**Model 3***Research question 3.	Does seed community composition influence seed removal? Specifically, are seeds more or less likely to be removed when presented in novel versus familiar seed communities??

#New analysis
#ABAM
abam<-data_cleaned_2%>%
  filter(Seed_sp=="Abies amabilis")
mabam_null<- glmmTMB(cbind(Success, seed_disposed - Success) ~ 1,
  family = betabinomial(),
  data = abam)
mabam_simple<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment + Stand,
                     family = betabinomial(),
                     data = abam)
AIC(mabam_null,mabam_simple)#adding explanatory variable improves the model
mabam_simple_int<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment * Stand,
                       family = betabinomial(),
                       data = abam)
AIC(mabam_simple,mabam_simple_int)#the interaction doesn't improve the model fit
mabam_simple2<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment + Stand + (1|Week),
                       family = betabinomial(),
                       data = abam)
AIC(mabam_simple,mabam_simple2)
anova(mabam_simple,mabam_simple2) #not better
mabam_simple3<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment + Stand + (1|Camera),
                        family = betabinomial(),
                        data = abam)
AIC(mabam_simple3,mabam_simple)# did not improve the model
anova(mabam_simple3,mabam_simple)
mabam_full<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment + Stand + (1|Week) + (1|Camera),
                        family = betabinomial(),
                        data = abam)
AIC(mabam_full,mabam_simple)#best model is full
#checking model
resid_abam<-simulateResiduals(mabam_simple)
plot(resid_abam)
testOverdispersion(resid_abam) #there is some underdispersion but looks relatively ok.
testZeroInflation(resid_abam)
summary(mabam_full)

#ABLA
abla<-data_cleaned_2%>%
  filter(Seed_sp=="Abies lasiocarpa")
mabla_null<- glmmTMB(cbind(Success, seed_disposed - Success) ~ 1,
                     family = betabinomial(),
                     data = abla)
mabla_simple<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment + Stand,
                       family = betabinomial(),
                       data = abla)
AIC(mabla_null,mabla_simple)#adding explanatory variable does not improve the model
mabla_simple_int<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment * Stand,
                           family = betabinomial(),
                           data = abla)
AIC(mabla_null,mabla_simple_int)#the interaction doesn't improve the model fit
mabla_simple2<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment + Stand + (1|Week),
                        family = betabinomial(),
                        data = abla)
AIC(mabla_null,mabla_simple2) #better
mabla_simple3<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment + Stand + (1|Camera),
                        family = betabinomial(),
                        data = abla)
AIC(mabla_simple3,mabla_simple2)# did not improve the model
anova(mabam_simple3,mabam_simple)
mabla_full<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment + Stand + (1|Week) + (1|Camera),
                     family = betabinomial(),
                     data = abla)
AIC(mabla_full,mabla_simple2)#best model is 2 but not statistically different so lets go with the most simple model. 
anova(mabla_full,mabla_simple2)
#checking model
resid_abla<-simulateResiduals(mabla_simple2)
plot(resid_abla)
testOverdispersion(resid_abla) #there is some underdispersion but looks relatively ok.
testZeroInflation(resid_abla)
summary(mabla_simple2)


#CANO
cano<-data_cleaned_2%>%
  filter(Seed_sp=="Callitropsis nootkatensis")
m_null<- glmmTMB(cbind(Success, seed_disposed - Success) ~ 1,
                     family = betabinomial(),
                     data = cano)
m_simple<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment + Stand,
                       family = betabinomial(),
                       data = cano)
AIC(m_null,m_simple)#adding explanatory variable improve the model
m_simple_int<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment * Stand,
                           family = betabinomial(),
                           data = cano)
AIC(m_simple,m_simple_int)#the interaction doesn't improve the model fit
m_simple2<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment + Stand + (1|Week),
                        family = betabinomial(),
                        data = cano)
AIC(m_simple,m_simple2) #better
m_simple3<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment + Stand + (1|Camera),
                        family = betabinomial(),
                        data = cano)
AIC(m_simple3,m_simple2)# did not improve the model
anova(m_simple3,m_simple2)
m_full<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment + Stand + (1|Week) + (1|Camera),
                     family = betabinomial(),
                     data = cano)
AIC(m_full,m_simple2)#best model is 2 but not statistically different so lets go with the most simple model.
anova(m_full,m_simple2)#not statistically significant so I will take the most simple model

mcano_simple2<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment + Stand + (1|Week),
                    family = betabinomial(),
                    data = cano)

#checking model
resid_cano<-simulateResiduals(mcano_simple2)
plot(resid_cano)
testOverdispersion(resid_cano) #there is some underdispersion but looks relatively ok.
testZeroInflation(resid_cano)
summary(mcano_simple2)



#PSME
psme<-data_cleaned_2%>%
  filter(Seed_sp=="Pseudotsuga menziesii")
m_null<- glmmTMB(cbind(Success, seed_disposed - Success) ~ 1,
                 family = betabinomial(),
                 data = psme)
m_simple<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment + Stand,
                   family = betabinomial(),
                   data = psme)
AIC(m_null,m_simple)#adding explanatory variable improve the model
m_simple_int<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment * Stand,
                       family = betabinomial(),
                       data = psme)
AIC(m_simple,m_simple_int)#the interaction doesn't improve the model fit
m_simple2<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment + Stand + (1|Week),
                    family = betabinomial(),
                    data = psme)#doesn't work, there is probably not enough differneces between weeks 
m_simple3<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment + Stand + (1|Camera),
                    family = betabinomial(),
                    data = psme)
AIC(m_simple,m_simple3)# did not improve the model
anova(m_simple,m_simple3)#not statistically significant
m_full<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment + Stand + (1|Week) + (1|Camera),
                 family = betabinomial(),
                 data = psme)#doesn't work either

anova(m_simple,m_simple3)
#as not significant, I will take most simple model

mpsme_simple<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment + Stand,
                   family = betabinomial(),
                   data = psme)
mpsme_full<-glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment + Stand + (1|Week) + (1|Camera),
        family = betabinomial(),
        data = psme)#doesn't work either

#checking model
resid_psme<-simulateResiduals(mpsme_simple)
plot(resid_psme)
testOverdispersion(resid_psme) #there is some underdispersion but looks relatively ok.
testZeroInflation(resid_psme)
summary(mpsme_simple)
summary(mpsme_full)
#this does make sense as it was constantly removed no matter the week the stand or the camera location



#TSHE
tshe<-data_cleaned_2%>%
  filter(Seed_sp=="Tsuga heterophylla")
m_null<- glmmTMB(cbind(Success, seed_disposed - Success) ~ 1,
                 family = betabinomial(),
                 data = tshe)
m_simple<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment + Stand,
                   family = betabinomial(),
                   data = tshe)
AIC(m_null,m_simple)#adding explanatory variable improve the model
anova(m_null,m_simple)#but not statistically significant
m_simple_int<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment * Stand,
                       family = betabinomial(),
                       data = tshe)
AIC(m_simple,m_simple_int)#the interaction doesn't improve the model fit
m_simple2<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment + Stand + (1|Week),
                    family = betabinomial(),
                    data = tshe)
AIC(m_simple,m_simple2) #doesn't improve fit
m_simple3<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment + Stand + (1|Camera),
                    family = betabinomial(),
                    data = tshe)
AIC(m_simple3,m_null)# almost improved the model
anova(m_simple3,m_null)# not statistically significant
m_full<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment + Stand + (1|Week) + (1|Camera),
                 family = betabinomial(),
                 data = tshe)
AIC(m_full,m_simple3)
anova(m_full,m_simple3)#not statistically significant so I will take the most simple model

AIC(m_)
mtshe_full<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment + Stand + (1|Week) + (1|Camera),
                 family = betabinomial(),
                 data = tshe)

#checking model
resid_tshe<-simulateResiduals(mtshe_full)
plot(resid_tshe)
testOverdispersion(resid_tshe) #there is some underdispersion but looks relatively ok.
testZeroInflation(resid_tshe)
summary(mtshe_full)




#THPL
thpl<-data_cleaned_2%>%
  filter(Seed_sp=="Thuja plicata")
m_null<- glmmTMB(cbind(Success, seed_disposed - Success) ~ 1,
                 family = betabinomial(),
                 data = thpl)
m_simple<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment + Stand,
                   family = betabinomial(),
                   data = thpl)
AIC(m_null,m_simple)#adding explanatory variable does  not improve the model
m_simple_int<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment * Stand,
                       family = betabinomial(),
                       data = thpl)
AIC(m_null,m_simple_int)#the interaction doesn't improve the model fit
m_simple2<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment + Stand + (1|Week),
                    family = betabinomial(),
                    data = thpl)
AIC(m_null,m_simple2) #better
anova(m_null,m_simple2)#significant
m_simple3<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment + Stand + (1|Camera),
                    family = betabinomial(),
                    data = thpl)
AIC(m_simple3,m_simple2)# did not improve the model
anova(m_simple3,m_simple2)
m_full<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment + Stand + (1|Week) + (1|Camera),
                 family = betabinomial(),
                 data = thpl)
AIC(m_full,m_simple2)#best model is 2 but not statistically different so lets go with the most simple model.
anova(m_full,m_simple2)#not statistically significant so I will take the most simple model

mthpl_simple2<- glmmTMB(cbind(Success, seed_disposed - Success) ~ Treatment + Stand + (1|Week),
                        family = betabinomial(),
                        data = thpl)

#checking model
resid_thpl<-simulateResiduals(m_full)
plot(resid_thpl)
testOverdispersion(resid_thpl) #there is some underdispersion but looks relatively ok.
testZeroInflation(resid_thpl)
summary(mthpl_simple2)







data_subset_low <- data_cleaned_2 %>% 
  filter(!(Seed_sp %in% c("Abies amabilis", "Callitropsis nootkatensis","Abies lasiocarpa")))


m_null<- glmmTMB(cbind(Success, seed_disposed - Success) ~ 1,
                 family = betabinomial(),
                 data = data_subset_low)

m_simple<-glmmTMB(
  cbind(Success, seed_disposed - Success) ~ Treatment + Seed_sp + Stand,
  family = betabinomial(),
  data = data_subset_low
)

m_simple_int<-glmmTMB(
  cbind(Success, seed_disposed - Success) ~ Treatment * Seed_sp + Stand ,
  family = betabinomial(),
  data = data_subset_low
)

summary(m_simple_int)

AIC(m_null,m_simple); AIC(m_simple,m_simple_int)
anova(m_simple,m_simple_int)

m_simple_2 <- glmmTMB(
  cbind(Success, seed_disposed - Success) ~ Treatment + Seed_sp + Stand +
    (1 | Camera),
  family = betabinomial(),
  data = data_subset_low
)

AIC(m_simple,m_simple_2)

m_simple_3 <- glmmTMB(
  cbind(Success, seed_disposed - Success) ~ Treatment + Seed_sp + Stand +
    + (1 | Week),
  family = betabinomial(),
  data = data_subset_low
)

AIC(m_simple_3,m_simple_2)

m_all <- glmmTMB(
  cbind(Success, seed_disposed - Success) ~ Treatment + Seed_sp + Stand +
    (1 | Camera) + (1 | Week),
  family = betabinomial(),
  data = data_subset_low
)

AIC(m_all,m_simple_2)

m_simple_int2 <- glmmTMB(
  cbind(Success, seed_disposed - Success) ~ Treatment * Seed_sp + Stand +
    (1 | Camera),
  family = betabinomial(),
  data = data_subset_low
)

AIC(m_simple_int,m_simple_int2)

m_simple_int3 <- glmmTMB(
  cbind(Success, seed_disposed - Success) ~ Treatment * Seed_sp + Stand +
     + (1 | Week),
  family = betabinomial(),
  data = data_subset_low
)

AIC(m_simple_int3,m_simple_int2)

m_all_int <- glmmTMB(
  cbind(Success, seed_disposed - Success) ~ Treatment * Seed_sp + Stand +
    (1 | Camera) + (1 | Week),
  family = betabinomial(),
  data = data_subset_low
)

AIC(m_all_int,m_simple_int2)

resid<-simulateResiduals(m_all)
plot(resid)
testOverdispersion(resid)
testZeroInflation(resid)

























em3 <- emmeans(model_community, ~ Treatment * Seed_sp + Stand, type = "response")
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
em_species <- emmeans(model_community, ~ Seed_sp, type = "response")
pairs(em_species)#There is a statistical difference between all seed species. 

em_treatment <- emmeans(model_community, ~ Treatment, type = "response")
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

glmer_modelOLRE_New4 <- glmer(
  cbind(Success, Seeds.Placed - Success) ~ Treatment * Seed_sp +
    (1 | Camera) + (1 | ObsID) + (1|Week) ,
  family = binomial,
  data = ata_subset_2,
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



AIC(m_null, m1)

m2<- glmer(cbind(Success, Seeds.Placed - Success) ~Treatment *Stand + (1|Week), family = binomial, data=specie)

AIC(m_null, m2)

m_full <- glmer(cbind(Success, Seeds.Placed - Success) ~Treatment *Stand + (1|Week) + (1|Camera), family = binomial, data=specie)

AIC(m2, m_full)

resid_specie<-simulateResiduals(m_full)
plot(resid_specie)
testOverdispersion(resid_specie)
testZeroInflation(resid_specie)
#both overdispersed and zero inflated could change the model to some more appropritate distribution

m_fullbeta <- glmmTMB(cbind(Success, Seeds.Placed - Success) ~Treatment *Stand + (1|Week) + (1|Camera), family = betabinomial, data=specie)

AIC(m_full, m_fullbeta)

resid_beta<-simulateResiduals(m_fullbeta)
plot(resid_beta)
testOverdispersion(resid_beta)
testZeroInflation(resid_beta)




# New analysis ------------------------------------------------------------

species_range<-list(
  "Abies lasiocarpa" = c("High") ,        
  "Abies amabilis"  = c("Low", "Mid","High") ,          
  "Callitropsis nootkatensis" = c("Mid","High") ,
  "Pseudotsuga menziesii"   = c("Low","Mid"),  
  "Tsuga heterophylla"  = c("Low","Mid")  ,     
  "Thuja plicata"       = c("Low","Mid")  
)

data_cleaned_2$in_range<-mapply(function(sp,st) st %in% species_range[[sp]], 
                                as.character(data_cleaned_2$Seed_sp),
                                as.character(data_cleaned_2$Stand))

model_removal_species_range<- glmmTMB(cbind(Success, seed_disposed - Success)~
                                       Stand + Seed_sp * in_range +
                                       (1 | Camera)  + (1|Week),
                                     family = betabinomial(),   data= data_cleaned_2)

summary(model_removal_species_range)


# New analysis : Treatment effect -----------------------------------------

data_cleaned_2$context<- ifelse(data_cleaned_2$Treatment=="All", "Mixed", "Same-range only")
data_cleaned_2$context <-factor(data_cleaned_2$context, levels = c("Same-range only", "Mixed"))

model_removal_treatment_effect<- glmmTMB(cbind(Success, seed_disposed - Success)~
                                        Stand + Seed_sp * context +
                                        (1 | Camera)  + (1|Week),
                                      family = betabinomial(),   data= data_cleaned_2)

summary(model_removal_treatment_effect)

model_removal_treatment_effect_2<- glmmTMB(cbind(Success, seed_disposed - Success)~
                                           Stand + Seed_sp + context +
                                           (1 | Camera)  + (1|Week),
                                         family = betabinomial(),   data= data_cleaned_2)

summary(model_removal_treatment_effect_2)
AIC(model_removal_treatment_effect,model_removal_treatment_effect_2)




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
model_pois <- glmmTMB(detections_per_week ~ Stand + Species_ID,
                  data = predator_density_site_species_standardized,
                  family = poisson())
#Doesn't work
#Takes into account the fact that there is not the same amount of week per site. 
model_pois_Week<-glmmTMB(predator_detections ~ Stand + Species_ID,
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

model_nb <- glmmTMB(predator_detections ~ Stand + Species_ID + offset(log(Weeks_sampled)),
                  family = nbinom2(),
                   data = predator_density_site_species_standardized)
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



