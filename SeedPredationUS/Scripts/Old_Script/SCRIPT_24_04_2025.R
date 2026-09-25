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
library(dplyr)
library(ggplot2)
library(tidyr)
library(car)
library(lme4)
library(ggpubr)  

# Loading the data --------------------------------------------------------
# Set the working directory
getwd()
setwd("C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets")

# List files in the main directory
list.files()

# Load datasets
seed_predation <- read.csv("Datasets/SeedPredation_First_week.csv", sep=";")
all_data_seed<- read.csv("Datasets/seed_data.csv",sep= ";")


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
data_cleaned_2$Success <- data_cleaned_2$seeds_eaten

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
ggplot(data_cleaned_2, aes(x = Seed_sp, y = seeds, color = Treatment, fill = Treatment)) +
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
ggplot(data_cleaned_2, aes(x = Treatment, y = seeds, color = Stand, fill = Stand)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.6),
              alpha = 0.5, shape = 20, size = 2) +
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(width = 0.6),
               shape = 23, size = 5, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               position = position_dodge(width = 0.6), width = 0.2) +
  scale_fill_manual(values = c("TO04" = "darkred", "AV06" = "orange", "AE10" = "lightblue")) +
  scale_color_manual(values = c("TO04" = "darkred", "AV06" = "orange", "AE10" = "lightblue")) +
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
data_cleaned_2$Treatment <- factor(data_cleaned_2$Treatment, levels = c("All", "Low", "High"))

glmer_model <- glmer(cbind(Success, Seeds.Placed - Success) ~ Stand * Treatment + (1|Camera) + (1|Seed_sp), 
                 data = data_cleaned_2, 
                 family = binomial(link = "logit"))
plot(glmer_model)
summary(glmer_model)
vif(glmer_model)  # Check for multicollinearity
#No multicollinearity

#install.packages("DHARMa")
library(DHARMa)
# Simulate residuals
sim_res <- simulateResiduals(fittedModel = glmer_model, plot = TRUE)
# Simulate residuals
sim_res <- simulateResiduals(glmer_model)
# Check dispersion
testDispersion(sim_res)
#I think my model is overdispersed

# Quick test to see if there is overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq / rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq, ratio=prat, rdf=rdf, p=pval)
}
overdisp_fun(glmer_model)
#Yes there is.


#Using an OLRE for removing the overdispersion
#Because I have overdipersion (more variability in my data than the model expects)
data_cleaned_2$ObsID <- factor(1:nrow(data_cleaned_2))

#Testing new model
glmer_modelOLRE <- glmer(
  cbind(Success, Seeds.Placed - Success) ~ Stand * Treatment + (1|Seed_sp) +
    (1 | Camera) + (1 | ObsID),
  family = binomial,
  data = data_cleaned_2
)
#Doesn't converge = not able to find the perfect estimate parameter: Need to do something
#Using a optimizer helps R take its time to find the estimate and help the model to find estimate parameters. 
glmer_modelOLRE <- glmer(
  cbind(Success, Seeds.Placed - Success) ~ Stand * Treatment + 
    (1 | Seed_sp) + (1 | Camera) + (1 | ObsID),
  family = binomial,
  data = data_cleaned_2,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
overdisp_fun(glmer_modelOLRE)
summary(glmer_modelOLRE)
#I removed the overdispersion but now there is no more significant effect of the interaction between stand and treatment. 

data_cleaned_2$Seed_sp <- factor(data_cleaned_2$Seed_sp, levels = c("PSME", "THPL", "TSHE","ABAM","ABLA","CANO"))

glmer_modelOLRE_2 <- glmer(
  cbind(Success, Seeds.Placed - Success) ~ Stand + Seed_sp + 
    (1 | ObsID),
  family = binomial,
  data = data_cleaned_2,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e6))
)
summary(glmer_modelOLRE_2)
data_cleaned_2$Treatment <- factor(data_cleaned_2$Treatment, levels = c("All", "Low", "High"))







glmer_model_removal_rate <- glmer(
  cbind(Success, Seeds.Placed - Success) ~ Seed_sp + Treatment + Stand + 
    (1 | Camera) + (1 | ObsID) + (1 | Week),
  family = binomial,
  data = data_cleaned_2,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(glmer_model_removal_rate)



# Look at the number of observations in each category
table(data_cleaned_2$Seed_sp)
table(data_cleaned_2$Stand)
table(data_cleaned_2$Treatment)

data_cleaned_2$


glmer_model_removal_rate <- glmer(
  cbind(Success, Seeds.Placed - Success) ~ Seed_sp *Stand + 
    (1 | Camera) + (1 | ObsID) + (1 | Week),
  family = binomial,
  data = data_cleaned_2,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(glmer_model_removal_rate)

sim_res <- simulateResiduals(fittedModel = glmer_model_removal_rate, plot = TRUE)


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

# MODEL PER SPECIES -------------------------------------------------------
data_cleaned_2_grouped$ObsID <- factor(1:nrow(data_cleaned_2_grouped))
glmer_model_group <- glmer(cbind(Success, Seeds.Placed - Success) ~ Elevation_group + Stand  + (1|Camera) + (1|Week), 
                     data = data_cleaned_2_grouped, 
                     family = binomial(link = "logit"))
plot(glmer_model_group)
summary(glmer_model_group)
# ABAM --------------------------------------------------------------------
ABAM<-data_cleaned_2%>%
  filter(Seed_sp=="ABAM")
ABAM$ObsID <- factor(1:nrow(ABAM))

glmer_ABAM <- glmer(
  cbind(Success, Seeds.Placed - Success) ~ Stand * Treatment + 
    (1 | Camera) + (1 | ObsID),
  family = binomial,
  data = ABAM,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))
)
summary (glmer_ABAM)
sim_res <- simulateResiduals(fittedModel = glmer_ABAM, plot = TRUE)

# ABLA --------------------------------------------------------------------
ABLA<-data_cleaned_2%>%
  filter(Seed_sp=="ABLA")
ABLA$ObsID <- factor(1:nrow(ABLA))

summary(model1ABLA)
plot(model1ABLA)
sim_res <- simulateResiduals(fittedModel = model1ABLA, plot = TRUE)
# CANO --------------------------------------------------------------------
CANO<-data_cleaned_2%>%
  filter(Seed_sp=="CANO")
CANO$ObsID <- factor(1:nrow(CANO))
model1CANO <- glmer(cbind(Success, Seeds.Placed - Success) ~ 1+ (1|ObsID), 
                  data = CANO, 
                  family = binomial(link = "logit"))
model2CANO <- glmer(cbind(Success, Seeds.Placed - Success) ~ Stand+ (1|ObsID), 
                  data = CANO, 
                  family = binomial(link = "logit"))
model3CANO <- glmer(cbind(Success, Seeds.Placed - Success) ~ Treatment+ (1|ObsID), 
                  data = CANO, 
                  family = binomial(link = "logit"))
model4CANO <- glmer(cbind(Success, Seeds.Placed - Success) ~ Stand + Treatment+ (1|ObsID), data = CANO, 
                  family = binomial(link = "logit"))
model5CANO <- glmer(cbind(Success, Seeds.Placed - Success) ~ Stand * Treatment+ (1|ObsID), data = CANO, 
                  family = binomial(link = "logit"))

# Listing all my models
models <- list(
  model   = model1CANO,
  model2  = model2CANO,
  model3  = model3CANO,
  model4  = model4CANO,
  model5  = model5CANO
)

# Compute AIC for each model
aic_scores <- sapply(models, AIC)
# Print AIC values for all models
print(aic_scores)

# Identify the model with the lowest AIC
best_model_name <- names(which.min(aic_scores))
best_aic <- min(aic_scores)

plot(model2CANO)
summary(model4CANO)
sim_res <- simulateResiduals(fittedModel = model2CANO, plot = TRUE)

# PSME --------------------------------------------------------------------
PSME<-data_cleaned_2%>%
  filter(Seed_sp=="PSME")
PSME$ObsID <- factor(1:nrow(PSME))
glmer_PSME <- glmer(
  cbind(Success, Seeds.Placed - Success) ~ Stand * Treatment + 
    (1 | Camera) + (1 | ObsID),
  family = binomial,
  data = PSME,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))
)


# TSHE --------------------------------------------------------------------

TSHE<-data_cleaned_2%>%
  filter(Seed_sp=="TSHE")
TSHE$ObsID <- factor(1:nrow(TSHE))
model1TSHE <- glmer(cbind(Success, Seeds.Placed - Success) ~ 1+ (1|ObsID), 
                  data = TSHE, 
                  family = binomial(link = "logit"))
model2TSHE <- glmer(cbind(Success, Seeds.Placed - Success) ~ Stand+ (1|ObsID), 
                  data = TSHE, 
                  family = binomial(link = "logit"))
model3TSHE <- glmer(cbind(Success, Seeds.Placed - Success) ~ Treatment+ (1|ObsID), 
                  data = TSHE, 
                  family = binomial(link = "logit"))
model4TSHE <- glmer(cbind(Success, Seeds.Placed - Success) ~ Stand + Treatment+ (1|ObsID), data = TSHE, 
                  family = binomial(link = "logit"))
model5TSHE <- glmer(cbind(Success, Seeds.Placed - Success) ~ Stand * Treatment+ (1|ObsID), data = TSHE, 
                  family = binomial(link = "logit"))

# Listing all my models
models <- list(
  model   = model1TSHE,
  model2  = model2TSHE,
  model3  = model3TSHE,
  model4  = model4TSHE,
  model5  = model5TSHE
)

# Compute AIC for each model
aic_scores <- sapply(models, AIC)
# Print AIC values for all models
print(aic_scores)

plot(model4TSHE)
summary(model4TSHE)
sim_res <- simulateResiduals(fittedModel = model5TSHE, plot = TRUE)
#mODEL 4 IS THE BEST BUT i FEEL LIKE MODEL 5 IS AS GOOD...

# THPL --------------------------------------------------------------------

THPL<-data_cleaned_2%>%
  filter(Seed_sp=="THPL")
THPL$ObsID <- factor(1:nrow(THPL))
model1THPL <- glmer(cbind(Success, Seeds.Placed - Success) ~ 1+ (1|ObsID), 
                  data = THPL, 
                  family = binomial(link = "logit"))
model2THPL <- glmer(cbind(Success, Seeds.Placed - Success) ~ Stand+ (1|ObsID), 
                  data = THPL, 
                  family = binomial(link = "logit"))
model3THPL <- glmer(cbind(Success, Seeds.Placed - Success) ~ Treatment+ (1|ObsID), 
                  data = THPL, 
                  family = binomial(link = "logit"))
model4THPL <- glmer(cbind(Success, Seeds.Placed - Success) ~ Stand + Treatment+ (1|ObsID), data = THPL, 
                  family = binomial(link = "logit"))
model5THPL <- glmer(cbind(Success, Seeds.Placed - Success) ~ Stand * Treatment+ (1|ObsID), data = THPL, 
                  family = binomial(link = "logit"))

# Listing all my models
models <- list(
  model   = model1THPL,
  model2  = model2THPL,
  model3  = model3THPL,
  model4  = model4THPL,
  model5  = model5THPL
)

# Compute AIC for each model
aic_scores <- sapply(models, AIC)
# Print AIC values for all models
print(aic_scores)

plot(model1THPL)
summary(model1THPL)
sim_res <- simulateResiduals(fittedModel = model1THPL, plot = TRUE)


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

# Saving dataset ----------------------------------------------------------------

# Save the cleaned data as an RData file
save(data_cleaned_2, file = "C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/data_cleaned_2.RData")


