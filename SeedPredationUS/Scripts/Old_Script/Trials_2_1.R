
install.packages("GGally")

library(dplyr)
library(corrplot)
library(car)
library(GGally)
library(lme4)

# Loading the data --------------------------------------------------------
getwd()
load("C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/data_merged_6.RData")

str(data_merged_6)
#1. First I want to make sure that all my variables are in the correct format
#2. I want to make sure that my explanatory variables (numerical) are standartized 
#3. I want to see if my response variable follow a normal distribution
#4. I want to see if my response variable for each site, each specie and each treatment follow a normal distribution
#5. I want to see if my explanatory variables follow a normal distribution (the numerical ones)
#6. Based on point 4; if any shows a normal distribution, I can do an anova
#7. I want to explore the collinearity between my explanatory variables (numerical with correlation matrix and factors using available tools ie. orthogonal contrast).
#8. Once I have defined all of this I can start creating models
#9. I first want to identify which explanatory variables explains the best the variation of the seed removal rate overall. The most probable model will be a binomial glmer model.
#10. I want to do the same that in point 9 but for each seed sp. 
#11. I want to do the same that in point 9 but for each site
#12. I want to do the same that in point 9 but for each treatment.


# Data formatting-------------------------------------------------------------------------
#1. Data formatting
#Numerical variables
data_merged_6$mean_dbh_y <- as.numeric(gsub(",", ".", data_merged_6$mean_dbh_y))
data_merged_6$Seeds.Remaining <- as.numeric(data_merged_6$Seeds.Remaining)
data_merged_6$removal_per_all <- as.numeric(data_merged_6$removal_per_all)
data_merged_6$seeds_eaten <- as.numeric(data_merged_6$seeds_eaten)
data_merged_6$seeds_disposed <-as.numeric(data_merged_6$seeds_disposed)
data_merged_6$species_diversity_camera<- as.numeric(data_merged_6$species_diversity_camera)
data_merged_6$number_of_trees<- as.integer(data_merged_6$number_of_trees)
data_merged_6$Shannon_Index_5m<- as.numeric(data_merged_6$Shannon_Index_5m)
#Factor variables
# Convert non-numeric response variables to factors
data_merged_6$Stand <- factor(data_merged_6$Stand)
data_merged_6$Seed_sp <- factor(data_merged_6$Seed_sp)
data_merged_6$Treatment <- factor(data_merged_6$Treatment)
data_merged_6$canopy_class <- factor(data_merged_6$canopy_class)
data_merged_6$Matches_Species <- factor(data_merged_6$Matches_Species)
data_merged_6$Week<-factor(data_merged_6$Week)

# Standartization of numerical variables-------------------------------------------------------------------------
#2.Standardize numerical variables
#!!!!!Not sure I need this step!!!!!
numerical_vars <- c("Seeds.Remaining", "mean_dbh_y", "Shannon_Index_5m", "seeds_disposed", "removal_per_all","species_diversity_camera","number_of_trees","seeds_eaten")
data_merged_6[, numerical_vars] <- scale(data_merged_6[, numerical_vars])

# Normality of response variable-------------------------------------------------------------------------
#3.Check normality of response variable (seeds_eaten) if below 0.05 = not normal
shapiro.test(data_merged_6$seeds_eaten)
#Not normal


# Normality of explanatory variables (factor)-------------------------------------------------------------------------
#4. Check normality for all the explanatory variables.

# Group by site, perform Shapiro-Wilk test for normality of seeds_eaten
site_normality <- data_merged_6 %>%
  group_by(stand_id) %>%
  summarise(p_value = shapiro.test(removal_per_all)$p.value)

# Group by species, perform Shapiro-Wilk test for normality of seeds_eaten
species_normality <- data_merged_6 %>%
  group_by(species) %>%
  summarise(p_value = shapiro.test(removal_per_all)$p.value)

# Group by treatment, perform Shapiro-Wilk test for normality of seeds_eaten
treatment_normality <- data_merged_6 %>%
  group_by(Treatment) %>%
  summarise(p_value = shapiro.test(removal_per_all)$p.value)

# View results
print("Normality Test Results by Site:")
print(site_normality)

print("Normality Test Results by Species:")
print(species_normality)

print("Normality Test Results by Treatment:")
print(treatment_normality)

#Nothing is normal


# Normality of the numerical explanatory variables-------------------------------------------------------------------------

#5. Check normality of numerical explanatory variables
# Identify numeric variables
shapiro.test(data_merged_6$number_of_trees)
shapiro.test(data_merged_6$seeds_disposed)
shapiro.test(data_merged_6$seeds_eaten)
shapiro.test(data_merged_6$species_diversity_camera)
shapiro.test(data_merged_6$Shannon_Index_5m)
shapiro.test(data_merged_6$mean_dbh_y)
shapiro.test(data_merged_6$Seeds.Remaining)

hist(data_merged_6$number_of_trees)
hist(data_merged_6$seeds_disposed)
hist(data_merged_6$seeds_eaten)
hist(data_merged_6$species_diversity_camera)
hist(data_merged_6$Shannon_Index_5m)
hist(data_merged_6$mean_dbh_y)
hist(data_merged_6$Seeds.Remaining)

hist(exp(data_merged_6$number_of_trees))
hist(exp(data_merged_6$seeds_disposed))
hist(exp(data_merged_6$seeds_eaten))
hist(exp(data_merged_6$species_diversity_camera))
hist(exp(data_merged_6$Shannon_Index_5m))
hist(exp(data_merged_6$mean_dbh_y))
hist(exp(data_merged_6$Seeds.Remaining))

shapiro.test(exp(data_merged_6$number_of_trees))
shapiro.test(exp(data_merged_6$seeds_disposed))
shapiro.test(exp(data_merged_6$seeds_eaten))
shapiro.test(exp(data_merged_6$species_diversity_camera))
shapiro.test(exp(data_merged_6$Shannon_Index_5m))
shapiro.test(exp(data_merged_6$mean_dbh_y))
shapiro.test(exp(data_merged_6$Seeds.Remaining))
#None of them are normal. 


# Kruskal test-------------------------------------------------------------------------

#6. As the data is not normal for any of them, I am using the kruskal test to see if there is the difference between the removal rate in the different explanatory variables is significant or not. 
# Example: Kruskal-Wallis test
kruskal.test(removal_per_all ~ Seed_sp, data = data_merged_6)
kruskal.test(removal_per_all ~ Stand, data = data_merged_6)
kruskal.test(removal_per_all ~ Treatment, data = data_merged_6)
kruskal.test(removal_per_all ~ Matches_Species, data = data_merged_6)
kruskal.test(removal_per_all ~ mean_dbh_y, data = data_merged_6)
kruskal.test(removal_per_all ~ number_of_trees, data = data_merged_6)
kruskal.test(removal_per_all ~ Shannon_Index_5m, data = data_merged_6)

#There is no statistical difference for the treatment and the Matches_Species (if the adult tree is the same or not at the seed sp. presented)
#This means that the treatment did not affect the seed removal neither the fact that the adult tree was present or not

# Correlation matrix-------------------------------------------------------------------------

# Correlation matrix
cor_matrix <- cor(data_merged_6[, numerical_vars])
print(cor_matrix)
corrplot(cor_matrix, method = "circle")
# Install and load the 'car' package (if not already installed)

# Calculate VIF for predictor variables
vif_values <- vif(lm(data_merged_6[, numerical_vars]))
# Display VIF values
print(vif_values)
# Calculate condition number
condition_number <- sqrt(max(eigen(cor_matrix)$values) / min(eigen(cor_matrix)$values))
# Display condition number
print(condition_number)

# Pairwise scatterplot of predictor variables
ggpairs(data_merged_6[, numerical_vars])
# Compute eigenvalues of the correlation matrix
eigen_values <- eigen(cor_matrix)$values
# Display eigenvalues
print(eigen_values)

#Results: 

# Model for all -------------------------------------------------------------------------
library(lme4)

# Convert 'successes' and 'trials' to integers if needed
#data_merged_6$Seeds.Remaining <- as.integer(data_merged_6$Seeds.Remaining)
successes<-data_merged_6$seeds_eaten
#data_merged_6$seeds_disposed <- as.integer(data_merged_6$seeds_disposed)
trials<-data_merged_6$seeds_disposed

#data_merged_6$removal_per_all <- data_merged_6$removal_per_all / 100

unique(data_merged_6$seeds)
data_merged_6$seeds <- ifelse(data_merged_6$seeds == 0, 0.01, 
                              ifelse(data_merged_6$seeds == 1, 0.99, data_merged_6$seeds))
# Replace 0 with 0.01 in data_merged_6$seeds_eaten
data_merged_6$seeds_eaten[data_merged_6$seeds_eaten == 0] <- 0


model_0 <- glmer ( cbind(successes, trials - successes)~ 
                 Stand + Treatment + Seed_sp + (1 | Camera.number) +( 1|Week), 
               family = binomial, data =data_merged_6 )
isSingular(model_0)
rePCA(model_0)
model_1 <- glmer( cbind(successes, trials - successes)~ 
                  Stand  + (1 | Camera.number) +( 1|Week), 
                family = binomial, data =data_merged_6 )
model_2 <- glmer( cbind(successes, trials - successes)~ 
                  Treatment + (1 | Camera.number) +( 1|Week), 
                family = binomial, data =data_merged_6 )
model_3 <- glmer( cbind(successes, trials - successes)~ 
                  Seed_sp + (1 | Camera.number) +( 1|Week), 
                family = binomial, data =data_merged_6 )
model_4 <- glmer( cbind(successes, trials - successes)~ 
                  Stand + Seed_sp + (1 | Camera.number) +( 1|Week), 
                family = binomial, data =data_merged_6 )
model_5 <- glmer( cbind(successes, trials - successes)~ 
                    Stand + Treatment + (1 | Camera.number) +( 1|Week), 
                  family = binomial, data =data_merged_6 )
model_6 <- glmer( cbind(successes, trials - successes)~ 
                    Seed_sp + Treatment + (1 | Camera.number) +( 1|Week), 
                  family = binomial, data =data_merged_6 )

# Check summary of the model
summary(model_0);hist(resid(model_0));qqnorm(resid(model_0));qqline(resid(model_0))
summary(model_1);hist(resid(model_1));qqnorm(resid(model_1));qqline(resid(model_1))
summary(model_2);hist(resid(model_2));qqnorm(resid(model_2));qqline(resid(model_2))
summary(model_3);hist(resid(model_3));qqnorm(resid(model_3));qqline(resid(model_3))
summary(model_4);hist(resid(model_4));qqnorm(resid(model_4));qqline(resid(model_4))
summary(model_5);hist(resid(model_4));qqnorm(resid(model_4));qqline(resid(model_4))
summary(model_6);hist(resid(model_4));qqnorm(resid(model_4));qqline(resid(model_4))

print(AIC(model_0,model_1,model_2,model_3,model_4,model_5,model_6))
#Best model is 0

# Model for seed sp.-------------------------------------------------------------------------

#Use filtering/grouping based on Seed_sp and fit separate models.

#Seed Sp
ABAM<- subset(data_merged_6,Seed_sp=="ABAM")
successes<-ABAM$seeds_eaten
trials<-ABAM$seeds_disposed
model_0 <- glmer(cbind(successes, trials - successes) ~ Stand + Treatment + Matches_Species +( 1|Week), family = binomial, data = ABAM)
isSingular(model_0)
rePCA(model_0)
summary(model_0)
model_1 <- glmer(cbind(successes, trials - successes) ~ Stand +(1 | Camera.number) +( 1|Week), family = binomial, data = ABAM)
model_2 <- glmer(cbind(successes, trials - successes) ~ Treatment +(1 | Camera.number) +( 1|Week), family = binomial, data = ABAM)
model_3 <- glmer(cbind(successes, trials - successes) ~ Treatment*Stand +(1 | Camera.number) +( 1|Week), family = binomial, data = ABAM)
summary(model_1)
summary(model_0);hist(resid(model_0));qqnorm(resid(model_0));qqline(resid(model_0))
summary(model_1);hist(resid(model_1));qqnorm(resid(model_1));qqline(resid(model_1))
summary(model_2);hist(resid(model_2));qqnorm(resid(model_2));qqline(resid(model_2))
print(AIC(model_0,model_1,model_2, model_3))

#Treatment doesn't have effect on the seed removal


#Seed Sp
ABLA<- subset(data_merged_6,Seed_sp=="ABLA")
successes<-ABLA$seeds_eaten
trials<-ABLA$seeds_disposed
model_0 <- glmer(cbind(successes, trials - successes) ~ Stand + Treatment + Matches_Species +( 1|Week), family = binomial, data = ABLA)
isSingular(model_0)
rePCA(model_0)
summary(model_0)
model_1 <- glmer(cbind(successes, trials - successes) ~ Stand +(1 | Camera.number) +( 1|Week), family = binomial, data = ABLA)
model_2 <- glmer(cbind(successes, trials - successes) ~ Treatment +(1 | Camera.number) +( 1|Week), family = binomial, data = ABLA)
model_3 <- glmer(cbind(successes, trials - successes) ~ Treatment*Stand +(1 | Camera.number) +( 1|Week), family = binomial, data = ABLA)
summary(model_1)
summary(model_0);hist(resid(model_0));qqnorm(resid(model_0));qqline(resid(model_0))
summary(model_1);hist(resid(model_1));qqnorm(resid(model_1));qqline(resid(model_1))
summary(model_2);hist(resid(model_2));qqnorm(resid(model_2));qqline(resid(model_2))
print(AIC(model_0,model_1,model_2, model_3))



# Model for sites-------------------------------------------------------------------------

#Model for each site
AE10_site<- subset(data_merged_6,Stand=="AE10")
AV06_site<- subset(data_merged_6, Stand="AV06")
TO04_site<- subset(data_merged_6,Stand="TO04")

# Convert necessary variables to numeric
AE10_site$mean_dbh_y <- as.numeric(as.character(AE10_site$mean_dbh_y))
AE10_site$species_diversity_camera <- as.numeric(as.character(AE10_site$species_diversity_camera))
AE10_site$Matches_Species <- ifelse(AE10_site$Matches_Species == "Yes", 1, 0)  # Convert Matches_Species to binary numeric (0 or 1)
AV06_site$mean_dbh_y <- as.numeric(as.character(AV06_site$mean_dbh_y))
AV06_site$species_diversity_camera <- as.numeric(as.character(AV06_site$species_diversity_camera))
AV06_site$Matches_Species <- ifelse(AV06_site$Matches_Species == "Yes", 1, 0)
TO04_site$mean_dbh_y <- as.numeric(as.character(TO04_site$mean_dbh_y))
TO04_site$species_diversity_camera <- as.numeric(as.character(TO04_site$species_diversity_camera))
TO04_site$Matches_Species <- ifelse(TO04_site$Matches_Species == "Yes", 1, 0)


#AE10: 
successes<-AE10_site$seeds_eaten
trials<-AE10_site$seeds_disposed

# Fit the linear regression model
model_0 <- glmer(cbind(successes, trials - successes) ~ number_of_trees + Matches_Species + mean_dbh_y + species_diversity_camera +(1 | Camera.number) +( 1|Week), 
                 family = binomial, data = AE10_site)
model_1<-glmer(cbind(successes, trials - successes) ~ Matches_Species + mean_dbh_y + species_diversity_camera +(1 | Camera.number) +( 1|Week), 
               family = binomial, data = AE10_site)
model_2 <- glmer(cbind(successes, trials - successes) ~ Matches_Species + mean_dbh_y + (1 | Camera.number) +( 1|Week), 
                 family = binomial, data = AE10_site)
model_3 <- glmer(cbind(successes, trials - successes) ~ Matches_Species +(1 | Camera.number) +( 1|Week), 
                 family = binomial, data = AE10_site)
model_4 <- glmer(cbind(successes, trials - successes) ~ Matches_Species : mean_dbh_y + (1 | Camera.number) +( 1|Week), 
                 family = binomial, data = AE10_site)

summary(model_0);hist(resid(model_0));qqnorm(resid(model_0));qqline(resid(model_0))
summary(model_1);hist(resid(model_1));qqnorm(resid(model_1));qqline(resid(model_1))
summary(model_2);hist(resid(model_2));qqnorm(resid(model_2));qqline(resid(model_2))
summary(model_3);hist(resid(model_3));qqnorm(resid(model_3));qqline(resid(model_3))
summary(model_4);hist(resid(model_4));qqnorm(resid(model_4));qqline(resid(model_4))


print(AIC(model_0,model_1,model_2,model_3,model_4))


#Best model = Matched species 

#AV06


successes<-AV06_site$seeds_eaten
trials<-AV06_site$seeds_disposed
model_0 <- glmer(cbind(successes, trials - successes) ~ number_of_trees + Matches_Species + mean_dbh_y + species_diversity_camera +(1 | Camera.number) +( 1|Week), 
                 family = binomial, data = AV06_site)
model_1 <- glmer(cbind(successes, trials - successes) ~ Matches_Species + mean_dbh_y + species_diversity_camera +(1 | Camera.number) +( 1|Week), 
                 family = binomial, data = AV06_site)
model_2 <- glmer(cbind(successes, trials - successes) ~  Matches_Species +(1 | Camera.number) +( 1|Week), 
                 family = binomial, data = AV06_site)
model_3 <- glmer(cbind(successes, trials - successes) ~ Matches_Species + mean_dbh_y +(1 | Camera.number) +( 1|Week), 
                 family = binomial, data = AV06_site)
model_4 <- glmer(cbind(successes, trials - successes) ~ Matches_Species + species_diversity_camera +(1 | Camera.number) +( 1|Week), 
                 family = binomial, data = AV06_site)

summary(model_0);hist(resid(model_0));qqnorm(resid(model_0));qqline(resid(model_0))
summary(model_1);hist(resid(model_1));qqnorm(resid(model_1));qqline(resid(model_1))
summary(model_2);hist(resid(model_2));qqnorm(resid(model_2));qqline(resid(model_2))
summary(model_3);hist(resid(model_3));qqnorm(resid(model_3));qqline(resid(model_3))
summary(model_4);hist(resid(model_4));qqnorm(resid(model_4));qqline(resid(model_4))

print(AIC(model_0,model_1,model_2,model_3,model_4))

#Best model is matched species and species diversity. 

#TO04


successes<-TO04_site$seeds_eaten
trials<-TO04_site$seeds_disposed
model_0 <- glmer(cbind(successes, trials - successes) ~ number_of_trees + Matches_Species + mean_dbh_y + species_diversity_camera +(1 | Camera.number) +( 1|Week),family = binomial, data = TO04_site)
model_1 <- glmer(cbind(successes, trials - successes) ~ Matches_Species + mean_dbh_y + species_diversity_camera +(1 | Camera.number) +( 1|Week),family = binomial, data = TO04_site)
model_2 <- glmer(cbind(successes, trials - successes) ~  Matches_Species +(1 | Camera.number) +( 1|Week), family = binomial, data = TO04_site)
model_3 <- glmer(cbind(successes, trials - successes) ~ Matches_Species + mean_dbh_y +(1 | Camera.number) +( 1|Week), family = binomial, data = TO04_site)
model_4 <- glmer(cbind(successes, trials - successes) ~ Matches_Species + species_diversity_camera +(1 | Camera.number) +( 1|Week),family = binomial, data = TO04_site)
model_5 <- glmer(cbind(successes, trials - successes) ~ Matches_Species * species_diversity_camera +(1 | Camera.number) +( 1|Week),family = binomial, data = TO04_site)

summary(model_0);hist(resid(model_0));qqnorm(resid(model_0));qqline(resid(model_0))
summary(model_1);hist(resid(model_1));qqnorm(resid(model_1));qqline(resid(model_1))
summary(model_2);hist(resid(model_2));qqnorm(resid(model_2));qqline(resid(model_2))
summary(model_3);hist(resid(model_3));qqnorm(resid(model_3));qqline(resid(model_3))
summary(model_4);hist(resid(model_4));qqnorm(resid(model_4));qqline(resid(model_4))

print(AIC(model_0,model_1,model_2,model_3,model_4,model_5))


#Seems like the same model?


#AE10: 
# Fit the linear regression model
model_0 <- glmer(seeds ~ Seed_sp+ Treatment+ number_of_trees + Matches_Species + mean_dbh_y + species_diversity_camera +(1 | Camera.number) +( 1|Week), 
                 family = binomial, data = AE10_site)
model_1<-lm(removal_per_all ~ Matches_Species, data = AE10_site)
model_2 <- lm(removal_per_all ~ number_of_trees + Matches_Species + species_diversity_camera, data = AE10_site)
model_3 <- lm(removal_per_all ~ number_of_trees + species_diversity_camera, data = AE10_site)
model_4 <- lm(removal_per_all ~ number_of_trees  + mean_dbh_y + species_diversity_camera, data = AE10_site)
model_5 <- lm(removal_per_all ~ number_of_trees + Matches_Species, data = AE10_site)
summary(model_0);hist(resid(model_0));qqnorm(resid(model_0));qqline(resid(model_0))
summary(model_1);hist(resid(model_1));qqnorm(resid(model_1));qqline(resid(model_1))
summary(model_2);hist(resid(model_2));qqnorm(resid(model_2));qqline(resid(model_2))
summary(model_3);hist(resid(model_3));qqnorm(resid(model_3));qqline(resid(model_3))
summary(model_4);hist(resid(model_4));qqnorm(resid(model_4));qqline(resid(model_4))

print(AIC(model_0,model_1,model_2,model_3,model_4, model_5))

#The best performing model is the removal per matched species. Followed by the number of trees and matched species. 


#AV06
model_0 <- glmer(removal_per_all ~ Seed_sp+ Treatment+ number_of_trees + Matches_Species + mean_dbh_y + species_diversity_camera, data = AV06_site)
summary(model_0);hist(resid(model_0));qqnorm(resid(model_0));qqline(resid(model_0))
model_1 <- lm(removal_per_all ~ Matches_Species + mean_dbh_y + species_diversity_camera, data = AV06_site)
model_2 <- glm(removal_per_all ~ Matches_Species , data = AV06_site)
model_3 <- glm(removal_per_all ~ species_diversity_camera, data = AV06_site)
model_4 <- glm(removal_per_all ~ Matches_Species + species_diversity_camera, data = AV06_site)
summary(model_0);hist(resid(model_0));qqnorm(resid(model_0));qqline(resid(model_0))
summary(model_1);hist(resid(model_1));qqnorm(resid(model_1));qqline(resid(model_1))
summary(model_2);hist(resid(model_2));qqnorm(resid(model_2));qqline(resid(model_2))
summary(model_3);hist(resid(model_3));qqnorm(resid(model_3));qqline(resid(model_3))
summary(model_4);hist(resid(model_4));qqnorm(resid(model_4));qqline(resid(model_4))

print(AIC(model_0,model_1,model_2,model_3,model_4))

#The pest model is the model 4: Matched species and species_diversity camera 
