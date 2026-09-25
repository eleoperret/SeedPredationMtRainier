install.packages("dyplyr")
install.packages("corrplot")
install.packages("car")
install.packages("GGally")
installed.packages("lme4")
install.packages("MuMIn")
library(dplyr)
library(corrplot)
library(car)
library(GGally)
library(lme4)
library(MuMIn)


##Overall results: 
##For the dataset: Seed specie and Treatment seemed to explain the most variation in the chances of success or failure of a seed removed

# Loading the data --------------------------------------------------------
load("C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets/data_merged_6.RData")

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

#Things to consider. As there was no intention of matching the seed species with the adult tree and it was not standartized, can I imply from my result that it had an effect?

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
#data_merged_6[, numerical_vars] <- scale(data_merged_6[, numerical_vars])

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
  group_by(Seed_sp) %>%
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
#This means that the treatment did not affect the seed removal neither the fact that the adult tree was present or not.
##Is that correct?

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

#Defining the sucesses and trials
successes<-data_merged_6$seeds_eaten
trials<-data_merged_6$seeds_disposed

#Defining the different models
model_0 <- glmer ( cbind(successes, trials - successes)~ 
                 Stand + Treatment + Seed_sp + (1 | Camera.number) +( 1|Week), 
               family = binomial, data =data_merged_6 )
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
model_7<- glmer( cbind(successes, trials - successes)~ 
                           1 + (1 | Camera.number) +( 1|Week), 
                         family = binomial, data =data_merged_6 )

#Model selection based on the AIC score
print(AIC(model_0,model_1,model_2,model_3,model_4,model_5,model_6,model_7))

# Check summary of the model
summary(model_0);hist(resid(model_0));qqnorm(resid(model_0));qqline(resid(model_0))
summary(model_1);hist(resid(model_1));qqnorm(resid(model_1));qqline(resid(model_1))
summary(model_2);hist(resid(model_2));qqnorm(resid(model_2));qqline(resid(model_2))
summary(model_3);hist(resid(model_3));qqnorm(resid(model_3));qqline(resid(model_3))
summary(model_4);hist(resid(model_4));qqnorm(resid(model_4));qqline(resid(model_4))
summary(model_5);hist(resid(model_5));qqnorm(resid(model_5));qqline(resid(model_5))
summary(model_6);hist(resid(model_6));qqnorm(resid(model_6));qqline(resid(model_6))
summary(model_)7;hist(resid(model_7));qqnorm(resid(model_7));qqline(resid(model_7))

#Best model is 6 based on the AIC score (Species + Treatment).
# R square
r.squaredGLMM(model_6)
#Around 34% is explained by my model and 96 % in total

# Model for seed sp.-------------------------------------------------------------------------

###ABAM 
ABAM<- subset(data_merged_6,Seed_sp=="ABAM")
successes<-ABAM$seeds_eaten
trials<-ABAM$seeds_disposed
model_0 <- glmer(cbind(successes, trials - successes) ~ 1 +( 1|Week)+(1 | Camera.number), family = binomial, data = ABAM)
model_1 <- glmer(cbind(successes, trials - successes) ~ Stand +( 1|Week)+(1 | Camera.number), family = binomial, data = ABAM)
model_2 <- glmer(cbind(successes, trials - successes) ~ Treatment + ( 1|Week)+(1 | Camera.number), family = binomial, data = ABAM)
model_3 <- glmer(cbind(successes, trials - successes) ~ Stand + Treatment + ( 1|Week)+(1 | Camera.number), family = binomial, data = ABAM)
model_4 <- glmer(cbind(successes, trials - successes) ~ Stand * Treatment + ( 1|Week) +(1 | Camera.number), family = binomial, data = ABAM)


#They are all singular
#Possibility of exploration :
# Create a contingency table
contingency_table <- table(ABAM$Stand, ABAM$Treatment)
# Display the contingency table
print(contingency_table)
# Perform chi-squared test of independence
chi_square_test <- chisq.test(ABAM$Stand, ABAM$Treatment)
# Display the results of the chi-squared test
print(chi_square_test)
#There is no correlation between Stand and Treatment
#But the model is still not working. Why?

print(AIC(model_0,model_1,model_2,model_3, model_4))

summary(model_0);hist(resid(model_0));qqnorm(resid(model_0));qqline(resid(model_0))
summary(model_1);hist(resid(model_1));qqnorm(resid(model_1));qqline(resid(model_1))
summary(model_2);hist(resid(model_2));qqnorm(resid(model_2));qqline(resid(model_2))
summary(model_3);hist(resid(model_3));qqnorm(resid(model_3));qqline(resid(model_3))
summary(model_4);hist(resid(model_4));qqnorm(resid(model_4));qqline(resid(model_4))

#Looks like model 1 is the best model (but still singular and cannot resolve that)
#Looking at the summary,the week doesn't seem to have an effect at all. Why? 
#Checking the model without the week as a random variable. 
model_1 <- glmer(cbind(successes, trials - successes) ~ Stand +(1 | Camera.number), family = binomial, data = ABAM)
#No more singularity. But why? 
print(AIC(model_0,model_1,model_2,model_3, model_4))
#Model 1 is still the best model.
summary(model_1);hist(resid(model_1));qqnorm(resid(model_1));qqline(resid(model_1))
#tried to remove the week as a random effect for all models and it gives the same result with model 1 being the lowest AIC score. 

#Model 1: Stand explains the most the removal for ABAM seed species.
# R square
r.squaredGLMM(model_1)
#Explaines 28% and 72% with the random variable. 

###ABLA 
ABLA<- subset(data_merged_6,Seed_sp=="ABLA")
successes<-ABLA$seeds_eaten
trials<-ABLA$seeds_disposed
model_0 <- glmer(cbind(successes, trials - successes) ~ 1 +( 1|Week)+(1 | Camera.number), family = binomial, data = ABLA)
model_1 <- glmer(cbind(successes, trials - successes) ~ Stand + ( 1|Week)+(1 | Camera.number), family = binomial, data = ABLA)
model_2 <- glmer(cbind(successes, trials - successes) ~ Treatment + ( 1|Week)+(1 | Camera.number), family = binomial, data = ABLA)
model_3 <- glmer(cbind(successes, trials - successes) ~ Stand + Treatment + ( 1|Week)+(1 | Camera.number), family = binomial, data = ABLA)
model_4 <- glmer(cbind(successes, trials - successes) ~ Stand * Treatment + ( 1|Week), family = binomial, data = ABLA)

print(AIC(model_0,model_1,model_2, model_3,model_4))
#Best model is model 0. Model null
summary(model_0);hist(resid(model_0));qqnorm(resid(model_0));qqline(resid(model_0))
summary(model_1);hist(resid(model_1));qqnorm(resid(model_1));qqline(resid(model_1))
summary(model_2);hist(resid(model_2));qqnorm(resid(model_2));qqline(resid(model_2))
summary(model_3);hist(resid(model_3));qqnorm(resid(model_3));qqline(resid(model_3))
summary(model_4);hist(resid(model_4));qqnorm(resid(model_4));qqline(resid(model_4))

#Based on the AIC score, the best model is the null model.
#There is no variation explained by the explanatory variables. Maybe dataset to small or need more information to capture the reason behind the variation of seed removal. 
# R square
r.squaredGLMM(model_0)
#Seems like almost all the model variation is explained by the random effects (93%)
#However there is a hole in the histogram of resid. Why?

###CANO 
CANO<- subset(data_merged_6,Seed_sp=="CANO")
successes<-CANO$seeds_eaten
trials<-CANO$seeds_disposed
model_0 <- glmer(cbind(successes, trials - successes) ~ 1 +( 1|Week)+(1 | Camera.number), family = binomial, data = CANO)
model_1 <- glmer(cbind(successes, trials - successes) ~ Stand + ( 1|Week)+(1 | Camera.number), family = binomial, data = CANO)
model_2 <- glmer(cbind(successes, trials - successes) ~ Treatment + ( 1|Week)+(1 | Camera.number), family = binomial, data = CANO)
model_3 <- glmer(cbind(successes, trials - successes) ~ Stand + Treatment + ( 1|Week)+(1 | Camera.number), family = binomial, data = CANO)
model_4 <- glmer(cbind(successes, trials - successes) ~ Stand * Treatment + ( 1|Week), family = binomial, data = CANO)

print(AIC(model_0,model_1,model_2, model_3,model_4))
#Best model is model 3
summary(model_0);hist(resid(model_0));qqnorm(resid(model_0));qqline(resid(model_0))
summary(model_1);hist(resid(model_1));qqnorm(resid(model_1));qqline(resid(model_1))
summary(model_2);hist(resid(model_2));qqnorm(resid(model_2));qqline(resid(model_2))
summary(model_3);hist(resid(model_3));qqnorm(resid(model_3));qqline(resid(model_3))
summary(model_4);hist(resid(model_4));qqnorm(resid(model_4));qqline(resid(model_4))

#The model with the lowest AIC score is model 3 with Stand + Treatment.
# R square
r.squaredGLMM(model_3)
#Almost all the variation is explained by my model (99%), that is odd I think. 
#Now based on that, what from the stand could be the reason behind?

model_5 <- glmer(cbind(successes, trials - successes) ~ Treatment  + mean_dbh_y+ species_diversity_camera+ number_of_trees+ (1|Camera.number)+ ( 1|Week) +(1|Stand), family = binomial, data = CANO)
summary(model_5);hist(resid(model_5));qqnorm(resid(model_5));qqline(resid(model_5))
model_6 <- glmer(cbind(successes, trials - successes) ~ Treatment  + species_diversity_camera+ (1|Camera.number)+ ( 1|Week) +(1|Stand), family = binomial, data = CANO)
summary(model_6);hist(resid(model_6));qqnorm(resid(model_6));qqline(resid(model_6))
print(AIC(model_5,model_6))

# R square
r.squaredGLMM(model_6)

#The number of species around the camera could explain together with the treatment the reason behind the variation of seed removal. Best model based on AIC score is model 6
#However best model overall is Stand + Treatment

###PSME
PSME<- subset(data_merged_6,Seed_sp=="PSME")
successes<-PSME$seeds_eaten
trials<-PSME$seeds_disposed
model_0 <- glmer(cbind(successes, trials - successes) ~ 1 +( 1|Week)+(1 | Camera.number), family = binomial, data = PSME)
model_1 <- glmer(cbind(successes, trials - successes) ~ Stand + ( 1|Week)+(1 | Camera.number), family = binomial, data = PSME)
model_2 <- glmer(cbind(successes, trials - successes) ~ Treatment + ( 1|Week)+(1 | Camera.number), family = binomial, data = PSME)
model_3 <- glmer(cbind(successes, trials - successes) ~ Stand + Treatment + ( 1|Week)+(1 | Camera.number), family = binomial, data = PSME)
model_4 <- glmer(cbind(successes, trials - successes) ~ Stand * Treatment + ( 1|Week), family = binomial, data = PSME)

print(AIC(model_0,model_1,model_2,model_3, model_4))
summary(model_0);hist(resid(model_0));qqnorm(resid(model_0));qqline(resid(model_0))
summary(model_1);hist(resid(model_1));qqnorm(resid(model_1));qqline(resid(model_1))
summary(model_2);hist(resid(model_2));qqnorm(resid(model_2));qqline(resid(model_2))
summary(model_3);hist(resid(model_3));qqnorm(resid(model_3));qqline(resid(model_3))
summary(model_4);hist(resid(model_4));qqnorm(resid(model_4));qqline(resid(model_4))

#Based on the AIC score the best model is 1 however, it fails to converge
#Model 0 and 2 are the second best as they do not fail. Model 2 do seem to have a better histogram of residuals. Then is treatment the best model?

# R square
r.squaredGLMM(model_0); r.squaredGLMM(model_2)
#However based on the R squared model_0 seems better. I think there is something I'm missing maybe...

model_2 <- glmer(cbind(successes, trials - successes) ~ Treatment + ( 1|Week)+(1 | Camera.number) +(1|Stand), family = binomial, data = PSME)
#If the Stand is added as a random variable the score gets lower.But not sure I want to treat the stand as a random variable. 

#Need to dig deeper into it?


###THPL
THPL<- subset(data_merged_6,Seed_sp=="THPL")
successes<-THPL$seeds_eaten
trials<-THPL$seeds_disposed
model_0 <- glmer(cbind(successes, trials - successes) ~ 1 +( 1|Week)+(1 | Camera.number), family = binomial, data = THPL)
model_1 <- glmer(cbind(successes, trials - successes) ~ Stand + ( 1|Week)+(1 | Camera.number), family = binomial, data = THPL)
model_2 <- glmer(cbind(successes, trials - successes) ~ Treatment + ( 1|Week)+(1 | Camera.number), family = binomial, data = THPL)
model_3 <- glmer(cbind(successes, trials - successes) ~ Stand + Treatment + ( 1|Week)+(1 | Camera.number), family = binomial, data = THPL)
model_4 <- glmer(cbind(successes, trials - successes) ~ Stand * Treatment + ( 1|Week), family = binomial, data = THPL)

print(AIC(model_0,model_1,model_2,model_3, model_4))
#It seems that the best model is 0 or 2 :Null or Treatment
summary(model_0);hist(resid(model_0));qqnorm(resid(model_0));qqline(resid(model_0))
summary(model_1);hist(resid(model_1));qqnorm(resid(model_1));qqline(resid(model_1))
summary(model_2);hist(resid(model_2));qqnorm(resid(model_2));qqline(resid(model_2))
summary(model_3);hist(resid(model_3));qqnorm(resid(model_3));qqline(resid(model_3))
summary(model_4);hist(resid(model_4));qqnorm(resid(model_4));qqline(resid(model_4))

#Looking at the histogramm of residuals; model 2 seems more normal

# R square
r.squaredGLMM(model_0); r.squaredGLMM(model_2)
#the R square is the same. 

#I don't know how I can justify my selection (I would select model_2 : Treatment ) but can I do that only based on the histogramm of residuals?


###TSHE
TSHE<- subset(data_merged_6,Seed_sp=="TSHE")
successes<-TSHE$seeds_eaten
trials<-TSHE$seeds_disposed
model_0 <- glmer(cbind(successes, trials - successes) ~ 1 +( 1|Week)+(1 | Camera.number), family = binomial, data = TSHE)
model_1 <- glmer(cbind(successes, trials - successes) ~ Stand + ( 1|Week)+(1 | Camera.number), family = binomial, data = TSHE)
model_2 <- glmer(cbind(successes, trials - successes) ~ Treatment + ( 1|Week)+(1 | Camera.number), family = binomial, data = TSHE)
model_3 <- glmer(cbind(successes, trials - successes) ~ Stand + Treatment + ( 1|Week)+(1 | Camera.number), family = binomial, data = TSHE)
model_4 <- glmer(cbind(successes, trials - successes) ~ Stand * Treatment + ( 1|Week), family = binomial, data = TSHE)

print(AIC(model_0,model_1,model_2,model_3, model_4))
#Best model is model 2
summary(model_0);hist(resid(model_0));qqnorm(resid(model_0));qqline(resid(model_0))
summary(model_1);hist(resid(model_1));qqnorm(resid(model_1));qqline(resid(model_1))
summary(model_2);hist(resid(model_2));qqnorm(resid(model_2));qqline(resid(model_2))
summary(model_3);hist(resid(model_3));qqnorm(resid(model_3));qqline(resid(model_3))
summary(model_4);hist(resid(model_4));qqnorm(resid(model_4));qqline(resid(model_4))

# R square
r.squaredGLMM(model_2); r.squaredGLMM(model_3); r.squaredGLMM(model_0)
#Again barely explains the variability but the overall model (with random) explains 99% ! Very high. Does this means that that I should go for the null model?

#How to justify again?

#Treatment only has an effect? Stand + Treatment (model 3) has a better R squared (0,2% compared to 0,05% from model 2 )

# Model for sites-------------------------------------------------------------------------

#Model for each site
AE10_site<- subset(data_merged_6,Stand=="AE10")
AV06_site<- subset(data_merged_6, Stand=="AV06")
TO04_site<- subset(data_merged_6,Stand=="TO04")

###AE10: 
successes<-AE10_site$seeds_eaten
trials<-AE10_site$seeds_disposed

model_0 <- glmer(cbind(successes, trials - successes) ~ number_of_trees + Matches_Species + mean_dbh_y + species_diversity_camera +(1 | Camera.number) +( 1|Week), family = binomial, data = AE10_site)
model_1<-glmer(cbind(successes, trials - successes) ~ Matches_Species + mean_dbh_y + species_diversity_camera +(1 | Camera.number) +( 1|Week),family = binomial, data = AE10_site)
model_2 <- glmer(cbind(successes, trials - successes) ~ Matches_Species + mean_dbh_y + (1 | Camera.number) +( 1|Week),family = binomial, data = AE10_site)
model_3 <- glmer(cbind(successes, trials - successes) ~ Matches_Species +(1 | Camera.number) +( 1|Week), family = binomial, data = AE10_site)
model_4 <- glmer(cbind(successes, trials - successes) ~ Matches_Species * mean_dbh_y + (1 | Camera.number) +( 1|Week), family = binomial, data = AE10_site)
model_5 <- glmer(cbind(successes, trials - successes) ~ 1 + (1 | Camera.number) +( 1|Week), family = binomial, data = AE10_site)

print(AIC(model_0,model_1,model_2,model_3,model_4,model_5))
summary(model_0);hist(resid(model_0));qqnorm(resid(model_0));qqline(resid(model_0))
summary(model_1);hist(resid(model_1));qqnorm(resid(model_1));qqline(resid(model_1))
summary(model_2);hist(resid(model_2));qqnorm(resid(model_2));qqline(resid(model_2))
summary(model_3);hist(resid(model_3));qqnorm(resid(model_3));qqline(resid(model_3))
summary(model_4);hist(resid(model_4));qqnorm(resid(model_4));qqline(resid(model_4))
summary(model_5);hist(resid(model_5));qqnorm(resid(model_5));qqline(resid(model_5))

#Even if not the lowest AIC score,I think that the best model is model 3. Based on the QQ-plot of the residuals and the significance of Matched species Yes. 
#Best model = Matched species 
counts <- table(AE10_site$Matches_Species)
print(counts)
#There are more No than yes, which means that the seeds were more likely removed for this site when the Tree specie did not match the seed sp. This is as expected as there is a high removal rate of PSME (which is not a specie present at the site)
Counts_speciestrees<-table (AE10_site$species)
print(Counts_speciestrees)

r.squaredGLMM(model_3);r.squaredGLMM(model_4)
#However, the best model based on the AIC score and R squared in model 4. Not sure how to select?

###AV06
successes<-AV06_site$seeds_eaten
trials<-AV06_site$seeds_disposed
model_0 <- glmer(cbind(successes, trials - successes) ~ number_of_trees + Matches_Species + mean_dbh_y + species_diversity_camera +(1 | Camera.number) +( 1|Week), family = binomial, data = AV06_site)
model_1 <- glmer(cbind(successes, trials - successes) ~ Matches_Species + mean_dbh_y + species_diversity_camera +(1 | Camera.number) +( 1|Week), family = binomial,data = AV06_site)
model_2 <- glmer(cbind(successes, trials - successes) ~  Matches_Species +(1 | Camera.number) +( 1|Week), family = binomial, data = AV06_site)
model_3 <- glmer(cbind(successes, trials - successes) ~ Matches_Species + mean_dbh_y +(1 | Camera.number) +( 1|Week), family = binomial, data = AV06_site)
model_4 <- glmer(cbind(successes, trials - successes) ~ Matches_Species + species_diversity_camera +(1 | Camera.number) +( 1|Week),family = binomial, data = AV06_site)
model_5 <- glmer(cbind(successes, trials - successes) ~ 1 +(1 | Camera.number) +( 1|Week),family = binomial, data = AV06_site)
model_6 <- glmer(cbind(successes, trials - successes) ~ number_of_trees + Matches_Species + mean_dbh_y +(1 | Camera.number) +( 1|Week), family = binomial, data = AV06_site)

print(AIC(model_0,model_1,model_2,model_3,model_4,model_5,model_6))
summary(model_0);hist(resid(model_0));qqnorm(resid(model_0));qqline(resid(model_0))
summary(model_1);hist(resid(model_1));qqnorm(resid(model_1));qqline(resid(model_1))
summary(model_2);hist(resid(model_2));qqnorm(resid(model_2));qqline(resid(model_2))
summary(model_3);hist(resid(model_3));qqnorm(resid(model_3));qqline(resid(model_3))
summary(model_4);hist(resid(model_4));qqnorm(resid(model_4));qqline(resid(model_4))
summary(model_5);hist(resid(model_5));qqnorm(resid(model_5));qqline(resid(model_5))
summary(model_6);hist(resid(model_6));qqnorm(resid(model_6));qqline(resid(model_6))

#Best model is model 6 (even if slightly highest AIC, has a lower BIC and all explanatory variables are significant).
#For AV06= Number of trees + Matched species + Mean dbh

#Signification?
#Tree number
AV06_site$nbtreecat<-ifelse (AV06_site$number_of_trees<10,"small",ifelse(AV06_site$number_of_trees<20,"medium","large"))
counts_nbtrees<-table(AV06_site$nbtreecat)
print(counts_nbtrees)
AV06_site$nbtreecat <- factor(AV06_site$nbtreecat, levels = c("small", "medium", "large"), ordered = TRUE)
plot(AV06_site$nbtreecat,AV06_site$removal_per_all)
plot(AV06_site$number_of_trees,AV06_site$removal_per_all)
#The number of trees around the camera had a influence on the removal rate. From the counts and boxplot we can see that the amount of medium tree density is smaller but had a more significant impact on the removal than the small trees density.

#Matched specie
counts <- table(AV06_site$Matches_Species)
print(counts)
plot(AV06_site$Matches_Species,AV06_site$removal_per_all)
#There is overall a higher proportion of seed species that do not match the adult tree and it seems that this had an influence on the removal rate.

#Tree size
AV06_site$dbhcat<-ifelse (AV06_site$mean_dbh_y<20,"small",ifelse(AV06_site$mean_dbh_y<30,"medium","large"))
counts_dbh <- table(AV06_site$dbhcat)
print(counts_dbh)
AV06_site$dbhcat <- factor(AV06_site$dbhcat, levels = c("small", "medium", "large"), ordered = TRUE)
plot(AV06_site$dbhcat,AV06_site$removal_per_all)
plot(AV06_site$mean_dbh_y,AV06_site$removal_per_all)
#It seems that eventhough the mean dbh is distributed equally among the site (they all have the same amount of trees in each categories, small, medium and large), the dbh still had an influence on the removal with the smallest trees behind the one where seed removal was the highest. This is also confirmed by the number of tree results above, as this represents the 5m around the camera, and if there are more trees around the camera, they must be also finer. 
#It means that in general for this site, the more trees and thiner contributed more to the removal, and the novelty of the seed (or the no-matching with the adult tree) did also influence. Which based on the species the camera was attached to is not suprinsing as there was only ABAM and TSHE (no PSME which is one of the most remove and preferred specie)
Counts_speciestrees<-table (AV06_site$species)
print(Counts_speciestrees)

###TO04
successes<-TO04_site$seeds_eaten
trials<-TO04_site$seeds_disposed

model_0 <- glmer(cbind(successes, trials - successes) ~ number_of_trees + Matches_Species + mean_dbh_y + species_diversity_camera +(1 | Camera.number) +( 1|Week),family = binomial, data = TO04_site)
model_1 <- glmer(cbind(successes, trials - successes) ~ Matches_Species + mean_dbh_y + species_diversity_camera +(1 | Camera.number) +( 1|Week),family = binomial, data = TO04_site)
model_2 <- glmer(cbind(successes, trials - successes) ~  Matches_Species +(1 | Camera.number) +( 1|Week), family = binomial, data = TO04_site)
model_3 <- glmer(cbind(successes, trials - successes) ~ Matches_Species + mean_dbh_y +(1 | Camera.number) +( 1|Week), family = binomial, data = TO04_site)
model_4 <- glmer(cbind(successes, trials - successes) ~ Matches_Species + species_diversity_camera +(1 | Camera.number) +( 1|Week),family = binomial, data = TO04_site)
model_5 <- glmer(cbind(successes, trials - successes) ~ Matches_Species * species_diversity_camera +(1 | Camera.number) +( 1|Week),family = binomial, data = TO04_site)
model_6 <- glmer(cbind(successes, trials - successes) ~ 1 +(1 | Camera.number) +( 1|Week),family = binomial, data = TO04_site)

print(AIC(model_0,model_1,model_2,model_3,model_4,model_5,model_6))
summary(model_0);hist(resid(model_0));qqnorm(resid(model_0));qqline(resid(model_0))
summary(model_1);hist(resid(model_1));qqnorm(resid(model_1));qqline(resid(model_1))
summary(model_2);hist(resid(model_2));qqnorm(resid(model_2));qqline(resid(model_2))
summary(model_3);hist(resid(model_3));qqnorm(resid(model_3));qqline(resid(model_3))
summary(model_4);hist(resid(model_4));qqnorm(resid(model_4));qqline(resid(model_4))
summary(model_6);hist(resid(model_6));qqnorm(resid(model_6));qqline(resid(model_6))

#Best model is model 2 with Matched species

#Interpretation
counts <- table(TO04_site$Matches_Species)
print(counts)
plot(TO04_site$Matches_Species,TO04_site$removal_per_all)
#There is overall a higher proportion of seed species that do not match the adult tree and it seems that this had an influence on the removal rate.
Counts_speciestrees<-table (TO04_site$species)
print(Counts_speciestrees)
#Again, the seeds where never placed in front of a PSME or CANO (I mean CANO is not even present at the site).
#This can show that even if the seed is not present at the site, or the adult tree is not from the same specie, PSME and CANO still get removed a lot more compared to the ABAM and TSHE. 


#What about ABAM. It is present at each site. For this specie in particular. Is there a higher removal rate when the parent tree is there or not?
counts <- table(ABAM$Matches_Species)
print(counts)
plot(ABAM$Matches_Species,ABAM$removal_per_all)
#Even though there are more times where the adult tree is present, the removal seems similar, which means that for ABAM the presence or absence of the adult tree did not seem to impact removal rate. 