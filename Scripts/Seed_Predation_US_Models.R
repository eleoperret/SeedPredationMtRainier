####Code for CAMERA TRAP EXPERIMENT US 2017
####Autumn 2023
####ETHZ Eléonore Perret
####dataset containing all infos for camera trap experiment US
##In this code, I want to find a model describing my dataset

#Hi Janneke, Here are some new models with the added variables. 
#Could you check if what you think of them and also I'm not sure if some are normal or not?

#For my models in general, I did the following. 
#1. Check with all the variables
#2. If I see one or more varaibles that are significant I test the model with only them
#3. If the AIC score is lower I take this model, otherwise I test others
#4. I also then check for interactions in general
#5. If some models showed lower AIC score but where not able to run (ie : fail to converge error message), I decided to take the model that did work
#I don't know what you think about this. Is this a good practice?

#Install librairies
library(ggplot2)
library(gridExtra)
library(betareg)
library(lme4)
library(dplyr)

#Results: 
#For the whole dataset: Seed_sp  + Treatment 
#For ABAM:Stand 
#For ABLA:NULL
#For CANO: Stand + Treament
#For PSME:Stand  
#For THPL:Stand*treatment
#For TSHE:Stand*treatment


# Loading the data --------------------------------------------------------
setwd("C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets")
load("data_cleaned_2.RData")

# Data formatting ---------------------------------------------------------
#Changing the name of the dataset for simplicity. 
data<-data_cleaned_2
# Convert the factor to integers
data$Week[data$Week == "First"] <- "1"
data$Week[data$Week == "Second"] <- "2"
data$Week[data$Week == "Third"] <- "3"
data$Week[data$Week == "Fourth"] <- "4"

#I also changed this so I have successes and failures for the binomial analysis
# Create a new variable 'successes' as the seeds_eaten column
data$successes <- data$seeds_eaten
# Create a new variable 'trials' as the seeds_disposed column
data$trials <- data$seeds_disposed

# Linear model: doesn't work (not normal data) ------------------------------------------------------------

# First linear models
testlm_null <- lm(removal_per_all ~ 1, data = data)
testlm_all<-lm(removal_per_all~Stand + Seed_sp+ Treatment, data= data)
summary (testlm_null)
summary(testlm_all)


#Testing the model correctness:
# Plot residuals vs fitted values
plot(testlm_all$fitted.values, testlm_all$residuals,
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted values")
abline(h = 0, col = "red")  # Add horizontal line at 0 for reference
#If the points show no pattern (i.e., are randomly scattered), then the linearity assumption holds.

# Histogram of residuals
hist(testlm_all$residuals, breaks = 20, main = "Histogram of Residuals", xlab = "Residuals")
# Q-Q plot of residuals
qqnorm(testlm_all$residuals)
qqline(testlm_all$residuals, col = "red")
#If the residuals follow a straight line in the Q-Q plot, they are approximately normally distributed.

# # Same as above for residuals vs fitted values
# plot(testlm_all$fitted.values, testlm_all$residuals, 
#      xlab = "Fitted values", ylab = "Residuals",
#      main = "Residuals vs Fitted values")
# abline(h = 0, col = "red")  # Add horizontal line at 0 for reference
# #If the residuals fan out (show a "funnel shape"), this may suggest heteroscedasticity.
# 
# 
# # Check for multicollinearity
# library(car)
# vif(testlm_all)
# #If you find high VIF values, consider removing or combining predictors that are highly correlated.
# 
# # Leverage and Cook's distance
# influence <- influence.measures(testlm_all)
# # Plot Cook's distance
# plot(influence, which = 4)  # Shows Cook's distance
# #High leverage points (outliers) or influential points can affect your model significantly. You can calculate leverage and Cook's distance for each observation.Look for points where Cook's distance is high (above 1) or points with high leverage.
# 
# # Residuals vs fitted values with red line indicating the reference
# plot(testlm_all$fitted.values, testlm_all$residuals,
#      main = "Residuals vs Fitted Values",
#      xlab = "Fitted values", ylab = "Residuals")
# abline(h = 0, col = "red")

#The data is not normal with this model so it doesn't work

# Beta_regression model: doesn't work (not normal data) ---------------------------------------------------
##Try a beta regression
# Issue - cannot handle values that are exactly 0 and exactly 1
# replace them with 0.005, 0.995
data_2 <- data$seeds
data_2[data_2[]==0] <- 0.005
data_2[data_2[]==1] <- 0.995
data$data_2 <- data_2

testbeta_null <- betareg(data_2 ~ 1, data = data)
testbeta_null_all<- betareg(seeds~Stand + Seed_sp+ Treatment+ SI + Nb_trees + Matches_Species, data= data_2)
summary(testbeta_null)
summary(testbeta_null_all)

#It seems that the seed_sp, the stand and the specie diversity have a significant impact

testbeta_sp_st_sd<-betareg(data_2~Stand + Seed_sp+  species_diversity_camera , data= data)
summary (testbeta_sp_st_sd)

plot(testbeta_sp_st_sd)
residuals_test <- residuals(testbeta_sp_st_sd)
shapiro.test(residuals_test)
hist(residuals_test)
qqnorm(residuals_test)
qqline(residuals_test, col = 2)

#Looks normal? or not?


# GLM model: doesn't work (not normal data) ---------------------------------------------------------------

# Lets try a glm
testglm_null <- glm(seeds ~ 1, family="binomial", 
                    weights = seeds_disposed, data=data)
testglm_all <- glm(seeds ~Stand + Seed_sp+ Treatment, family="binomial", 
                    weights = seeds_disposed, data=data)
summary (testglm_null)
summary(testglm_all)


# Shapiro-Wilk test for normality on deviance residuals
shapiro.test(deviance_residuals)
# Q-Q plot for deviance residuals
qqnorm(deviance_residuals)
qqline(deviance_residuals, col = "red")


# Since the outcome is binary, the residuals will not follow a normal distribution. This is why there are other ways of making sure you have a good model. 1. The Deviance of residuals. 2. Pearson residuals 3. Linearity of logit .4 . Influential observations. 5. Model fit 6. Multicollinearity. 

# Check the deviance residuals
deviance_residuals <- residuals(testglm_all, type = "deviance")
summary(deviance_residuals)
#If any of the residuals are large in magnitude, they may indicate outliers or points that the model is struggling to fit
# Check the Pearson residuals
pearson_residuals <- residuals(testglm_all, type = "pearson")
summary(pearson_residuals)
#These will help you assess the overall fit and identify outliers.
# Calculate Cook's Distance
cooks_distance <- cooks.distance(testglm_all)
# Check influential points
influential_points <- which(cooks_distance > (4 / length(cooks_distance)))
influential_points
#If any observations have a Cook's distance significantly higher than the rest, they may be influential and warrant further investigation.
# Residuals vs Fitted values
plot(fitted(testglm_all), deviance_residuals, 
     main = "Residuals vs Fitted", 
     xlab = "Fitted Values", 
     ylab = "Deviance Residuals")
abline(h = 0, col = "red")
# Check linearity of the logit (plotting partial residuals)
library(car)
crPlots(testglm_all)
# Check for overdispersion
dispersion <- sum(residuals(testglm_all, type = "pearson")^2) / df.residual(testglm_all)
dispersion


#GLM is not a good fit: overdispersed and not normally distributed. 


# GLMER model -------------------------------------------------------------

# Lets try the glmer

testglmer_null <- glmer(cbind(successes, trials - successes) ~ 
                          1+ (1 | Camera) +(1|Week), 
                        family = binomial, data =data )
testglmer_all <- glmer(cbind(successes, trials - successes) ~ 
                         Stand + Seed_sp+ Treatment + (1 | Camera) +(1|Week), 
                        family = binomial, data =data )
#The ALL model performs better, lets keep this one
summary(testglmer_all)
#It seems that the seed specie, treatment are significant

testglmer_all_2 <- glmer(cbind(successes, trials - successes) ~ 
                                Seed_sp + Treatment +  (1 | Camera)+(1|Week), 
                              family = binomial, data =data )
testglmer_all_3 <- glmer(cbind(successes, trials - successes) ~ 
                           Seed_sp * Treatment +  (1 | Camera)+(1|Week), 
                         family = binomial, data =data )
testglmer_all_4 <- glmer(cbind(successes, trials - successes) ~ 
                           Seed_sp +  (1 | Camera)+(1|Week), 
                         family = binomial, data =data )
testglmer_all_5 <- glmer(cbind(successes, trials - successes) ~ 
                           Treatment +  (1 | Camera)+(1|Week), 
                         family = binomial, data =data )
testglmer_all_6 <- glmer(cbind(successes, trials - successes) ~ 
                           Stand +  (1 | Camera)+(1|Week), 
                         family = binomial, data =data )
testglmer_all_7 <- glmer(cbind(successes, trials - successes) ~ 
                           Stand + Seed_sp+ (1 | Camera)+(1|Week), 
                         family = binomial, data =data )
testglmer_all_8 <- glmer(cbind(successes, trials - successes) ~ 
                           Stand + Treatment+ (1 | Camera)+(1|Week), 
                         family = binomial, data =data )
testglmer_all_9 <- glmer(cbind(successes, trials - successes) ~ 
                           Treatment * Seed_sp+ (1 | Camera)+(1|Week), 
                         family = binomial, data =data )
testglmer_all_10 <- glmer(cbind(successes, trials - successes) ~ 
                           Stand * Seed_sp+ (1 | Camera)+(1|Week), 
                         family = binomial, data =data )
testglmer_all_11 <- glmer(cbind(successes, trials - successes) ~ 
                           Stand * Seed_sp * Treatment + (1 | Camera)+(1|Week), 
                         family = binomial, data =data )
print(AIC(testglmer_all,testglmer_all_2,testglmer_all_3,testglmer_all_4, testglmer_all_5,, testglmer_all_6, testglmer_all_7, testglmer_all_8, testglmer_all_9, testglmer_all_10, testglmer_all_11))
summary(testglmer_all_6)
#I would say that the best model is the one with seed_sp and treatment. 
summary(testglmer_all_2)
#Let's look at my model now:
# Check the deviance residuals
deviance_residuals <- residuals(testglmer_all_2, type = "deviance")
summary(deviance_residuals)
# Shapiro-Wilk test for normality on deviance residuals
shapiro.test(deviance_residuals)
# Q-Q plot for deviance residuals
qqnorm(deviance_residuals)
qqline(deviance_residuals, col = "red")
# Plot residuals vs fitted values
fitted_values <- fitted(testglmer_all_2)
residuals_pearson <- residuals(testglmer_all_2, type = "pearson")
# Create the plot
plot(fitted_values, residuals_pearson, 
     xlab = "Fitted Values", 
     ylab = "Pearson Residuals", 
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red", lty = 2)  # Add a horizontal line at 0
# Calculate Cook's distance
cooks_distance <- cooks.distance(testglmer_all_2)
# Identify influential points (threshold typically set at 4/n, where n is the number of observations)
influential_points <- which(cooks_distance > (4 / length(cooks_distance)))
# Plot Cook's distance
plot(cooks_distance, type = "h", main = "Cook's Distance", 
     ylab = "Cook's Distance", xlab = "Index")
abline(h = 4 / length(cooks_distance), col = "red", lty = 2)
dispersion <- sum(residuals(testglmer_all_2, type = "pearson")^2) / df.residual(testglmer_all_2)
dispersion
# View random effects
ranef(testglmer_all_2)

#The best model is the one with seed species and treatment but it is not normally distributed.  

# GLMER SPECIES NOTES -----------------------------------------------------
#For the species, I removed the random variable week for ABAM as it produced mutiple error messages when computing the models (too complicated of a model) and also because based on the GLMER model for the whole data, week actually does not have a strong influence on the data variability at least less than camera. So I hope it is ok...

# GLMER binomial species ABAM  --------------------------------------------------
subset_data <- subset(data, Seed_sp == "ABAM")

binomial_model_abam_1 <- glmer(cbind(successes, trials - successes) ~ 
                                 1 +(1 | Camera), 
                               family = binomial, data = subset_data)
binomial_model_abam_2 <- glmer(cbind(successes, trials - successes) ~ 
                                 Stand + (1 | Camera), 
                               family = binomial, data = subset_data)
binomial_model_abam_3 <- glmer(cbind(successes, trials - successes) ~ 
                                 Treatment +(1 | Camera), 
                               family = binomial, data = subset_data)
binomial_model_abam_4 <- glmer(cbind(successes, trials - successes) ~ 
                                 Stand +  Treatment +(1 | Camera), 
                               family = binomial, data = subset_data)
binomial_model_abam_5 <- glmer(cbind(successes, trials - successes) ~ 
                                 Stand *  Treatment +(1 | Camera), 
                               family = binomial, data = subset_data)

print(AIC(binomial_model_abam_1,binomial_model_abam_2,binomial_model_abam_3,binomial_model_abam_4,binomial_model_abam_5))


#It seems that the interaction doesn't create a better model

#Testing normality
par(mfrow= c(1,1))
plot(binomial_model_abam_3)
residuals_test <- residuals(binomial_model_abam_3)
shapiro.test(residuals_test)
hist(residuals_test)
qqnorm(residuals_test)
qqline(residuals_test, col = 2)
#not normal

#For the moment my best model is Stand 

# GLMER binomial species ABLA --------------------------------------------------
subset_data <- subset(data, Seed_sp == "ABLA")

binomial_model_abla_1 <- glmer(cbind(successes, trials - successes) ~ 
                                 1 +(1 | Camera)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_abla_2 <- glmer(cbind(successes, trials - successes) ~ 
                                 Stand + (1 | Camera)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_abla_3 <- glmer(cbind(successes, trials - successes) ~ 
                                 Treatment +(1 | Camera)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_abla_4 <- glmer(cbind(successes, trials - successes) ~ 
                                 Stand +  Treatment +(1 | Camera)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_abla_5 <- glmer(cbind(successes, trials - successes) ~ 
                                 Stand *  Treatment +(1 | Camera)+ (1|Week), 
                               family = binomial, data = subset_data)

# Without the WEEK as a randow variable : Still Model 1 the best. 
# binomial_model_abla_1 <- glmer(cbind(successes, trials - successes) ~ 
#                                  1 +(1 | Camera), 
#                                family = binomial, data = subset_data)
# binomial_model_abla_2 <- glmer(cbind(successes, trials - successes) ~ 
#                                  Stand + (1 | Camera), 
#                                family = binomial, data = subset_data)
# binomial_model_abla_3 <- glmer(cbind(successes, trials - successes) ~ 
#                                  Treatment +(1 | Camera), 
#                                family = binomial, data = subset_data)
# binomial_model_abla_4 <- glmer(cbind(successes, trials - successes) ~ 
#                                  Stand +  Treatment +(1 | Camera), 
#                                family = binomial, data = subset_data)
# binomial_model_abla_5 <- glmer(cbind(successes, trials - successes) ~ 
#                                  Stand *  Treatment +(1 | Camera), 
#                                family = binomial, data = subset_data)


print(AIC(binomial_model_abla_1,binomial_model_abla_2,binomial_model_abla_3,binomial_model_abla_4,binomial_model_abla_5))

#it seems that the null model is the best model

#Testing normality
residuals_test <- residuals(binomial_model_abla_4)
shapiro.test(residuals_test)
hist(residuals_test)
qqnorm(residuals_test)
qqline(residuals_test, col = 2)
#Also not normal?

#Best model: NULL

# GLMER binomial species CANO --------------------------------------------------
subset_data <- subset(data, Seed_sp == "CANO")

binomial_model_cano_1 <- glmer(cbind(successes, trials - successes) ~ 
                                 1 +(1 | Camera)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_cano_2 <- glmer(cbind(successes, trials - successes) ~ 
                                 Stand + (1 | Camera)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_cano_3 <- glmer(cbind(successes, trials - successes) ~ 
                                 Treatment +(1 | Camera)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_cano_4 <- glmer(cbind(successes, trials - successes) ~ 
                                 Stand +  Treatment +(1 | Camera)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_cano_5 <- glmer(cbind(successes, trials - successes) ~ 
                                 Stand *  Treatment +(1 | Camera)+ (1|Week), 
                               family = binomial, data = subset_data)

#Without the week. 
# binomial_model_cano_1 <- glmer(cbind(successes, trials - successes) ~ 
#                                  1 +(1 | Camera), 
#                                family = binomial, data = subset_data)
# binomial_model_cano_2 <- glmer(cbind(successes, trials - successes) ~ 
#                                  Stand + (1 | Camera), 
#                                family = binomial, data = subset_data)
# binomial_model_cano_3 <- glmer(cbind(successes, trials - successes) ~ 
#                                  Treatment +(1 | Camera), 
#                                family = binomial, data = subset_data)
# binomial_model_cano_4 <- glmer(cbind(successes, trials - successes) ~ 
#                                  Stand +  Treatment +(1 | Camera), 
#                                family = binomial, data = subset_data)
# binomial_model_cano_5 <- glmer(cbind(successes, trials - successes) ~ 
#                                  Stand *  Treatment +(1 | Camera), 
#                                family = binomial, data = subset_data)

print(AIC(binomial_model_cano_1,binomial_model_cano_2,binomial_model_cano_3,binomial_model_cano_4,binomial_model_cano_5 ))

summary(binomial_model_cano_4)
#Looks like the best one is still number 2: Stand

#Testing normality
residuals_test <- residuals(binomial_model_cano_4)
shapiro.test(residuals_test)
hist(residuals_test)
qqnorm(residuals_test)
qqline(residuals_test, col = 2)

#THe best model for CANO is Stand or Stand + Treament (depending if we keep or not the week as a random variable)


# GLMER binomial species PSME --------------------------------------------------
subset_data <- subset(data, Seed_sp == "PSME")

binomial_model_psme_1 <- glmer (cbind(successes, trials - successes) ~ 
                                 1+ (1|Camera)+(1 | Week) , 
                               family = binomial, data = subset_data)
binomial_model_psme_2 <- glmer (cbind(successes, trials - successes) ~ 
                                 Stand + (1|Camera)+(1 | Week) , 
                               family = binomial, data = subset_data)
binomial_model_psme_3 <- glmer (cbind(successes, trials - successes) ~ 
                                 Treatment + (1|Camera)+(1 | Week), 
                               family = binomial, data = subset_data)
binomial_model_psme_4 <- glmer(cbind(successes, trials - successes) ~ 
                                 Stand +  Treatment+ (1|Camera)+(1 | Week) , 
                               family = binomial, data = subset_data)
binomial_model_psme_5 <- glmer(cbind(successes, trials - successes) ~ 
                                 Stand *  Treatment + (1|Camera)+(1 | Week) , 
                               family = binomial, data = subset_data)
# binomial_model_psme_6 <- glmer(cbind(successes, trials - successes) ~ 
#                                  Matches_Species +(1 | Camera) , 
#                                family = binomial, data = subset_data)
# binomial_model_psme_7 <- glmer(cbind(successes, trials - successes) ~ 
#                                  Matches_Species+ Treatment +(1 | Camera) , 
#                                family = binomial, data = subset_data)
# binomial_model_psme_8 <- glmer(cbind(successes, trials - successes) ~ 
#                                  Matches_Species + Stand +(1 | Camera) , 
#                                family = binomial, data = subset_data)
# binomial_model_psme_9 <- glmer(cbind(successes, trials - successes) ~ 
#                                  Matches_Species + Treatment + Stand +(1 | Camera), 
#                                family = binomial, data = subset_data)

print(AIC(binomial_model_psme_1,binomial_model_psme_2,binomial_model_psme_3,binomial_model_psme_4,binomial_model_psme_5))

summary(binomial_model_psme_2)
#Testing normality
print(binomial_model_psme_3)
residuals_test <- residuals(binomial_model_psme_3)
shapiro.test(residuals_test)
hist(residuals_test)
qqnorm(residuals_test)
qqline(residuals_test, col = 2)

#I still think that the treatment is the best. Or null (statistically more significant). Maybe check with Janneke? But the model really doesn't look normal at all... 


# GLMER binomial species THPL --------------------------------------------------
subset_data <- subset(data, Seed_sp == "THPL")

binomial_model_thpl_1 <- glmer(cbind(successes, trials - successes) ~ 
                                 1 +(1 | Camera)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_thpl_2 <- glmer(cbind(successes, trials - successes) ~ 
                                 Stand + (1 | Camera)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_thpl_3 <- glmer(cbind(successes, trials - successes) ~ 
                                 Treatment +(1 | Camera)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_thpl_4 <- glmer(cbind(successes, trials - successes) ~ 
                                 Stand +  Treatment +(1 | Camera)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_thpl_5 <- glmer(cbind(successes, trials - successes) ~ 
                                 Stand *  Treatment +(1 | Camera)+ (1|Week), 
                               family = binomial, data = subset_data)

print(AIC(binomial_model_thpl_1,binomial_model_thpl_2,binomial_model_thpl_3,binomial_model_thpl_4,binomial_model_thpl_5))

summary(binomial_model_thpl_5)


#Model 4 is still the lowest model.

#Testing normality
residuals_test <- residuals(binomial_model_thpl_5)
shapiro.test(residuals_test)
hist(residuals_test)
qqnorm(residuals_test)
qqline(residuals_test, col = 2)

#Species_diversity*treatment
#It seems that for thpl, the interaction between stand and treatment explains the most the variability


# GLMER binomial species TSHE --------------------------------------------------
subset_data <- subset(data, Seed_sp == "TSHE")

binomial_model_tshe_1 <- glmer(cbind(successes, trials - successes) ~ 
                                 1 +(1 | Camera)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_tshe_2 <- glmer(cbind(successes, trials - successes) ~ 
                                 Stand + (1 | Camera)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_tshe_3 <- glmer(cbind(successes, trials - successes) ~ 
                                 Treatment +(1 | Camera)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_tshe_4 <- glmer(cbind(successes, trials - successes) ~ 
                                 Stand +  Treatment +(1 | Camera)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_tshe_5 <- glmer(cbind(successes, trials - successes) ~ 
                                 Treatment *  Stand +(1 | Camera)+ (1|Week), 
                               family = binomial, data = subset_data)

print(AIC(binomial_model_tshe_1,binomial_model_tshe_2,binomial_model_tshe_3,binomial_model_tshe_4,binomial_model_tshe_5))

summary(binomial_model_tshe_5)


#Testing normality
residuals_test <- residuals(binomial_model_tshe_5)
shapiro.test(residuals_test)
hist(residuals_test)
qqnorm(residuals_test)
qqline(residuals_test, col = 2)

#For tshe it seems that site* treatment explains the most variance






