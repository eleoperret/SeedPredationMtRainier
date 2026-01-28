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

#Results: 
#For the whole dataset: Seed_sp  + Treatment * species_diversity_camera
#For ABAM:Stand * species diversity per camera
#For ABLA:NULL
#For CANO:Stand*Specie_diversity+Treatment
#For PSME:Treatment* number of trees
#For THPL:Species_diversity*treatment
#For TSHE:site* treatment


# Loading the data --------------------------------------------------------
setwd("C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets")
load ("conspecific_with_dbh.RData")
# Data formatting ---------------------------------------------------------
#Changing the variables into characters (is that ok? That is what Ruben recommanded me)
# as.numeric(data_merged_6$number_of_trees)
# as.numeric(data_merged_6$species_diversity_camera)

#Changing the name of the dataset for simplicity. 
data<-conspecific_with_dbh

#Making the dbh mean normal (same here? Ruben and Billur told me I should normalize this)
ggplot(data, aes(x=log(mean_dbh_y))) +
  geom_density(fill="blue", alpha=0.5) +
  labs(title="Density Plot of Week Variable", x="Number_of_trees")
log_dbh<-log(data$mean_dbh_y)
residuals_test <- residuals(data$log_dbh)
shapiro.test(log_dbh)
hist(log_dbh)
qqnorm(log_dbh)
qqline(log_dbh, col = 2)

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
testlm_all<-lm(removal_per_all~Stand + Seed_sp+ Treatment+ SI + Nb_trees + Matches_Species, data= data)
summary (testlm_null)
summary(testlm_all)

#It seems that the seed_sp, the stand and the specie diversity have a significant impact

testlm_sp_st_sd<-lm(removal_per_all~Stand + Seed_sp+  species_diversity_camera , data= data)
summary (testlm_sp_st_sd)

plot(testlm_sp_st_sd)
residuals_test <- residuals(testlm_sp_st_sd)
shapiro.test(residuals_test)
hist(residuals_test)
qqnorm(residuals_test)
qqline(residuals_test, col = 2)

#The data is not normal with this model so it doesn't work

# Beta_regression model: doesn't work (not normal data) ---------------------------------------------------
##Try a beta regression
# Issue - cannot handle values that are exactly 0 and exactly 1
# replace them with 0.005, 0.995
removal_per_all_2 <- data$seeds
removal_per_all_2[removal_per_all_2[]==0] <- 0.005
removal_per_all_2[removal_per_all_2[]==1] <- 0.995
data$removal_per_all_2 <- removal_per_all_2

testbeta_null <- betareg(removal_per_all_2 ~ 1, data = data)
testbeta_null_all<- betareg(removal_per_all_2 ~ Stand + Seed_sp+ Treatment+ log_dbh+ species_diversity_camera + number_of_trees, data = data)
summary(testbeta_null)
summary(testbeta_null_all)

#It seems that the seed_sp, the stand and the specie diversity have a significant impact

testbeta_sp_st_sd<-betareg(removal_per_all_2~Stand + Seed_sp+  species_diversity_camera , data= data)
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
testglm_null <- glm(removal_per_all_2 ~ 1, family="binomial", 
                    weights = seeds_disposed, data=data)
testglm_all <- glm(removal_per_all_2 ~Stand + Seed_sp+ Treatment+ log_dbh+ species_diversity_camera + number_of_trees, family="binomial", 
                    weights = seeds_disposed, data=data)
summary (testglm_null)
summary(testglm_all)

#Seems like the seed_sp, treatment, site, specie diversity and number of trees are significant 
#lets test

testglm_all_2 <- glm(removal_per_all_2 ~Stand + Seed_sp+ Treatment+ species_diversity_camera + number_of_trees, family="binomial", 
                   weights = seeds_disposed, data=data)
summary(testglm_all_2)

#Seems better.

plot(testglm_all_2)
residuals_test <- residuals(testglm_all_2)
shapiro.test(residuals_test)
hist(residuals_test)
qqnorm(residuals_test)
qqline(residuals_test, col = 2)

#Also not normal

# GLMER model -------------------------------------------------------------

# Lets try the glmer

testglmer_null <- glmer(cbind(successes, trials - successes) ~ 
                          1+ (1 | Camera.number) +(1|Week), 
                        family = binomial, data =data )
testglmer_all <- glmer(cbind(successes, trials - successes) ~ 
                          Seed_sp + log_dbh + Stand + Treatment + species_diversity_camera+ number_of_trees + (1 | Camera.number) +(1|Week), 
                        family = binomial, data =data )
summary (testglmer_null)
summary(testglmer_all)

#It seems that the seed specie, treatment and specie diversity are significant

testglmer_all_2 <- glmer(cbind(successes, trials - successes) ~ 
                                Seed_sp + Treatment + species_diversity_camera+  (1 | Camera.number), 
                              family = binomial, data =data )
summary(testglmer_all_2)
#Doesn't perform better and model doesn't work

testglmer_all_3 <- glmer(cbind(successes, trials - successes) ~ 
                           Seed_sp + Treatment +  (1 | Camera.number), 
                         family = binomial, data =data )
summary(testglmer_all_3)
#Neither

print(AIC(testglmer_null))
print(AIC(testglmer_all))
print(AIC(testglmer_all_2))
print(AIC(testglmer_all_3))

#For the moment the best is with every variable

#let's check for interactions: 
testglmer_int1 <- glmer(cbind(successes, trials - successes) ~ 
                         Seed_sp * log_dbh * Stand * Treatment * species_diversity_camera* number_of_trees + (1 | Camera.number) +(1|Week), 
                       family = binomial, data =data )
testglmer_int2 <- glmer(cbind(successes, trials - successes) ~ 
                         Seed_sp  * Treatment * species_diversity_camera + (1 | Camera.number) +(1|Week), 
                       family = binomial, data =data )
testglmer_int3 <- glmer(cbind(successes, trials - successes) ~ 
                          Seed_sp  * Treatment + species_diversity_camera + (1 | Camera.number) +(1|Week), 
                        family = binomial, data =data )
testglmer_int4 <- glmer(cbind(successes, trials - successes) ~ 
                          Seed_sp  + Treatment * species_diversity_camera + (1 | Camera.number) +(1|Week), 
                        family = binomial, data =data )
testglmer_int5 <- glmer(cbind(successes, trials - successes) ~ 
                          Seed_sp  * Treatment  + (1 | Camera.number) +(1|Week), 
                        family = binomial, data =data )
print(AIC(testglmer_all,testglmer_all_2,testglmer_all_3,testglmer_int2,testglmer_int3,testglmer_int4,testglmer_int5))

#int 1,2,3 not working

#model int_4 is the only one that does not create an error message. 
#I think I should take that one as it has a lower AIC than the "all" model. 
#This means that the Seed_sp + Treatmetn and the interaction between Treatment and species diversity is the best model 


# GLMER per species ABAM  --------------------------------------------------

subset_data <- subset(data, Seed_sp == "ABAM")

binomial_model_abam_1 <- glmer(cbind(successes, trials - successes) ~ 
                          log_dbh + Stand + Treatment + species_diversity_camera+ number_of_trees + (1 | Camera.number) +( 1|Week), 
                        family = binomial, data =subset_data )
summary(binomial_model_abam_1)

#It seems that stand and species diversity have a significant effect

binomial_model_abam_2 <- glmer(cbind(successes, trials - successes) ~ 
                               Stand + species_diversity_camera+  (1 | Camera.number)+( 1|Week), 
                             family = binomial, data =subset_data )
summary(binomial_model_abam_2)

#This model has a lower AIC score

#Testing interactions and others
binomial_model_abam_3 <- glmer(cbind(successes, trials - successes) ~ 
                                 Stand * species_diversity_camera+  (1 | Camera.number)+( 1|Week), 
                               family = binomial, data =subset_data )
binomial_model_abam_4 <- glmer(cbind(successes, trials - successes) ~ 
                                 Stand +  (1 | Camera.number)+( 1|Week), 
                               family = binomial, data =subset_data )
binomial_model_abam_5 <- glmer(cbind(successes, trials - successes) ~ 
                                 species_diversity_camera +  (1 | Camera.number)+( 1|Week), 
                               family = binomial, data =subset_data )
print(AIC(binomial_model_abam_2,binomial_model_abam_3, binomial_model_abam_4, binomial_model_abam_5))

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

#For the moment my best model is Stand * species diversity per camera

# GLMER binomial species ABLA --------------------------------------------------

subset_data <- subset(data, Seed_sp == "ABLA")

binomial_model_abla_1 <- glmer(cbind(successes, trials - successes) ~ 
                                 log_dbh+Stand + species_diversity_camera + Treatment + number_of_trees +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
summary(binomial_model_abla_1)
#stand, diversity and treamtent 

binomial_model_abla_2 <- glmer(cbind(successes, trials - successes) ~ 
                                    Stand + species_diversity_camera + Treatment  +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
summary(binomial_model_abla_2)

binomial_model_abla_3 <- glmer(cbind(successes, trials - successes) ~ 
                                 Stand +  Treatment  +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
summary(binomial_model_abla_3)

binomial_model_abla_4 <- glmer(cbind(successes, trials - successes) ~ 
                                 1+(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_abla_5 <- glmer(cbind(successes, trials - successes) ~ 
                                 Stand +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_abla_6 <- glmer(cbind(successes, trials - successes) ~ 
                                 Treatment  +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_abla_7 <- glmer(cbind(successes, trials - successes) ~ 
                                 species_diversity_camera  +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
print(AIC(binomial_model_abla_1,binomial_model_abla_2,binomial_model_abla_3,binomial_model_abla_4,binomial_model_abla_5,binomial_model_abla_6,binomial_model_abla_7))

#it seems that the null model is the best model
#let's test for interaction
binomial_model_abla_3_int <- glmer(cbind(successes, trials - successes) ~ 
                                 Stand *  Treatment  +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
print(AIC(binomial_model_abla_3,binomial_model_abla_3_int,binomial_model_abla_4))

#It seems that the null model is still the best.

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
                                 log_dbh + Stand + number_of_trees + species_diversity_camera + Treatment +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
summary(binomial_model_cano_1)

#It seems that the stand , species diversity and treatment have an effect.
#LEt's try

binomial_model_cano_2 <- glmer(cbind(successes, trials - successes) ~ 
                                 Stand + species_diversity_camera + Treatment +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
summary(binomial_model_cano_2)
binomial_model_cano_2b <- glmer(cbind(successes, trials - successes) ~ 
                                 Stand  + Treatment +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_cano_2c <- glmer(cbind(successes, trials - successes) ~ 
                                species_diversity_camera + Treatment +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_cano_2d <- glmer(cbind(successes, trials - successes) ~ 
                                 Stand + species_diversity_camera  +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_cano_2e <- glmer(cbind(successes, trials - successes) ~ 
                                  Stand  +(1 | Camera.number)+ (1|Week), 
                                family = binomial, data = subset_data)
binomial_model_cano_2f <- glmer(cbind(successes, trials - successes) ~ 
                                 species_diversity_camera  +(1 | Camera.number)+ (1|Week), 
                                family = binomial, data = subset_data)
binomial_model_cano_2g <- glmer(cbind(successes, trials - successes) ~ 
                                  Treatment  +(1 | Camera.number)+ (1|Week), 
                                family = binomial, data = subset_data)
print(AIC(binomial_model_cano_2,binomial_model_cano_2b,binomial_model_cano_2c,binomial_model_cano_2d,binomial_model_cano_2e,binomial_model_cano_2f,binomial_model_cano_2g))


#Looks like the best one is still number 2: Stand, species and Treatment. better, now let's check for interactions?

binomial_model_cano_3 <- glmer(cbind(successes, trials - successes) ~ 
                                 Stand * species_diversity_camera + Treatment +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_cano_4 <- glmer(cbind(successes, trials - successes) ~ 
                                 Stand + species_diversity_camera * Treatment +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_cano_5 <- glmer(cbind(successes, trials - successes) ~ 
                                 Stand * species_diversity_camera * Treatment +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
#Model 5 is not working?
print(AIC(binomial_model_cano_2,binomial_model_cano_3,binomial_model_cano_4))

#I don't know as there is a problem with n°5 if it is ok to take n°3. 

#Testing normality
residuals_test <- residuals(binomial_model_cano_3)
shapiro.test(residuals_test)
hist(residuals_test)
qqnorm(residuals_test)
qqline(residuals_test, col = 2)

#THe best model for CANO is Stand*Specie_diversity+Treatment


# GLMER binomial species PSME --------------------------------------------------

subset_data <- subset(data, Seed_sp == "PSME")

binomial_model_psme_1 <- glmer(cbind(successes, trials - successes) ~ 
                                 log_dbh + Stand + number_of_trees + species_diversity_camera + Treatment +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
summary(binomial_model_psme_1)
#Seems like only number of trees is significant
binomial_model_psme_2 <- glmer(cbind(successes, trials - successes) ~ 
                                 number_of_trees +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_psme_3 <- glmer(cbind(successes, trials - successes) ~ 
                                 Stand + number_of_trees  +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
print(AIC(binomial_model_psme_1,binomial_model_psme_2,binomial_model_psme_3))
#It seems that there is a problem with the model 1 and 3 (large eigenvalue ratio)
#Let's try without the number of trees

binomial_model_psme_3 <- glmer(cbind(successes, trials - successes) ~ 
                                 log_dbh + Stand + species_diversity_camera + Treatment +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
summary(binomial_model_psme_3)
#same problem
#let's check now each variable individually.


binomial_model_psme_4 <- glmer(cbind(successes, trials - successes) ~ 
                                 Stand+ (1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_psme_5 <- glmer(cbind(successes, trials - successes) ~ 
                                 Treatment+ (1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_psme_6 <- glmer(cbind(successes, trials - successes) ~ 
                                 log_dbh+ (1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
print(AIC(binomial_model_psme_2,binomial_model_psme_4,binomial_model_psme_5,binomial_model_psme_6))

#when running: model 4 problem
#so let's keep the model that do not have a problem = 2, 5 and 6
#which means number of trees, treatment and log.dbh

#let's see with them
binomial_model_psme_7 <- glmer(cbind(successes, trials - successes) ~ 
                                 log_dbh+ number_of_trees+ Treatment+ (1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_psme_8 <- glmer(cbind(successes, trials - successes) ~ 
                                 log_dbh+ number_of_trees+(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_psme_9 <- glmer(cbind(successes, trials - successes) ~ 
                                 log_dbh+ Treatment+(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_psme_10 <- glmer(cbind(successes, trials - successes) ~ 
                                 number_of_trees+ Treatment+(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_psme_10b <- glmer(cbind(successes, trials - successes) ~ 
                                Stand+ Treatment+(1 | Camera.number)+ (1|Week), 
                                family = binomial, data = subset_data)
print(AIC(binomial_model_psme_2,binomial_model_psme_5,binomial_model_psme_6,binomial_model_psme_7,binomial_model_psme_8,binomial_model_psme_9,binomial_model_psme_10,binomial_model_psme_10b))
#no problem with those models (except for 10b). 2,5,6 have the lowest score still
#let's check for interactions
binomial_model_psme_11 <- glmer(cbind(successes, trials - successes) ~ 
                                 log_dbh* number_of_trees* Treatment+ (1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_psme_12 <- glmer(cbind(successes, trials - successes) ~ 
                                 log_dbh* number_of_trees+(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_psme_13 <- glmer(cbind(successes, trials - successes) ~ 
                                 log_dbh* Treatment+(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_psme_14 <- glmer(cbind(successes, trials - successes) ~ 
                                  number_of_trees* Treatment+(1 | Camera.number)+ (1|Week), 
                                family = binomial, data = subset_data)
print(AIC(binomial_model_psme_2,binomial_model_psme_5,binomial_model_psme_6,binomial_model_psme_7,binomial_model_psme_8,binomial_model_psme_9,binomial_model_psme_10,binomial_model_psme_11,binomial_model_psme_12,binomial_model_psme_13,binomial_model_psme_14))

#model 11 doesn't work
#model 14 seems the best: Treatment* number of trees

summary (binomial_model_psme_14)

#Testing normality
print(binomial_model_psme_14)
residuals_test <- residuals(binomial_model_psme_14)
shapiro.test(residuals_test)
hist(residuals_test)
qqnorm(residuals_test)
qqline(residuals_test, col = 2)

#Is it the best one, doesn't seem normal?

# GLMER binomial species THPL --------------------------------------------------

subset_data <- subset(data, Seed_sp == "THPL")

binomial_model_thpl_1 <- glmer(cbind(successes, trials - successes) ~ 
                                 log_dbh + Stand + number_of_trees + species_diversity_camera + Treatment +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)

summary (binomial_model_thpl_1)
#Seems like only species diversity matters

binomial_model_thpl_2 <- glmer(cbind(successes, trials - successes) ~ 
                                 species_diversity_camera +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
summary(binomial_model_thpl_2)

binomial_model_thpl_null <- glmer(cbind(successes, trials - successes) ~ 
                                 1 +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
summary(binomial_model_thpl_null)

print(AIC(binomial_model_thpl_1,binomial_model_thpl_2,binomial_model_thpl_null))

#it seems that only species diversity has an effect

#Now let's test interactions?
binomial_model_thpl_3 <- glmer(cbind(successes, trials - successes) ~ 
                                 species_diversity_camera* Stand +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_thpl_4 <- glmer(cbind(successes, trials - successes) ~ 
                                 species_diversity_camera* Treatment +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_thpl_5 <- glmer(cbind(successes, trials - successes) ~ 
                                 species_diversity_camera *log_dbh +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_thpl_6 <- glmer(cbind(successes, trials - successes) ~ 
                                 species_diversity_camera* number_of_trees +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)

print(AIC(binomial_model_thpl_2,binomial_model_thpl_3,binomial_model_thpl_4,binomial_model_thpl_5,binomial_model_thpl_6))

#it seems that model 4 the treatment* species_diversity per camera have the lowest AIC score.
#testing just treatment then, and treatment + specie diversity.
binomial_model_thpl_7 <- glmer(cbind(successes, trials - successes) ~ 
                                 Treatment +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_thpl_8 <- glmer(cbind(successes, trials - successes) ~ 
                                 species_diversity_camera+ Treatment +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)

print(AIC(binomial_model_thpl_4,binomial_model_thpl_7,binomial_model_thpl_8))

#Model 4 is still the lowest model.

#Testing normality
residuals_test <- residuals(binomial_model_thpl_4)
shapiro.test(residuals_test)
hist(residuals_test)
qqnorm(residuals_test)
qqline(residuals_test, col = 2)

#Species_diversity*treatment
#It seems that for thpl, the interaction between specie_diversity and treatment explains the most the variability


# GLMER binomial species TSHE --------------------------------------------------

subset_data <- subset(data, Seed_sp == "TSHE")

binomial_model_tshe_1 <- glmer(cbind(successes, trials - successes) ~ 
                                 log_dbh + Stand + number_of_trees + species_diversity_camera + Treatment +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)

summary (binomial_model_tshe_1)
#based on the result only the treatment seems to have an effect
binomial_model_tshe_2 <- glmer(cbind(successes, trials - successes) ~ 
                                 Treatment +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
summary (binomial_model_tshe_2)

print(AIC(binomial_model_tshe_1,binomial_model_tshe_2))

#Now let's check for interactions
binomial_model_tshe_3 <- glmer(cbind(successes, trials - successes) ~ 
                                 log_dbh *Treatment +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_tshe_4 <- glmer(cbind(successes, trials - successes) ~ 
                                 Stand * Treatment +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_tshe_5 <- glmer(cbind(successes, trials - successes) ~ 
                                 number_of_trees *Treatment +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_tshe_6 <- glmer(cbind(successes, trials - successes) ~ 
                                 species_diversity_camera * Treatment +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)

print(AIC(binomial_model_tshe_1,binomial_model_tshe_2, binomial_model_tshe_3,binomial_model_tshe_4,binomial_model_tshe_5,binomial_model_tshe_6))

summary(binomial_model_tshe_4)

#It seems that the best model is stand*treatment

#Let's check now just stand and stand+ treatment
binomial_model_tshe_7 <- glmer(cbind(successes, trials - successes) ~ 
                                 Stand  +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
binomial_model_tshe_8 <- glmer(cbind(successes, trials - successes) ~ 
                                 Stand + Treatment +(1 | Camera.number)+ (1|Week), 
                               family = binomial, data = subset_data)
print(AIC(binomial_model_tshe_4,binomial_model_tshe_7,binomial_model_tshe_8))
#Model 4 is still the best
#Testing normality
residuals_test <- residuals(binomial_model_tshe_4)
shapiro.test(residuals_test)
hist(residuals_test)
qqnorm(residuals_test)
qqline(residuals_test, col = 2)

#For tshe it seems that site* treatment explains the most variance






