library("dplyr")
#Testing for collinearity
head(data_merged_6)
unique(colname(data_merged_6))
print(colnames(data_merged_6))


hist(resid(data_merged_6$removal_per_all))

# Convert non-numeric response variables to factors
data_subset$Stand <- factor(data_merged_6$Stand)
data_subset$Seed_sp <- factor(data_merged_6$Seed_sp)
data_subset$Treatment <- factor(data_merged_6$Treatment)
data_subset$canopy_class <- factor(data_merged_6$canopy_class)
data_subset$Matches_Species <- factor(data_merged_6$Matches_Species)


##PREPROCESSING PREDICTOR VARIABLES AND TESTING MODEL
# Select relevant predictor variables from the dataset
predictors <- c("Shannon_Index_5m", "mean_dbh_y", "number_of_trees")
predictors_2 <- c("Shannon_Index_5m", "mean_dbh_y", "number_of_trees","Stand","Seed_sp","Treatment","canopy_class","Matches_Species")

# Subset the dataset to include only selected predictor variables and the response variable
data_subset <- data_merged_6[, c(predictors_2, "removal_per_all")]

# List of column names to convert to numeric
columns_to_convert <- c("Shannon_Index_5m", "mean_dbh_y", "number_of_trees")

# Convert specified columns to numeric using mutate_at()
data_subset <- data_subset %>%
  mutate_at(vars(columns_to_convert), as.numeric)

# Center and standardize the predictor variables
data_standardized <- scale(data_subset[, predictors], center = TRUE, scale = TRUE)

# Combine standardized predictors with the response variable
data_standardized <- cbind(data_standardized, removal_per_all = data_subset$removal_per_all)

# Convert matrix data_standardized to dataframe
data_df <- as.data.frame(data_standardized)

# Verify the dataframe structure
str(data_df)

# Fit linear regression model using dataframe
model <- lm(removal_per_all ~ ., data = data_df)

# Summary of the model
summary(model)

# Display coefficients and their interpretations
coefficients <- coef(model)
print(coefficients)

##TESTING FOR NORMALITY AND CONSTANT VARIANCE OF THE MODEL
# Histogram of residuals
hist(resid(model), breaks = 20, main = "Histogram of Residuals")

# QQ plot of residuals
qqnorm(resid(model))
qqline(resid(model))

# Shapiro-Wilk test for normality
shapiro.test(resid(model))

#CONSTANT VARIANCE
# Residuals vs. Fitted plot
plot(fitted(model), resid(model), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red", lty = 2)  # Add horizontal line at y = 0


# Install and load the 'lmtest' package (if not already installed)
install.packages("lmtest")
library(lmtest)

# Breusch-Pagan test for homoscedasticity
bptest(model)


##TESTING FOR COLLINEARITY OF PREDICTOR VARIABLES
# Compute correlation matrix
cor_matrix <- cor(data_subset[, predictors])

# Display correlation matrix
print(cor_matrix)

# Plot correlation matrix (optional)
library(corrplot)
corrplot(cor_matrix, method = "circle")

# Install and load the 'car' package (if not already installed)
install.packages("car")
library(car)

# Calculate VIF for predictor variables
vif_values <- vif(model)

# Display VIF values
print(vif_values)

# Calculate condition number
condition_number <- sqrt(max(eigen(cor_matrix)$values) / min(eigen(cor_matrix)$values))

# Display condition number
print(condition_number)

# Install and load the 'GGally' package (if not already installed)
install.packages("GGally")
library(GGally)

# Pairwise scatterplot of predictor variables
ggpairs(data_subset[, predictors])

# Compute eigenvalues of the correlation matrix
eigen_values <- eigen(cor_matrix)$values

# Display eigenvalues
print(eigen_values)


##With all predictor variables. 
predictors <- c("Shannon_Index_5m", "mean_dbh_y", "number_of_trees","Stand","Seed_sp","Treatment","canopy_class","Matches_Species")

# Define the linear regression model formula with dataset's predictor variables and independent variables
model_1 <- lm(removal_per_all ~ Shannon_Index_5m + mean_dbh_y + number_of_trees + Stand + Seed_sp + Treatment + Matches_Species, data = data_subset)

# Summary of the model
summary(model_1)

levels(data_subset$Stand)


# Filter rows where Stand is TO04
subset_TO04 <- data_subset[data_subset$Stand == "TO04", ]

# Print the values of removal_per_all for Stand TO04
print(subset_TO04$removal_per_all)

subset_TO04
is.na(subset_TO04$removal_per_all)
which
data_subset$removal_per_all
  
  
#Normality
hist(resid(model_1))
qqnorm(resid(model_1))
qqline(resid(model_1))


# Fit the linear regression model
model <- lm(removal_per_all ~ Shannon_Index_5m + mean_dbh_y + number_of_trees + Stand + Seed_sp + Treatment + Matches_Species, data = data_subset)

# Summary of the model
summary(model)

model <- lm(removal_per_all ~ Stand, data = data_subset)
summary(model)

#Changing the reference variable
# Suppose 'Site' is a factor variable with levels A, B, C
# We want to set 'Site' level B as the reference category

# Reorder the levels of 'Site' with B as the first level
data_subset$Stand <- factor(data_subset$Stand, levels = c("AV06", "TO04", "AE10"))
model <- lm(removal_per_all ~ Shannon_Index_5m + mean_dbh_y + number_of_trees + Stand + Seed_sp + Treatment + Matches_Species, data = data_subset)
summary(model)

# Check distribution of 'Site' levels
table(data_subset$Stand)
# Plotting frequency of 'Stand' levels
barplot(table(data_subset$Stand), 
        main = "Frequency of Stand Levels",
        xlab = "Stand Levels",
        ylab = "Frequency")
table(data_subset$Seed_sp)


#Model for each site
AE10_site<- subset(data_subset,Stand=="AE10")
AV06_site<- subset(data_subset, Stand="AV06")
TO04_site<- subset(data_subset,Stand="TO04")


#Shapiro test
# Define function to perform Shapiro-Wilk test and print results
perform_shapiro_test <- function(data, subset_name) {
  shapiro_test <- shapiro.test(data)
  cat("Shapiro-Wilk Test for Normality -", subset_name, "\n")
  cat("Test Statistic:", shapiro_test$statistic, "\n")
  cat("p-value:", shapiro_test$p.value, "\n\n")
  
  # Interpret the p-value
  if (shapiro_test$p.value < 0.05) {
    cat("Conclusion: Data are not normally distributed (reject null hypothesis)\n")
  } else {
    cat("Conclusion: Data are normally distributed (fail to reject null hypothesis)\n")
  }
}

# Perform Shapiro-Wilk test for 'removal_per_all' within each subset
perform_shapiro_test(AE10_site$removal_per_all, "AE10_site")
perform_shapiro_test(AV06_site$removal_per_all, "AV06_site")
perform_shapiro_test(TO04_site$removal_per_all, "TO04_site")

# Define function to create Q-Q plot
plot_qq <- function(data, subset_name) {
  qqnorm(data, main = paste("Q-Q Plot for", subset_name))
  qqline(data, col = "red")  # Add a reference line for normal distribution
}

# Create Q-Q plot for 'removal_per_all' within each subset
par(mfrow = c(1, 3))  # Set up a 1x3 layout for plots

# Q-Q plot for AE10_site
plot_qq(AE10_site$removal_per_all, "AE10_site")

# Q-Q plot for AV06_site
plot_qq(AV06_site$removal_per_all, "AV06_site")

# Q-Q plot for TO04_site
plot_qq(TO04_site$removal_per_all, "TO04_site")

# Define function to create histogram
plot_histogram <- function(data, subset_name) {
  hist(data, breaks = "FD", main = paste("Histogram for", subset_name),
       xlab = "removal_per_all", ylab = "Frequency", col = "skyblue", border = "black")
}

# Create histogram for 'removal_per_all' within each subset
par(mfrow = c(1, 3))  # Set up a 1x3 layout for plots

# Histogram for AE10_site
plot_histogram(AE10_site$removal_per_all, "AE10_site")

# Histogram for AV06_site
plot_histogram(AV06_site$removal_per_all, "AV06_site")

# Histogram for TO04_site
plot_histogram(TO04_site$removal_per_all, "TO04_site")

library ("e1071")
# Function to calculate skewness and kurtosis
calculate_skewness_kurtosis <- function(data) {
  skew <- skewness(data)
  kurt <- kurtosis(data)
  cat("Skewness:", skew, "\n")
  cat("Kurtosis:", kurt, "\n")
}

# Calculate skewness and kurtosis for each subset
calculate_skewness_kurtosis(AE10_site$removal_per_all)
calculate_skewness_kurtosis(AV06_site$removal_per_all)
calculate_skewness_kurtosis(TO04_site$removal_per_all)



# Identify outliers using boxplot
boxplot(AE10_site$removal_per_all, main = "Boxplot for AE10_site")

# Explore extreme values or outliers visually and quantitatively

#Model
# Example: Fitting a binomial GLM
model <- glm(cbind(removal_per_all, seeds_disposed - seeds_eaten) ~ Stand + Treatment+ Shannon_Index_5m,mean_dbh_y,Seed_sp, 
             family = binomial, data = data_merged_6)
head(data_merged_6)



# Calculate summary statistics by 'SpeciesList'
library(dplyr)

data_summary <- data_merged_6 %>%
  group_by(Seed_sp) %>%
  summarise(total_trials = sum(trials), total_successes = sum(successes))

# Display summary statistics
print(data_summary)
# Plot total trials by 'SpeciesList'
library(ggplot2)

ggplot(data_summary, aes(x = Seed_sp, y = total_trials)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Total Trials by Seed Species", x = "SpeciesList", y = "Total Trials") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Fit weighted binomial GLM using weights based on trial counts
weights <- 1 / data_summary$total_trials  # Calculate weights based on total trials

model_weighted <- glm(cbind(total_successes, total_trials - total_successes) ~ Seed_sp,
                      family = binomial, data = data_summary, weights = weights)

# Summary of the weighted model
summary(model_weighted)



#Model for each site
AE10_site<- subset(data_merged_6,Stand=="AE10")
AV06_site<- subset(data_merged_6, Stand="AV06")
TO04_site<- subset(data_merged_6,Stand="TO04")

#Because there are not the same amount of trials (or weight) for each species, it makes more sense to see models based on the species only
#Model for each species
ABAM<-subset(data_merged_6,Seed_sp=="ABAM")
ABLA<-subset(data_merged_6,Seed_sp=="ABLA")
CANO<-subset(data_merged_6,Seed_sp=="CANO")
PSME<-subset(data_merged_6,Seed_sp=="PSME")
THPL<-subset(data_merged_6,Seed_sp=="THPL")
TSHE<-subset(data_merged_6,Seed_sp=="TSHE")

head(ABAM)
# Extract 'successes' and 'trials' from the dataset
successes <- ABAM$Seeds.Remaining
trials <- ABAM$seeds_disposed

# Fit binomial GLM
model <- glm(cbind(successes, trials - successes) ~ Stand + Treatment + mean_dbh_y + Matches_Species + number_of_trees + Shannon_Index_5m,
             family = binomial, data = ABAM)

levels(ABAM$Stand)
levels(ABAM$Treatment)
levels(ABAM$Matches_Species)
levels(ABAM$Seed_sp)

# Example: Adding independent variables to the model
model <- glm(cbind(successes, trials - successes) ~ Stand + Treatment + mean_dbh_y + Matches_Species + number_of_trees + Shannon_Index_5m,
             family = binomial(link = "logit"), data = ABAM)

# Load the lme4 package for glmer function
library(lme4)

# Fit a binomial GLMM with dependent variable and predictors
model_glmer <- glmer(cbind(successes, trials - successes) ~ Stand + Treatment + mean_dbh_y + Matches_Species + number_of_trees + Shannon_Index_5m + (1 | Stand),
                     family = binomial(link = "logit"), data = ABAM)

# Calculate correlation matrix
cor_matrix <- cor(ABAM[, c("Stand", "Treatment", "mean_dbh_y", "Matches_Species", "number_of_trees", "Shannon_Index_5m")])

# Print correlation matrix
print(cor_matrix)

# Perform PCA on predictor variables
pca_result <- prcomp(ABAM[, c("Stand", "Treatment", "mean_dbh_y", "Matches_Species", "number_of_trees", "Shannon_Index_5m")])

# Plot PCA variance explained
plot(pca_result)


# View summary of the GLMM
summary(model_glmer)


# Summary of the model
summary(model)



# Function to calculate Cramér's V for two categorical variables
cramers_v <- function(x, y) {
  confusion_matrix <- table(x, y)
  chi_sq <- chisq.test(confusion_matrix)$statistic
  n <- sum(confusion_matrix)
  phi_sq <- chi_sq / n
  min_dim <- min(dim(confusion_matrix)) - 1
  cramers_v <- sqrt(phi_sq / min_dim)
  return(cramers_v)
}

# Example: Calculate Cramér's V between two categorical variables
cramers_v(ABAM$Stand, ABAM$Treatment)

# Chi-square test of independence between two categorical variables
chisq_test <- chisq.test(ABAM$Stand, ABAM$Treatment)
print(chisq_test)
chisq_test <- chisq.test(ABAM$Stand, ABAM$mean_dbh_y)
print(chisq_test)
chisq_test <- chisq.test(ABAM$Stand, ABAM$Shannon_Index_5m)
print(chisq_test)
chisq_test <- chisq.test(ABAM$Stand, ABAM$number_of_trees)
print(chisq_test)
chisq_test <- chisq.test(ABAM$Stand, ABAM$Matches_Species)
print(chisq_test)
chisq_test <- chisq.test(ABAM$Treatment, ABAM$Matches_Species)
print(chisq_test)


# Example: Create a mosaic plot
mosaicplot(table(ABAM$Stand, ABAM$Treatment), main = "Mosaic Plot of Stand vs. Treatment")
mosaicplot(table(ABAM$Stand, ABAM$mean_dbh_y), main = "Mosaic Plot of Stand vs. Treatment")


#Visuallizing the structure of my data
# Display the structure of your dataset
str(ABAM)
# Summary of numeric variables
summary(ABAM[, c("mean_dbh_y", "number_of_trees", "Shannon_Index_5m", "successes", "trials")])
# Frequencies of categorical variables
table(ABAM$Stand)
table(ABAM$Treatment)
table(ABAM$Matches_Species)
# Check for missing values
colSums(is.na(ABAM))

# Proportion of missing values
prop.table(colSums(is.na(ABAM)))
# Check data types of successes and trials
str(ABAM$successes)
str(ABAM$trials)
# Calculate proportion of successes
ABAM$proportion_success <- ABAM$successes / ABAM$trials

# Select numeric predictors for PCA
predictors <- ABAM[, c("mean_dbh_y", "number_of_trees", "Shannon_Index_5m")]
# Standardize predictors
predictors_std <- scale(predictors)
# Perform PCA
pca_result <- prcomp(predictors_std, scale. = TRUE)
# Summary of PCA
summary(pca_result)
# Plot PCA results
biplot(pca_result, scale = 0)


# Define orthogonal contrasts for Stand variable
contrasts(ABAM$Stand) <- contr.poly(3)  # 3 levels: AE10, AV06, TO04
# Select numeric predictors for PCA
predictors <- ABAM[, c("mean_dbh_y", "number_of_trees", "Shannon_Index_5m")]
# Standardize predictors
predictors_std <- scale(predictors)
# Perform PCA
pca_result <- prcomp(predictors_std, scale. = TRUE)
# Extract principal components
pcs <- pca_result$x  # This will be used as new predictors in the model


library(lme4)

# Define the formula for the GLMM
formula <- cbind(successes, trials - successes) ~ Stand + Treatment + pcs[, 1:2] + (1 | Stand)

# Fit the GLMM
model_glmer <- glmer(formula, family = binomial, data = ABAM)

# Check model summary
summary(model_glmer)

formula_2 <- cbind(successes, trials - successes) ~ Stand + Treatment  + (1 | Stand)
model_glmer_2 <- glmer(formula_2, family = binomial, data = ABAM)
summary(model_glmer_2)

hist(resid(model_glmer))
qqnorm(resid(model_glmer))
qqline(resid(model_glmer))
shapiro.test(resid(model_glmer))


# Load required libraries
library(lme4)

# Define response variable
response_var <- "cbind(successes, trials - successes)"

# Define predictor variables
predictors <- c("Stand", "Treatment", "mean_dbh_y", "Matches_Species", 
                "number_of_trees", "Shannon_Index_5m")

# Create all possible combinations of predictors (main effects)
all_predictors <- predictors

# Create all possible combinations of predictor interactions (up to 2-way)
interaction_terms <- combn(predictors, 2, FUN = function(x) paste(x, collapse = ":"))

# Combine main effects with interactions
model_formulas <- c(all_predictors, interaction_terms)

# Fit and compare GLMMs for each model formula
model_results <- lapply(model_formulas, function(formula) {
  # Construct formula for glmer with random intercept for Stand
  full_formula <- paste(response_var, "~", formula, "+ (1 | Stand)")
  
  # Fit GLMM model
  model <- try(glmer(as.formula(full_formula), family = binomial, data = ABAM))
  
  # Return model object (or error message if model fitting fails)
  return(list(formula = formula, model = model))
})

# Extract model summaries for successful fits
successful_models <- lapply(model_results, function(result) {
  if (inherits(result$model, "try-error")) {
    return(NULL)  # Return NULL for failed models
  } else {
    return(summary(result$model))  # Return model summary
  }
})

# View successful model summaries
for (i in seq_along(successful_models)) {
  cat("Model Formula:", model_results[[i]]$formula, "\n")
  if (!is.null(successful_models[[i]])) {
    print(successful_models[[i]])
  } else {
    cat("Model fitting failed.\n")
  }
  cat("\n")
}


# Load required libraries
library(lme4)

# Define response variable
response_var <- "cbind(successes, trials - successes)"

# Define predictor variables
predictors <- c("Stand", "Treatment", "mean_dbh_y", "Matches_Species", 
                "number_of_trees", "Shannon_Index_5m")

# Create all possible combinations of predictors (main effects)
all_predictors <- predictors

# Create all possible combinations of predictor interactions (up to 2-way)
interaction_terms <- combn(predictors, 2, FUN = function(x) paste(x, collapse = ":"))

# Combine main effects with interactions
model_formulas <- c(all_predictors, interaction_terms)

# Initialize variables to track best model
best_model <- NULL
best_aic <- Inf

# Fit and compare GLMMs for each model formula
for (formula in model_formulas) {
  # Construct formula for glmer with random intercept for Stand
  full_formula <- paste(response_var, "~", formula, "+ (1 | Stand)")
  
  # Fit GLMM model
  model <- try(glmer(as.formula(full_formula), family = binomial, data = ABAM))
  
  # Check if model fitting was successful
  if (!inherits(model, "try-error")) {
    # Calculate AIC
    model_aic <- AIC(model)
    
    # Update best model if current model has lower AIC
    if (model_aic < best_aic) {
      best_model <- model
      best_aic <- model_aic
    }
  }
}

# Print the best model and its summary
if (!is.null(best_model)) {
  cat("Best Model (Lowest AIC):\n")
  print(summary(best_model))
} else {
  cat("No valid model found.\n")
}

#Per site
# Load required libraries
library(lme4)
successes <- data_merged_6$Seeds.Remaining
trials <- data_merged_6$seeds_disposed
# Define response variable
response_var <- "cbind(successes, trials - successes)"

# Define predictor variables
predictors <- c("Treatment", "mean_dbh_y", "Matches_Species", "number_of_trees", "Shannon_Index_5m")

# Initialize list to store results for each site
site_model_results <- list()

# Loop over each site (Stand)
for (site in unique(data_merged_6$Stand)) {
  cat("Site:", site, "\n")
  
  # Subset data for the current site
  site_data <- data_merged_6[data_merged_6$Stand == site, ]
  
  # Create all possible combinations of predictors (main effects)
  all_predictors <- predictors
  
  # Create all possible combinations of predictor interactions (up to 2-way)
  interaction_terms <- combn(predictors, 2, FUN = function(x) paste(x, collapse = ":"))
  
  # Combine main effects with interactions
  model_formulas <- c(all_predictors, interaction_terms)
  
  # Initialize variables to track best model for current site
  best_model <- NULL
  best_aic <- Inf
  
  # Fit and compare GLMMs for each model formula
  for (formula in model_formulas) {
    # Construct formula for glmer with random intercept for Stand
    full_formula <- paste(response_var, "~", formula, "+ (1 | Stand)")
    
    # Fit GLMM model
    model <- try(glmer(as.formula(full_formula), family = binomial, data = site_data))
    
    # Check if model fitting was successful
    if (!inherits(model, "try-error")) {
      # Calculate AIC
      model_aic <- AIC(model)
      
      # Update best model if current model has lower AIC
      if (model_aic < best_aic) {
        best_model <- model
        best_aic <- model_aic
      }
    }
  }
  
  # Store results for the current site
  site_model_results[[site]] <- list(best_model = best_model, best_aic = best_aic)
}

# Print best models and AICs for each site
for (site in names(site_model_results)) {
  cat("\nSite:", site, "\n")
  if (!is.null(site_model_results[[site]]$best_model)) {
    cat("Best Model (Lowest AIC):\n")
    print(summary(site_model_results[[site]]$best_model))
  } else {
    cat("No valid model found for this site.\n")
  }
}



