#Plots

setwd("C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2/Datasets")
#Loading the data to be treated

load("data_cleaned_2.RData")

library(dplyr)
library(tidyr)
library(ggplot2)


head(data_cleaned_2)

boxplot(removal_per_all ~ Stand, data = data_cleaned_2,
        main = "Seed Removal per Stand",
        xlab = "Stand",
        ylab = "Seed Removal (Proportion)",
        col = "lightblue",
        border = "darkblue")

aggregate(removal_per_all ~ Stand, data = data_cleaned_2, summary)

boxplot(removal_per_all ~ Seed_sp, data = data_cleaned_2,
        main = "Seed Removal per Stand",
        xlab = "Stand",
        ylab = "Seed Removal (Proportion)",
        col = "lightblue",
        border = "darkblue")

aggregate(removal_per_all ~ Seed_sp, data = data_cleaned_2, summary)


boxplot(removal_per_all ~ Treatment, data = data_cleaned_2,
        main = "Seed Removal per Stand",
        xlab = "Stand",
        ylab = "Seed Removal (Proportion)",
        col = "lightblue",
        border = "darkblue")

aggregate(removal_per_all ~ Treatment, data = data_cleaned_2, summary)

boxplot(removal_per_all ~ Seed_sp + Stand, data = data_cleaned_2,
        main = "Seed Removal per Stand",
        xlab = "Stand",
        ylab = "Seed Removal (Proportion)",
        col = "lightblue",
        border = "darkblue")

aggregate(removal_per_all ~ Seed_sp + Treatment, data = data_cleaned_2, summary)
aggregate(removal_per_all ~ Seed_sp + Stand, data = data_cleaned_2, summary)


ggplot(data_cleaned_2, aes(x = Stand, y = removal_per_all, fill = Stand)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) + # Boxplot without outliers for clarity
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") + # Add points for individual observations
  facet_wrap(~ Seed_sp, scales = "free_y") + # Separate panels for each species
  labs(
    title = "Seed Removal per Stand for Each Species",
    x = "Stand",
    y = "Removal per All (Weighted)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set3") # Nice color palette




ggplot(data_cleaned_2, aes(x = Treatment, y = removal_per_all, fill = Treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) + # Boxplot without outliers for clarity
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") + # Add points for individual observations
  facet_wrap(~ Seed_sp, scales = "free_y") + # Separate panels for each species
  labs(
    title = "Seed Removal per Stand for Each Species",
    x = "Stand",
    y = "Removal per All (Weighted)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set3") # Nice color palette



ggplot(data_cleaned_2, aes(x = Stand, y = removal_per_all, fill = Stand)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) + # Boxplot without outliers
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") + # Add individual observations
  labs(
    title = "Removal per All by Site",
    x = "Site (Stand)",
    y = "Removal per All"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set3")


#Checking if those differences are statistically different:
# Shapiro-Wilk test for normality by stand
by(data_cleaned_2$removal_per_all, data_cleaned_2$Stand, shapiro.test)
#Not notmally distributed so using non-parametrical test
# Kruskal-Wallis test
kruskal.test(removal_per_all ~ Stand, data = data_cleaned_2)
#Since the p-value is very small (less than 0.05), we can conclude that there are statistically significant differences in seed removal between at least two of the stands.
pairwise.wilcox.test(
  data_cleaned_2$removal_per_all,
  data_cleaned_2$Stand,
  p.adjust.method = "BH" # Adjust for multiple comparisons
)
#Result: There is a statistical difference between AAE10 and the other stand but not between AV06 and ToO04. 




ggplot(data_cleaned_2, aes(x = Treatment, y = removal_per_all, fill = Treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) + # Boxplot without outliers
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") + # Add individual observations
  labs(
    title = "Removal per All by Site",
    x = "Treatment",
    y = "Removal per All"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set3")
#Checking if those differences are statistically different:
# Shapiro-Wilk test for normality by stand
by(data_cleaned_2$removal_per_all, data_cleaned_2$Treatment, shapiro.test)
#Not notmally distributed so using non-parametrical test
# Kruskal-Wallis test
kruskal.test(removal_per_all ~ Treatment, data = data_cleaned_2)
pairwise.wilcox.test(
  data_cleaned_2$removal_per_all,
  data_cleaned_2$Treatment,
  p.adjust.method = "BH" # Adjust for multiple comparisons
)
#There is no statistical difference between the treatments. 