####################################################
## STATISTICS EXTRACTION FOR JOURNAL REPORTING
## For: Seed Predation Camera Trap Experiment 2017
## Goal: Extract effect sizes, CIs, z-stats, and p-values
## Journal requirement: report test statistics + df + p-values
## Note: glmer/glmmTMB/glm.nb use z-statistics (not t or F),
##       so no df needed — just z and p alongside OR/ratio + CI
####################################################


# ── 1. CAMERA TRAP MODEL (glm.nb) ────────────────────────────────────────────
# model_nb: predator_detections ~ Stand + Species_ID + offset(log(Weeks_sampled))

cat("\n====== MODEL 1: Camera trap — glm.nb ======\n")
model_nb_summary <- summary(model_nb)
print(model_nb_summary$coefficients)
confint(model_nb)

# Pairwise stand comparisons (already in your script)
cat("\n--- Pairwise stand comparisons (emmeans, response scale = ratios) ---\n")
em_camera_stand <- emmeans(model_nb, ~ Stand, type = "response", at = list(Weeks_sampled = 1))
pairwise_stand <- pairs(em_camera_stand)
pairwise_stand_df <- as.data.frame(
  summary(pairwise_stand, infer = c(TRUE, TRUE), level = 0.95)
)
print(pairwise_stand_df)
print(summary(pairwise_stand))
# These give you: ratio, SE, z.ratio, p.value  ← use all four in text

# Pairwise species comparisons
cat("\n--- Pairwise species comparisons ---\n")
em_camera_species <- emmeans(model_nb, ~ Species_ID, type = "response", at = list(Weeks_sampled = 1))
pairwise_species <- pairs(em_camera_species)
pairwise_species_df <- as.data.frame(
  summary(pairwise_species, infer = c(TRUE, TRUE), level = 0.95)
)
print(summary(pairwise_species))
print(pairwise_species_df)
# For P. maniculatus vs Tamias: extract ratio, SE, z, p

# Extract P. maniculatus vs all others specifically
# Filter contrasts involving P. maniculatus
pm_contrasts <- pairwise_species_df[grepl("P. maniculatus", pairwise_species_df$contrast), ]
print(pm_contrasts)
# Columns: estimate (= β on log scale), SE, z.ratio, p.value


# ── 2. SEED REMOVAL MODEL (glmer OLRE) ───────────────────────────────────────
# glmer_modelOLRE_New: cbind(Success, Seeds.Placed-Success) ~ Stand + Seed_sp + (1|Camera) + (1|ObsID) + (1|Week)

cat("\n====== MODEL 2: Seed removal — glmer OLRE ======\n")
print(summary(glmer_modelOLRE_New))
confint(glmer_modelOLRE_New, method = "Wald")

# Stand pairwise (response scale = odds ratios with CI)
cat("\n--- Stand pairwise comparisons (odds ratios + 95% CI) ---\n")
em_stand <- emmeans(glmer_modelOLRE_New, ~ Stand, type = "response")
stand_contrasts <- pairs(em_stand, adjust = "tukey")
stand_contrasts_df <- as.data.frame(summary(stand_contrasts))
stand_contrasts_df <- as.data.frame(
  summary(stand_contrasts, infer = c(TRUE, TRUE), level = 0.95)
)
print(stand_contrasts_df)
confint(stand_contrasts)
# Columns: odds.ratio, SE, z.ratio, p.value  ← use all four

# Seed species pairwise
cat("\n--- Seed species pairwise comparisons ---\n")
em_seeds <- emmeans(glmer_modelOLRE_New, ~ Seed_sp, type = "response")
seed_contrasts <- pairs(em_seeds, adjust = "tukey")
seed_contrasts_df <- as.data.frame(summary(seed_contrasts))
seed_contrasts_df <- as.data.frame(
  summary(seed_contrasts, infer = c(TRUE, TRUE), level = 0.95)
)
print(seed_contrasts_df)


# Quick formatted output for the three key stand comparisons in your text
cat("\n--- KEY STAND COMPARISONS (formatted for manuscript) ---\n")
for (i in 1:nrow(stand_contrasts_df)) {
  cat(sprintf("  %s: OR = %.2f, z = %.3f, p = %.4f\n",
              stand_contrasts_df$contrast[i],
              stand_contrasts_df$odds.ratio[i],
              stand_contrasts_df$z.ratio[i],
              stand_contrasts_df$p.value[i]))
}


# ── 3. ELEVATION GROUP MODEL (glmmTMB betabinomial) ──────────────────────────
# model_bb: cbind(Success, Seeds.Placed-Success) ~ Elevation_group * Stand + ...

cat("\n====== MODEL 3: Elevation group — glmmTMB betabinomial ======\n")
print(summary(model_bb))

# Pairwise by Stand (high vs low elevation seed group within each stand)
cat("\n--- Pairwise by Stand (elevation group effect within each stand) ---\n")
em2 <- emmeans(model_bb, ~ Stand * Elevation_group, type = "response")
pairs_by_stand <- pairs(em2, by = "Stand")
pairs_by_stand_df <- as.data.frame(summary(pairs_by_stand))
pairs_by_stand_df <- as.data.frame(
  summary(pairs_by_stand, infer = c(TRUE, TRUE), level = 0.95)
)
print(pairs_by_stand_df)


# Pairwise by Elevation_group (elevational trend within each seed group)
cat("\n--- Pairwise by Elevation_group (stand differences within high/low seed group) ---\n")
pairs_by_elevgroup <- pairs(em2, by = "Elevation_group", adjust = "tukey")
pairs_by_elevgroup_df <- as.data.frame(summary(pairs_by_elevgroup))
pairs_by_elevgroup_df <- as.data.frame(
  summary(pairs_by_elevgroup, infer = c(TRUE, TRUE), level = 0.95)
)
print(pairs_by_elevgroup_df)

pairs_by_elevgroup_ci <- as.data.frame(confint(
  pairs(emmeans(model_bb, ~ Stand * Elevation_group), by = "Elevation_group", adjust = "tukey")
))
cat("\n--- With 95% CI ---\n")
print(pairs_by_elevgroup_ci)


# ── 4. TREATMENT MODELS (glmer, low and high seed species) ───────────────────

cat("\n====== MODEL 4a: Treatment — low elevation seed species ======\n")
print(summary(glmer_modelOLRE_New3))
em_treatment_low <- emmeans(glmer_modelOLRE_New3, ~ Treatment | Seed_sp, type = "response")
treatment_contrasts_low <- pairs(em_treatment_low, adjust = "tukey")
treatment_contrasts_low_df <- as.data.frame(summary(treatment_contrasts_low))
treatment_contrasts_low_df <- as.data.frame(
  summary(treatment_contrasts_low, infer = c(TRUE, TRUE), level = 0.95)
)
print(treatment_contrasts_low_df)

cat("\n====== MODEL 4b: Treatment — high elevation seed species ======\n")
print(summary(glmer_modelOLRE_New4))
em_treatment_high <- emmeans(glmer_modelOLRE_New4, ~ Treatment | Seed_sp, type = "response")
treatment_contrasts_high <- pairs(em_treatment_high, adjust = "tukey")
treatment_contrasts_high_df <- as.data.frame(summary(treatment_contrasts_high))
treatment_contrasts_high_df <- as.data.frame(
  summary(treatment_contrasts_high, infer = c(TRUE, TRUE), level = 0.95)
)
print(treatment_contrasts_high_df)




