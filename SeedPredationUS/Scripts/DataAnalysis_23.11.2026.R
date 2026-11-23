####Code for Manuscript : Seed predation across an elevational gradient
####Last modified : 23.11.2026
####ETHZ Eléonore Perret
####This code is seperated in several parts. 
####1. Libraries 
####2. Loading the datasets. I have created a dataset that is already clean for the seed data
####3. Functions: For this script I just defined a function to get standardized emmeans tables. 
####4. Camera data cleaning : End product = Predator_data_clean.Rdata . Can be used directly in the loaded datasets. 
####5. Seed data cleaning : End product = data_cleaned_2.Rdata . Can be used directly in the loaded datasets. 
#From there on, this is the analysis with the plots found in the manuscript and the different models. 
####6. Research question 1: Seed predator density analysis (30 min. intervals). End product = Fig. X and PERMANOVA and model for seed predator density
####7. Research question 2: Seed preferences (glmTMB model + Figure X)
####8. Research question 3a: Elevation group (glmTMB model)
####9. Research question 3b: Treatment (glmTMB model + Figure X)
###10. Other figures : Elevation plot (Figure X). 


# 1. Loading librairies ------------------------------------------------------

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)
library(DHARMa)
library(emmeans)
library(glmmTMB)
library(MASS)
library(extrafont)
library(ggtext)
library(car)
library(vegan)
library(lubridate)
library(multcompView)
library(patchwork)

#For references 
#Package version and citation style
# packageVersion("glmmTMB"); citation("glmmTMB")  
# packageVersion("DHARMa"); citation("DHARMa")
# packageVersion("emmeans"); citation("emmeans")
# packageVersion("car"); citation("car")
# packageVersion("vegan"); citation("vegan")
# rstudioapi::versionInfo()$version     
# sessionInfo()  
# R.version.string 
                 

# 2. Loading the data --------------------------------------------------------
# Set the working directory
getwd()
setwd("C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_US_Manuscript/") #setting here your own directory. 

#If you want to run everything from the beggining : 
# Load datasets then follow part 3 and 4 for cleaning the dataset. Otherwise you can already run the R.data. 
# seed_predation <- read.csv("SeedPredationUS/Datasets/SeedPredation_First_week.csv", sep=";") #Seed trays information
# all_data_seed<- read.csv("SeedPredationUS/Datasets/seed_data.csv",sep= ";") #Removal rates informations
# predators_data <- read_excel("SeedPredationUS/Datasets/camera_data.xlsx") #Camera data results

#If only interested in the analysis itself, you can run those pre-cleaned datasets. 
load("SeedPredationUS/Datasets/data_cleaned_2.RData")
load("SeedPredationUS/Datasets/predator_data_clean.RData")


# 3. Function for emmean table ----------------------------------------------------------------

emmeans_table_format<- function(x, ...){
  df<-as.data.frame(summary (x, infer= TRUE,level=0.95, ...))
  
  if ("p.value"%in% names(df))
    df$p.value <-ifelse (df$p.value <0.001, "<0.001", sprintf ("%.3f", df$p.value))
  
  num<-sapply (df, is.numeric)
  df[num]<- lapply (df[num], sprintf, fmt= "%.3f")
  
  lo<-grep("LCL|lower.CL", names(df), value= TRUE)
  hi<-grep("UCL|upper.CL", names(df), value= TRUE)
  if(length(lo)== 1 && length(hi)==1){
    df$`95% CI`<- paste0("[", df[[lo]],",", df[[hi]], "]")
    keep<-setdiff(names(df), c(lo, hi, "95% CI", "df"))
    after<-sum(match(keep, names(df))< match (lo, names(df)))
    df<- df[append(keep, "95% CI", after= after)]
    }
  df
}

# 4. CAMERA TRAP DATA : Cleaning (END PRODUCT : Predator_data_clean.RData) ------------------------------------------------------------
seed_predation <- seed_predation %>% slice(1:27) #Because I have empty rows after the row 27 (this comes from excel), I will first delete all the rows below

#Changing names for clarity and plots 
predators_data$Stand <- factor(  
  predators_data$Stand, # For Stand
  levels = c("TO04", "AV06", "AE10"), 
  labels = c("Low", "Mid", "High")
)
predators_data$Species_ID <- factor(
  predators_data$Species_ID,# For Species_ID
  levels = c("Peromyscus maniculatus", "Tamias sp", "Lepus americanus", "Bird", "Flying squirrel", "Shrew?", "vole?", "Zapus?", "???"),
  labels = c("Peromyscus maniculatus", "Tamias spp.", "Lepus americanus", "Birds", "Glaucomys oregonensis", "Shrew spp.", "Vole spp.", "Zapus spp.", "Not identified")
)

# Filter to rows where an animal was present and the Species_ID is not missing or "NA" 
predator_data_clean <- predators_data %>%
  filter(Animal_presence == "Yes", !is.na(Species_ID), Species_ID != "NA")

save(predator_data_clean, file = "SeedPredationUS/Datasets/predator_data_clean.RData")





# 5. SEED DATA: Cleaning (END PRODUCT : data_cleaned_2.RData) ---------------------------------------------------

# Correct misspelled species names
all_data_seed$Seed_sp[all_data_seed$Seed_sp == "THPLE"] <- "THPL"
# Replace 'to count' with NA in the Seeds.Remaining column
all_data_seed$Seeds.Remaining[all_data_seed$Seeds.Remaining == 'to count'] <- NA

#seeds at the beginning
seed_disposed <- c(ABAM = 5, ABLA = 20, CANO = 30, PSME = 25, THPL = 90, TSHE = 90)
all_data_seed$seed_disposed<-seed_disposed[all_data_seed$Seed_sp]

#adding weight
species_weights <- c(ABAM = 0.0264, ABLA = 0.0149, CANO = 0.0053, PSME = 0.0063, THPL = 0.0015, TSHE = 0.0010)
all_data_seed$seed_weight <- species_weights[all_data_seed$Seed_sp]

# Remove rows with NA values that were added in the original dataset. This will not affect the results but just prevent errors for the GLMTMB models.
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
#by mass : 
data_cleaned_2$mass_placed<-data_cleaned_2$seed_disposed*data_cleaned_2$seed_weight
data_cleaned_2$mass_remaining<-data_cleaned_2$Seeds.Remaining*data_cleaned_2$seed_weight
data_cleaned_2$mass_removed <- data_cleaned_2$mass_placed-data_cleaned_2$mass_remaining
#by seeds: 
data_cleaned_2$seed_removed <- data_cleaned_2$seed_disposed-data_cleaned_2$Seeds.Remaining

# New variable "Success" (seeds eaten)
data_cleaned_2$Success <- data_cleaned_2$seed_removed
data_cleaned_2$proportion_removed_inmass<-data_cleaned_2$mass_removed/data_cleaned_2$mass_placed

# Now 'Success' is the number of successes (seeds eaten), and 'Seeds.Placed' is the number of trials.-- BINOMIAL RESPONSE
#save(data_cleaned_2, file = "SeedPredationUS/Datasets/data_cleaned_2.RData")

# ANALYSIS ----------------------------------------------------------------
# 6. Research question 1: Seed predator density analysis (30 min. intervals) -----------------------------------------------------

# every 30 min detections
pdat<-predator_data_clean%>%
  mutate(tod_sec= as.numeric(difftime(Time, floor_date(Time, "day"), units= "secs")),datetime= Date + seconds(tod_sec),cam_id = paste(Stand, Camera, sep= "_"))

sum(is.na(pdat$datetime))

events30<-pdat%>%
  arrange(cam_id, Species_ID, datetime)%>%
  group_by(cam_id, Species_ID)%>%
  mutate(gap= as.numeric(difftime(datetime, lag(datetime), units= "mins"))) %>%
  filter(is.na(gap) | gap >30 )%>%
  ungroup()


summary_tables_predator_overall<- function(d) {
  d %>%
    count(Stand, Week, Species_ID, name= "counts") %>%
    mutate(counts = replace_na(counts, 0)) %>%
    group_by(Stand, Species_ID) %>%
    summarise(total = sum(counts), weeks_detected = sum(counts > 0),mean_per_week = mean(counts), se_per_week = sd(counts) / sqrt(n()), .groups = "drop") %>%
    group_by(Stand) %>%
    mutate(pct_of_stand = 100 * total / sum(total)) %>%
    ungroup() %>%
    mutate(Stand = factor(Stand, levels = c("Low", "Mid", "High"))) %>%
    arrange(Stand, desc(total))
}

table_raw <- summary_tables_predator_overall(pdat)

table_events <- summary_tables_predator_overall(events30)

# side-by-side comparison
table_both_normal_30 <- full_join(table_raw, table_events, by = c("Stand", "Species_ID"),suffix = c("_images", "_events"))
#write.csv(table_both_normal_30, "SeedPredationUS/Datasets/table_both_normal_30.csv")

#only the events with 30 min intervals
weekly30 <-events30 %>% 
  count(Stand, Week, Species_ID, name= "n_det") %>%
  mutate(n_det= replace_na(n_det,0),Stand= factor(Stand, levels= c("Low", "Mid", "High")), stand_week= interaction(Stand, Week))

#How common is each taxon within each stand : (percentage table end product)
#Per stand
weekly30 %>% 
  group_by(Stand, Species_ID) %>% 
  summarise(n = sum(n_det), .groups = "drop") %>%
  group_by(Stand) %>% 
  mutate(pct = 100 * n / sum(n)) %>% 
  arrange(Stand, desc(n))

## Doughnut plot
#Common names :
common_names<-c(
  "Peromyscus maniculatus" = "*P. maniculatus*",  
  "Tamias spp." = "*Tamias* spp.",               
  "Lepus americanus" = "*L. americanus*",         
  "Birds" = "Birds",                    
  "Glaucomys oregonensis" = "*G. oregonensis*",  
  "Shrew spp." = "*Sorex* spp.",                
  "Vole spp." = "*Microtus* spp.",                 
  "Zapus spp." = "*Zapus* spp.",                
  "Not identified" = "Not identified"  
)

species_colors <- c(
  "Peromyscus maniculatus" = "#4477AA",  
  "Tamias spp." = "#66CCEE",               
  "Lepus americanus" = "#228833",         
  "Birds" = "#CCBB44",                    
  "Glaucomys oregonensis" = "#EE6677",  
  "Shrew spp." = "#AA3377",                
  "Vole spp." = "#BBBBBB",                 
  "Zapus spp." = "#44AA99",                
  "Not identified" = "#DDDDDD"           
)

threshold<-0.10

d<-weekly30 %>%
  mutate(Species_ID= factor(Species_ID, levels= names(species_colors)), Stand= factor(Stand, levels= c("Low", "Mid", "High")))%>%
  group_by(Stand, Species_ID)%>%
  summarise(total=sum(n_det), .groups= "drop")%>%
  group_by(Stand)%>%
  mutate(perc= total/sum(total), label= ifelse(perc >=threshold, paste0(formatC(perc*100, format= "f", digits= 1), "%"), NA))%>%
  ungroup()

rare<- d%>%
  group_by(Stand)%>%
  summarise(rare_perc= sum(perc[perc<threshold]), .groups= "drop")%>%
  mutate (y= rare_perc/2, label= paste0(formatC(rare_perc*100, format="f", digits = 1), "%"))

dounghnut_plot<-ggplot(d, aes(x=2, y= perc, fill= Species_ID)) +
  geom_col(width= 1, color= NA)+
  geom_text(aes(label=label), position = position_stack(vjust=0.5), size= 4.8, color= "white", na.rm= TRUE)+
  geom_text(data= rare, aes(x=1.15, y=y, label= label), inherit.aes = FALSE, size= 4.8)+
  coord_polar(theta = "y", start= 0)+
  xlim(0.5, 2.5)+
  scale_fill_manual(values= species_colors, labels= common_names, drop= FALSE)+
  facet_wrap(~Stand, labeller= as_labeller(c(Low = "Low elevation",Mid = "Mid elevation",High = "High elevation" )))+
  labs(fill= NULL)+
  theme_void()+
  theme(legend.text = element_markdown())+
  theme(legend.position = "right", strip.text = element_text(size= 12), text= element_text(family = "serif"), margin = margin(t=8, b=14), plot.margin=margin(10,10,10,10))
dounghnut_plot

#ggsave("seed_predator_plot.png", plot= dounghnut_plot,path= "SeedPredationUS/Plots/" )


#Does the community differes among stands? (permanova)

# PERMANOVA + dispersion 
wide30 <- weekly30 %>%
  dplyr::select(Stand, Week, Species_ID, n_det) %>%
  pivot_wider(names_from = Species_ID, values_from = n_det, values_fill = 0)

comm30 <- wide30 %>% dplyr::select(-Stand, -Week)

d30 <- vegdist(decostand(comm30, "total"), "bray")

set.seed(123)
adonis2(d30 ~ Stand, data = wide30, permutations = 9999)
permutest(betadisper(d30, wide30$Stand), permutations = 9999)


#Does activity differ among stands? 
wk <- weekly30 %>% 
  group_by(Stand, Week) %>% 
  summarise(tot = sum(n_det), .groups = "drop")

wk$Stand<-relevel(factor(wk$Stand), ref= "High")

wk_sp <- weekly30 %>% 
  group_by(Species_ID) %>% 
  summarise(tot = sum(n_det), .groups = "drop")%>%
  mutate(prop = tot / sum(tot), pct  = 100 * prop)

wk_sp_st <- weekly30 %>%
  group_by(Species_ID, Stand) %>%
  summarise(tot = sum(n_det), .groups = "drop") %>%
  group_by(Stand) %>%                       # now sum(tot) is the total of each stand
  mutate(prop = tot / sum(tot), pct  = 100 * prop) %>%
  ungroup()


#MODEL 1: 
m_tot <- glmmTMB(tot ~ Stand, family = nbinom2, data = wk)

model_predators_resid2 <- simulateResiduals(m_tot)
plot(model_predators_resid2)
testOverdispersion(model_predators_resid2)
testZeroInflation(model_predators_resid2)
plotResiduals(m_tot, form = ~.)

summary(m_tot)

model_1_table<-as.data.frame(summary(m_tot)$coefficients$cond)
lo<-model_1_table$Estimate - qnorm(0.975)*model_1_table$`Std. Error`
hi<-model_1_table$Estimate + qnorm(0.975)*model_1_table$`Std. Error`
model_1_table <-data.frame(
  Term= rownames (model_1_table), 
  Estimate= sprintf("%.3f", model_1_table[,"Estimate"]),
  SE= sprintf("%.3f", model_1_table[,"Std. Error"]),
  `95% CI`= sprintf("[%.3f, %.3f]",lo,hi),
  z= sprintf("%.3f", model_1_table[,"z value"]),
  p= ifelse(model_1_table[,"Pr(>|z|)"] <0.001, "<0.001",sprintf("%.3f", model_1_table[,"Pr(>|z|)"])),row.names = NULL)
model_1_table

Anova(m_tot, type= "II")

em <- emmeans(m_tot, ~ Stand, type = "response"); em
pairs(em, adjust = "tukey")
emmeans_table_format(pairs(em, adjust = "tukey"))


# 7. Research question 2: Seed preferences (glmTMB model + Figure X)------------------------------------------------------------------

#First let's look at our data: 

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

#Stand removal
stand_removal <- data_cleaned_2 %>%
  group_by(Stand)%>%
  mutate(removal_prop = Success / seed_disposed) %>% 
  summarize(mean_removal= mean(removal_prop, na.rm = TRUE))%>%
  arrange(mean_removal)
stand_removal

#Species AND Stand removal
species_stand_removal<- data_cleaned_2 %>%
  group_by(Stand, Seed_sp)%>%
  mutate(removal_prop = Success / seed_disposed) %>% 
  summarize(mean_removal= mean(removal_prop, na.rm = TRUE))%>%
  arrange(Seed_sp)
species_stand_removal

#Treatment (same context or mixed)
treatment_removal<- data_cleaned_2 %>%
  group_by(Treatment)%>%
  mutate(removal_prop = Success / seed_disposed) %>% 
  summarize(mean_removal= mean(removal_prop, na.rm = TRUE))
treatment_removal


#How limiting is seed predation (as measured by seed removal), and how does this vary among dominant conifer species and across elevations?

#I know that this is where I have the highest removal rates so I'm interested in the difference between this level and the others. 
data_cleaned_2$Stand<-relevel(factor(data_cleaned_2$Stand), ref= "High")

#Then for my model, I defined the most logical model based on my knowledge
model_removal_species_full<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand + Seed_sp +(1 | Camera)  + (1|Week),family = betabinomial(),   data= data_cleaned_2)

#checking now if it would make sense to have a simpler model based on AIC score
model_removal_species_null<-glmmTMB(cbind(Success, seed_disposed - Success)~1, family = betabinomial(),   data= data_cleaned_2)
model_removal_species_simple<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand + Seed_sp ,family = betabinomial(),data= data_cleaned_2)
model_removal_species_simple2<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand + Seed_sp+ (1|Camera),family = betabinomial(),data= data_cleaned_2)
model_removal_species_simple3<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand + Seed_sp+ (1|Week),family = betabinomial(),data= data_cleaned_2)

AIC(model_removal_species_null, model_removal_species_simple, model_removal_species_simple2, model_removal_species_simple3, model_removal_species_full) %>%
  arrange(AIC) #the full model has the best AIC score

Anova(model_removal_species_full, type= "II")

#the best model still remains the full one : 
model_removal_species<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand + Seed_sp + Seed_sp + (1 | Camera)  + (1|Week),family = betabinomial(),  data= data_cleaned_2)

#Simulation the residuals to check for distribution of residuals
SimOUtput<-simulateResiduals(model_removal_species)
plot(SimOUtput)
testOverdispersion(SimOUtput)
testZeroInflation(SimOUtput) 

plotResiduals(model_removal_species, form = ~.)

summary(model_removal_species)
model_2_table<-as.data.frame(summary(model_removal_species)$coefficients$cond)
lo<-model_2_table$Estimate - qnorm(0.975)*model_2_table$`Std. Error`
hi<-model_2_table$Estimate + qnorm(0.975)*model_2_table$`Std. Error`
model_2_table <-data.frame(
  Term= rownames (model_2_table), 
  Estimate= sprintf("%.3f", model_2_table[,"Estimate"]),
  SE= sprintf("%.3f", model_2_table[,"Std. Error"]),
  `95% CI`= sprintf("[%.3f, %.3f]",lo,hi),
  z= sprintf("%.3f", model_2_table[,"z value"]),
  p= ifelse(model_2_table[,"Pr(>|z|)"] <0.001, "<0.001",sprintf("%.3f", model_2_table[,"Pr(>|z|)"])),
  row.names = NULL)
model_2_table

#POST-HOC tests
em_stand <- emmeans(model_removal_species, ~ Stand , type = "response")
stand_contrasts <- pairs(em_stand, adjust = "tukey");stand_contrasts
emmeans_table_format(stand_contrasts)
#write.csv(emmeans_table_format(stand_contrasts), "SeedPredationUS/Datasets/estimated_probabilities_m2_st.csv")
em_seeds <- emmeans(model_removal_species, ~ Seed_sp , type = "response")
seed_contrasts <- pairs(em_seeds, adjust = "tukey");seed_contrasts
emmeans_table_format(seed_contrasts)
#write.csv(emmeans_table_format(seed_contrasts), "SeedPredationUS/Datasets/estimated_probabilities_m2_sp.csv")

#PLOTS
stand_levels <- c("Low", "Mid", "High")
sp_levels <- c("Thuja plicata", "Tsuga heterophylla", "Pseudotsuga menziesii",
               "Abies amabilis", "Callitropsis nootkatensis", "Abies lasiocarpa")

em_stand <- emmeans(model_removal_species, ~ Stand,   type = "response")
em_seeds <- emmeans(model_removal_species, ~ Seed_sp, type = "response")

stand_df <- as.data.frame(em_stand) %>% 
  mutate(Stand = factor(Stand, levels = stand_levels))
seed_df  <- as.data.frame(em_seeds) %>% 
  mutate(Seed_sp = factor(Seed_sp, levels = sp_levels))

fam <- "serif"

# stand
stars_fun <- function(p) case_when(p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", TRUE ~ "")

stand_br <- as.data.frame(pairs(em_stand, adjust = "tukey")) %>%#adding the statistical differences
  mutate(contrast = gsub("[()]", "", as.character(contrast)),
         s1 = sapply(strsplit(contrast, " [/-] "), `[`, 1),
         s2 = sapply(strsplit(contrast, " [/-] "), `[`, 2),
         stars = stars_fun(p.value)) %>%
  filter(stars != "") %>%
  mutate(x = match(s1, stand_levels), xend = match(s2, stand_levels),y = max(stand_df$asymp.UCL) + 0.04 + 0.07 * (row_number() - 1))

g_stand <- ggplot(stand_df, aes(x = Stand, y = prob)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.15) +
  geom_point(size = 5) +
  geom_segment(data = stand_br, aes(x = x, xend = xend, y = y, yend = y), inherit.aes = FALSE) +
  geom_text(data = stand_br, aes(x = (x + xend) / 2, y = y + 0.005, label = stars),inherit.aes = FALSE, vjust = 0, size = 7) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2),labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Elevation (Stand)", y ="Predicted probability of seed removal") +
  theme_minimal(base_size = 18) +
  theme(axis.text.x = element_text(family = fam,size= 18, face = "bold"),
        axis.text.y  = element_text(family = fam,size= 18,face = "bold"),
        axis.title.x = element_text(family = fam,size= 20,face = "bold", margin= margin(t=10)),
        axis.title.y = element_text(family = fam,size= 20,face = "bold", margin= margin(r=10)),
        legend.title= element_text(family = fam,size= 18),
        legend.text= element_text(family = fam,size= 18, face= "bold"),
        legend.position = "right",
        panel.grid.minor = element_blank())
g_stand
#ggsave("model_rq2_stand.png", plot= g_stand,path= "SeedPredationUS/Plots/" )

# species
g_species <- ggplot(seed_df, aes(x = Seed_sp, y = prob)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.15) +
  geom_point(size = 5) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2),labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Seed species", y = "Predicted probability of seed removal") +
  theme_minimal(base_size = 18) +
  theme(axis.text.x = element_text(family = fam,size= 18, face = "italic",angle = 45, hjust = 1),
        axis.text.y  = element_text(family = fam,size= 18,face = "bold"),
        axis.title.x = element_text(family = fam,size= 20,face = "bold", margin= margin(t=10)),
        axis.title.y = element_text(family = fam,size= 20,face = "bold", margin= margin(r=10)),
        legend.title= element_text(family = fam,size= 18),
        legend.text= element_text(family = fam,size= 18, face= "italic"),
        legend.position = "right",
        panel.grid.minor = element_blank())
g_species

#adding the statistical differences: 
pw <- as.data.frame(pairs(em_seeds, adjust = "tukey"))
pvals <- setNames(pw$p.value, gsub(" / ", "-", gsub("[()]", "", as.character(pw$contrast))))
lt <- multcompView::multcompLetters(pvals)$Letters   
seed_df$.group <- lt[as.character(seed_df$Seed_sp)]

g_species_final<-g_species + geom_text(data = seed_df, aes(y = asymp.UCL + 0.03, label = .group),size = 6, vjust = 0)
g_species_final
#ggsave("model_rq2_species.png", plot= g_species_final,path= "SeedPredationUS/Plots/" )


#NOW CHECKING BETWEEN SPECIES AND STAND DIFFERENCES (as my previous model cannot converge due to the high removal of P. menziesii (~100%), We decided to go for a single species model to check how the stand (or elevation) impacted the removal of each seed species

#First, I select the model (check AIC and then anova to see if statistically different)
#abam
model_1<- glmmTMB(cbind(Success, seed_disposed - Success)~ 1,family = betabinomial(),   data= data_cleaned_2[data_cleaned_2$Seed_sp=="Abies amabilis",])
model_2<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand ,family = betabinomial(),   data= data_cleaned_2[data_cleaned_2$Seed_sp=="Abies amabilis",])
model_3<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand + (1 | Camera),family = betabinomial(),   data= data_cleaned_2[data_cleaned_2$Seed_sp=="Abies amabilis",])
model_4<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand+ (1|Week) ,family = betabinomial(),   data= data_cleaned_2[data_cleaned_2$Seed_sp=="Abies amabilis",])
model_full<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand + (1 | Camera)  + (1|Week), family = betabinomial(),   data= data_cleaned_2[data_cleaned_2$Seed_sp=="Abies amabilis",])
AIC(model_1, model_2, model_3, model_4, model_full)%>% arrange (AIC)
anova(model_full, model_2);anova(model_full, model_3);anova(model_full, model_4) 
#model only Full is the same than the ones above. 

#abla
model_1<- glmmTMB(cbind(Success, seed_disposed - Success)~ 1,family = betabinomial(),   data= data_cleaned_2[data_cleaned_2$Seed_sp=="Abies lasiocarpa",])
model_2<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand ,family = betabinomial(),   data= data_cleaned_2[data_cleaned_2$Seed_sp=="Abies lasiocarpa",])
model_3<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand + (1 | Camera),family = betabinomial(),   data= data_cleaned_2[data_cleaned_2$Seed_sp=="Abies lasiocarpa",])
model_4<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand+ (1|Week) ,family = betabinomial(),   data= data_cleaned_2[data_cleaned_2$Seed_sp=="Abies lasiocarpa",])
model_full<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand + (1 | Camera)  + (1|Week), family = betabinomial(),   data= data_cleaned_2[data_cleaned_2$Seed_sp=="Abies lasiocarpa",])
AIC(model_1, model_2, model_3, model_4, model_full)%>% arrange (AIC)
anova(model_full, model_4)
#no statisticall difference, I can take model full

#cano
model_1<- glmmTMB(cbind(Success, seed_disposed - Success)~ 1,family = betabinomial(),   data= data_cleaned_2[data_cleaned_2$Seed_sp=="Callitropsis nootkatensis",])
model_2<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand ,family = betabinomial(),   data= data_cleaned_2[data_cleaned_2$Seed_sp=="Callitropsis nootkatensis",])
model_3<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand + (1 | Camera),family = betabinomial(),   data= data_cleaned_2[data_cleaned_2$Seed_sp=="Callitropsis nootkatensis",])
model_4<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand+ (1|Week) ,family = betabinomial(),   data= data_cleaned_2[data_cleaned_2$Seed_sp=="Callitropsis nootkatensis",])
model_full<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand + (1 | Camera)  + (1|Week), family = betabinomial(),   data= data_cleaned_2[data_cleaned_2$Seed_sp=="Callitropsis nootkatensis",])
AIC(model_1, model_2, model_3, model_4, model_full)%>% arrange (AIC)
anova(model_full, model_4)
#no statisticall difference, I can take model full

#psme
model_1<- glmmTMB(cbind(Success, seed_disposed - Success)~ 1,family = betabinomial(),data= data_cleaned_2[data_cleaned_2$Seed_sp=="Pseudotsuga menziesii",])
model_2<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand ,family = betabinomial(),data= data_cleaned_2[data_cleaned_2$Seed_sp=="Pseudotsuga menziesii",])
model_3<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand + (1 | Camera),family = betabinomial(),data= data_cleaned_2[data_cleaned_2$Seed_sp=="Pseudotsuga menziesii",])
model_4<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand+ (1|Week) ,family = betabinomial(),data= data_cleaned_2[data_cleaned_2$Seed_sp=="Pseudotsuga menziesii",])
model_full<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand + (1 | Camera)  + (1|Week), family = betabinomial(),   data= data_cleaned_2[data_cleaned_2$Seed_sp=="Pseudotsuga menziesii",])
AIC(model_1, model_2, model_3, model_4, model_full)%>% arrange (AIC)
anova(model_full, model_2);anova(model_full, model_3)
#no statisticall difference, I can take model full

#tshe
model_1<- glmmTMB(cbind(Success, seed_disposed - Success)~ 1,family = betabinomial(),   data= data_cleaned_2[data_cleaned_2$Seed_sp=="Tsuga heterophylla",])
model_2<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand ,family = betabinomial(),   data= data_cleaned_2[data_cleaned_2$Seed_sp=="Tsuga heterophylla",])
model_3<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand + (1 | Camera),family = betabinomial(),   data= data_cleaned_2[data_cleaned_2$Seed_sp=="Tsuga heterophylla",])
model_4<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand+ (1|Week) ,family = betabinomial(),   data= data_cleaned_2[data_cleaned_2$Seed_sp=="Tsuga heterophylla",])
model_full<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand + (1 | Camera)  + (1|Week), family = betabinomial(),   data= data_cleaned_2[data_cleaned_2$Seed_sp=="Tsuga heterophylla",])
AIC(model_1, model_2, model_3, model_4, model_full)%>% arrange (AIC)
anova(model_full, model_2);anova(model_full, model_3);anova(model_full, model_4);anova(model_full, model_1)
#no statisticall difference, I can take model full

#thpl
model_1<- glmmTMB(cbind(Success, seed_disposed - Success)~ 1,family = betabinomial(), data= data_cleaned_2[data_cleaned_2$Seed_sp=="Thuja plicata",])
model_2<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand ,family = betabinomial(),   data= data_cleaned_2[data_cleaned_2$Seed_sp=="Thuja plicata",])
model_3<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand + (1 | Camera),family = betabinomial(),   data= data_cleaned_2[data_cleaned_2$Seed_sp=="Thuja plicata",])
model_4<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand+ (1|Week) ,family = betabinomial(),   data= data_cleaned_2[data_cleaned_2$Seed_sp=="Thuja plicata",])
model_full<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand + (1 | Camera)  + (1|Week), family = betabinomial(),   data= data_cleaned_2[data_cleaned_2$Seed_sp=="Thuja plicata",])
AIC(model_1, model_2, model_3, model_4, model_full)%>% arrange (AIC)
anova(model_full, model_4)
#no statisticall difference, I can take model full


#Final selected model: 
model_abam<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand+ (1 | Camera)  + (1|Week) ,family = betabinomial(),   data= data_cleaned_2[data_cleaned_2$Seed_sp=="Abies amabilis",])
model_abla<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand + (1 | Week)+ (1 | Camera)  ,family = betabinomial(),data= data_cleaned_2[data_cleaned_2$Seed_sp=="Abies lasiocarpa",])
model_cano<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand+ (1|Week)+ (1 | Camera)  ,family = betabinomial(),   data= data_cleaned_2[data_cleaned_2$Seed_sp=="Callitropsis nootkatensis",])
model_psme<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand+ (1 | Camera)  + (1|Week) ,family = betabinomial(),data= data_cleaned_2[data_cleaned_2$Seed_sp=="Pseudotsuga menziesii",])
model_tshe<- glmmTMB(cbind(Success, seed_disposed - Success)~ Stand+ (1 | Camera)  + (1|Week),family = betabinomial(),   data= data_cleaned_2[data_cleaned_2$Seed_sp=="Tsuga heterophylla",])
model_thpl<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand+ (1|Week)+ (1|Camera) ,family = betabinomial(),   data= data_cleaned_2[data_cleaned_2$Seed_sp=="Thuja plicata",])

#Then I check the residuals to see if my model fits my data
SimOUtput_1<-simulateResiduals(model_abam)
plot(SimOUtput_1)
testOverdispersion(SimOUtput_1)
testZeroInflation(SimOUtput_1) 
plotResiduals(model_abam, form = ~.)

SimOUtput_2<-simulateResiduals(model_abla)
plot(SimOUtput_2)
testOverdispersion(SimOUtput_2)
testZeroInflation(SimOUtput_2) 
plotResiduals(model_abla, form = ~.)

SimOUtput_3<-simulateResiduals(model_cano)
plot(SimOUtput_3)
testOverdispersion(SimOUtput_3)
testZeroInflation(SimOUtput_3) 
plotResiduals(model_cano, form = ~.)

SimOUtput_4<-simulateResiduals(model_psme)
plot(SimOUtput_4)
testOverdispersion(SimOUtput_4)
testZeroInflation(SimOUtput_4) 
plotResiduals(model_psme, form = ~.)

SimOUtput_5<-simulateResiduals(model_tshe)
plot(SimOUtput_5)
testOverdispersion(SimOUtput_5)
testZeroInflation(SimOUtput_5) 

SimOUtput_6<-simulateResiduals(model_thpl)
plot(SimOUtput_6)
testOverdispersion(SimOUtput_6)
testZeroInflation(SimOUtput_6) 
plotResiduals(model_thpl, form = ~.)

#they all look ok 

summary(model_abam)
summary(model_abla)
summary(model_cano)
summary(model_psme)
summary(model_tshe)
summary(model_thpl)

Anova(model_abam, type= "II")
Anova(model_abla, type= "II")
Anova(model_cano, type= "II")
Anova(model_psme, type= "II")
Anova(model_tshe, type= "II")
Anova(model_thpl, type= "II")

models <- list("Thuja plicata" = model_thpl,"Tsuga heterophylla"= model_tshe,"Pseudotsuga menziesii"= model_psme,"Abies amabilis" = model_abam,"Callitropsis nootkatensis" = model_cano,"Abies lasiocarpa" = model_abla)

make_coef_table <- function(model) {
  tab <- as.data.frame(summary(model)$coefficients$cond)
  lo  <- tab$Estimate - qnorm(0.975) * tab$`Std. Error`
  hi  <- tab$Estimate + qnorm(0.975) * tab$`Std. Error`
  data.frame(
    Term     = rownames(tab),
    Estimate = sprintf("%.3f", tab[, "Estimate"]),
    SE       = sprintf("%.3f", tab[, "Std. Error"]),
    `95% CI` = sprintf("[%.3f, %.3f]", lo, hi),
    z        = sprintf("%.3f", tab[, "z value"]),
    p        = ifelse(tab[, "Pr(>|z|)"] < 0.001, "<0.001", sprintf("%.3f", tab[, "Pr(>|z|)"])),
    row.names   = NULL,
    check.names = FALSE   
  )
}

# one table per species, as a named list
tables <- lapply(models, make_coef_table)
#tables[["Abies amabilis"]] 
all_tables <- bind_rows(tables, .id = "Species")
all_tables
#write.csv(all_tables, "SeedPredationUS/Datasets/coef_tables_all_species.csv",row.names = FALSE)

#checking the emmeans
em_abam <- emmeans(model_abam, ~ Stand , type = "response")
abam_contrasts <- pairs(em_abam, adjust = "tukey");abam_contrasts; emmeans_table_format(abam_contrasts)
em_abla <- emmeans(model_abla, ~ Stand , type = "response")
abla_contrasts <- pairs(em_abla, adjust = "tukey");abla_contrasts; emmeans_table_format(abla_contrasts)
em_cano <- emmeans(model_cano, ~ Stand , type = "response")
cano_contrasts <- pairs(em_cano, adjust = "tukey");cano_contrasts; emmeans_table_format(cano_contrasts)
em_psme <- emmeans(model_psme, ~ Stand , type = "response")
psme_contrasts <- pairs(em_psme, adjust = "tukey");psme_contrasts; emmeans_table_format(psme_contrasts)
em_tshe <- emmeans(model_tshe, ~ Stand , type = "response")
tshe_contrasts <- pairs(em_tshe, adjust = "tukey");tshe_contrasts; emmeans_table_format(tshe_contrasts)
em_thpl <- emmeans(model_thpl, ~ Stand , type = "response")
thpl_contrasts <- pairs(em_thpl, adjust = "tukey");thpl_contrasts; emmeans_table_format(thpl_contrasts)

get_contrasts <- function(m) {
  em <- emmeans(m, ~ Stand, type = "response")
  as.data.frame(summary(pairs(em, adjust = "tukey"), infer = c(TRUE, TRUE)))
}

all_emmeans <- bind_rows(lapply(models, get_contrasts), .id = "Species")
all_emmeans; emmeans_table_format(all_emmeans)
#write.csv(emmeans_table_format(all_emmeans), "SeedPredationUS/Datasets/estimated_probabilities_m2_SPONLY.csv")


#PLOT
stand_levels <- c("Low", "Mid", "High")

get_em <- function(m, sp) {
  if ("Stand" %in% all.vars(formula(m))) {
    out <- as.data.frame(emmeans(m, ~ Stand, type = "response"))
  } else {
    e <- as.data.frame(emmeans(m, ~ 1, type = "response"))
    out <- e[rep(1, length(stand_levels)), ]
    out$Stand <- stand_levels
  }
  out$Species <- sp
  out[, c("Species", "Stand", "prob", "asymp.LCL", "asymp.UCL")]
}

em_all <- bind_rows(Map(get_em, models, names(models))) %>%
  mutate(Stand   = factor(Stand, levels = stand_levels),Species = factor(Species, levels = names(models)))

get_contr <- function(m, sp) {
  if (!"Stand" %in% all.vars(formula(m))) return(NULL)   
  p <- as.data.frame(pairs(emmeans(m, ~ Stand), adjust = "tukey"))
  parts <- strsplit(as.character(p$contrast), " [/-] ")
  data.frame(Species = sp,s1 = sapply(parts, `[`, 1),s2 = sapply(parts, `[`, 2),p.value = p$p.value)
}

contr <- bind_rows(Map(get_contr, models, names(models))) %>%
  mutate(stars = case_when(p.value < 0.001 ~ "***", p.value < 0.01  ~ "**",p.value < 0.05  ~ "*",TRUE ~ "")) %>%
  filter(stars != "")

dodge_w <- 0.7
xoff <- function(stand) (match(stand, stand_levels) - 2) * dodge_w / 3  

top <- em_all %>% group_by(Species) %>% summarise(ymax = max(asymp.UCL))

brackets <- contr %>%
  left_join(top, by = "Species") %>%
  mutate(sp_num = as.numeric(factor(Species, levels = names(models))),x = sp_num + xoff(s1),xend = sp_num + xoff(s2)) %>%
  group_by(Species) %>%
  mutate(y = ymax + 0.04 + 0.07 * (row_number() - 1)) %>%   
  ungroup()

species_stand_plot<-ggplot(em_all, aes(x = Species, y = prob, shape = Stand, fill = Stand)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),position = position_dodge(width = dodge_w), width = 0.2) +
  geom_point(position = position_dodge(width = dodge_w), size = 5, colour = "black") +
  geom_segment(data = brackets, aes(x = x, xend = xend, y = y, yend = y),inherit.aes = FALSE) +
  geom_segment(data = brackets, aes(x = x,    xend = x,    y = y, yend = y - 0.015),inherit.aes = FALSE) +
  geom_segment(data = brackets, aes(x = xend, xend = xend, y = y, yend = y - 0.015),inherit.aes = FALSE) +
  geom_text(data = brackets, aes(x = (x + xend) / 2, y = y + 0.005, label = stars),inherit.aes = FALSE, vjust = 0, size = 5) +
  scale_shape_manual(values = c(Low = 21, Mid = 22, High = 24)) +
  scale_fill_manual(values = c(Low = "white", Mid = "grey60", High = "black")) +
  #scale_y_continuous(breaks = seq(0, 1, 0.2), labels = scales::percent,limits = c(0, 1.2)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2),labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Seed species", y = "Predicted probability of seed removal",shape = "Stand", fill = "Stand", caption = "* p < 0.05, ** p < 0.01, *** p < 0.001 (Tukey-adjusted pairwise contrasts within species)") +
  theme_minimal(base_size = 18) +
  #theme(axis.text.x = element_text(face = "italic", angle = 30, hjust = 1), panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(family = fam,size= 18, face = "italic",angle = 45, hjust = 1),
        axis.text.y  = element_text(family = fam,size= 18,face = "bold"),
        axis.title.x = element_text(family = fam,size= 20,face = "bold", margin= margin(t=10)),
        axis.title.y = element_text(family = fam,size= 20,face = "bold", margin= margin(r=10)),
        legend.title= element_text(family = fam,size= 18),
        legend.text= element_text(family = fam,size= 18, face= "italic"),
        legend.position = "right",
        panel.grid.minor = element_blank())
species_stand_plot

#ggsave("model_rq2_species_stand.png", plot= species_stand_plot,path= "SeedPredationUS/Plots/" )


#Plotting everythin for the manuscript (Figure X) ()
# Abbreviating species to gain some space on the plot
abbrev_sp <- function(x) sub("^(\\w)\\w*\\s", "\\1. ", x)

#design of the PANEL (using the comp package)
design <- "
AAB
CCC
"

# top row: A1 (species) and A2 (stand), tagged as a sub-level
top_row <- (g_species_final + scale_x_discrete(labels = abbrev_sp) | g_stand) +
  plot_layout(widths = c(2, 1), tag_level = "new")

panel_R2 <- (top_row / (species_stand_plot + scale_x_discrete(labels = abbrev_sp))) +
  plot_layout(heights = c(1, 1.2)) +
  plot_annotation(tag_levels = c("A", "1")) &
  labs(y = "Predicted probability\nof seed removal") &
  theme(axis.title.y = element_text(family = "serif", size = 16, face = "bold", margin = margin(r = 8)),
        plot.tag     = element_text(family = "serif", size = 22, face = "bold"))
panel_R2
#ggsave("panel_R2.png", plot= panel_R2,path= "SeedPredationUS/Plots/" )

# 8. Research question 3a: Elevation group --------------------------------------------------
#Checking by species group (low vs high species)
data_cleaned_2 <- data_cleaned_2 %>%
  mutate(Elevation_group = case_when(
    Seed_sp %in% c("Abies lasiocarpa", "Abies amabilis", "Callitropsis nootkatensis") ~ "High",
    Seed_sp %in% c("Thuja plicata", "Tsuga heterophylla", "Pseudotsuga menziesii") ~ "Low",
    TRUE ~ NA_character_
  ))

data_cleaned_2$Elevation_group <- factor (data_cleaned_2$Elevation_group, levels = c("Low", "High"))

#Are species more removed when they are in their naturally occuring range?

#Building my model
model_elevation_null <- glmmTMB(  cbind(Success, seed_disposed - Success) ~ 1 , family = betabinomial,  data = data_cleaned_2)

model_elevation_simple <- glmmTMB(  cbind(Success, seed_disposed - Success) ~ Elevation_group + Stand  ,family = betabinomial(),  data = data_cleaned_2)

model_elevation_simple_int <- glmmTMB(  cbind(Success, seed_disposed - Success) ~ Elevation_group * Stand  ,family = betabinomial(),  data = data_cleaned_2)

model_elevation_simple2 <- glmmTMB(  cbind(Success, seed_disposed - Success) ~ Elevation_group + Stand +(1 | Seed_sp),
  family = betabinomial (),  data = data_cleaned_2)

model_elevation_simple3 <- glmmTMB(  cbind(Success, seed_disposed - Success) ~ Elevation_group + Stand +(1 | Seed_sp) + (1|Week),  family = betabinomial(),  data = data_cleaned_2)

model_elevation_simple4 <- glmmTMB(  cbind(Success, seed_disposed - Success) ~ Elevation_group + Stand + (1|Week) + (1|Camera) ,  family = betabinomial(),  data = data_cleaned_2)

model_elevation_simple5 <- glmmTMB(  cbind(Success, seed_disposed - Success) ~ Elevation_group + Stand +(1|Seed_sp) + (1|Camera) ,  family = betabinomial(),  data = data_cleaned_2)

model_elevation_full <- glmmTMB(  cbind(Success, seed_disposed - Success) ~ Elevation_group + Stand +  (1 | Seed_sp) + (1|Week) + (1|Camera) ,family = betabinomial(),data = data_cleaned_2)

model_elevation_full_int <- glmmTMB(  cbind(Success, seed_disposed - Success) ~ Elevation_group * Stand +  (1 | Seed_sp) + (1|Week) + (1|Camera) ,family = betabinomial(), data = data_cleaned_2)

AIC(model_elevation_null, model_elevation_simple, model_elevation_simple_int, model_elevation_simple2, model_elevation_simple3, model_elevation_simple4, model_elevation_simple5, model_elevation_full,model_elevation_full_int)%>%  arrange(AIC)

#best model based on AIC scoring is the full one, but does it fit?

anova(model_elevation_full,model_elevation_full_int)
#not statistically different, so lets see with the interaction: 

sim_res <- simulateResiduals(model_elevation_full_int)
plot(sim_res)#looks good
testOverdispersion(sim_res)
testZeroInflation(sim_res)
plotResiduals(model_elevation_full, form= ~.)

summary(model_elevation_full_int)
model_3_table<-as.data.frame(summary(model_elevation_full_int)$coefficients$cond)
Anova(model_elevation_full_int, type="II")

lo<-model_3_table$Estimate - qnorm(0.975)*model_3_table$`Std. Error`
hi<-model_3_table$Estimate + qnorm(0.975)*model_3_table$`Std. Error`
model_3_table <-data.frame(
  Term= rownames (model_3_table), 
  Estimate= sprintf("%.3f", model_3_table[,"Estimate"]),
  SE= sprintf("%.3f", model_3_table[,"Std. Error"]),
  `95% CI`= sprintf("[%.3f, %.3f]",lo,hi),
  z= sprintf("%.3f", model_3_table[,"z value"]),
  p= ifelse(model_3_table[,"Pr(>|z|)"] <0.001, "<0.001",sprintf("%.3f", model_3_table[,"Pr(>|z|)"])), row.names = NULL)
model_3_table

# POST-HOC Test
#is there a difference between the elevation group?
em3_elevation<- emmeans(model_elevation_full_int, ~ Elevation_group, type = "response")
elevation_comp_m3<-pairs(em3_elevation); elevation_comp_m3
write.csv(emmeans_table_format(elevation_comp_m3), "SeedPredationUS/Datasets/estimated_probabilities_m3_elev.csv")
#Is there a difference between the elevation group per stands?
em3 <- emmeans(model_elevation_full_int, ~ Elevation_group * Stand, type = "response");em_df3 <- as.data.frame(em3)
all_comp_m3<-pairs(em3, by= "Stand"); all_comp_m3
write.csv(emmeans_table_format(all_comp_m3), "SeedPredationUS/Datasets/estimated_probabilities_m3_all.csv")

# Plot of removal probability by stand and elevation group. 
em_df3<-em_df3%>%mutate(Stand = factor(Stand, levels = stand_levels))
elev_group_plot<-ggplot(em_df3, aes(x = as.factor(Stand), y = prob, color = as.factor(Elevation_group), group = as.factor(Elevation_group))) +
  geom_point(position = position_dodge(width = 0.3), size = 5) +
  geom_line(position = position_dodge(width = 0.3), linewidth = 1.2) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, position = position_dodge(width = 0.3)) +
  scale_color_manual(values = c("Low" = "black", "High" = "grey")) +
  scale_y_continuous(
    limits = c(0,1) ,
    breaks = seq(0, 1, by = 0.1),
    labels = scales::percent_format(accuracy = 1), 
    expand = c(0,0.01))+
  labs(#title = "Predicted Seed Removal Probability by Elevation Group and Stand",
    y = "Predicted Probability of Seed Removal",
    x = "Elevation (Stand)",
    color = "Seed Elevation Group") +
  theme_minimal() +
  theme(axis.text.x  = element_text(family = "serif", size = 16, face = "bold",angle = 45, hjust = 1),
    axis.text.y  = element_text(family = "serif", size = 16, face = "bold"),
    axis.title   = element_text(family = "serif", size = 16, face = "bold"),
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 16),
    legend.position = "right")
elev_group_plot

#ggsave("elev_group_plot.png", plot= elev_group_plot,path= "SeedPredationUS/Plots/" )


# 9. Research question 3b: Treatment ---------------------------------------------------------------
#**Model 4**: Are species more removed when they are around conspecific?

data_cleaned_2$context<- ifelse(data_cleaned_2$Treatment=="All", "Mixed", "Same-range only")
data_cleaned_2$context <-factor(data_cleaned_2$context, levels = c("Same-range only", "Mixed"))


model_removal_treatment_effect_null<- glmmTMB(cbind(Success, seed_disposed - Success)~1,family = betabinomial(),   data= data_cleaned_2)

model_removal_treatment_effect_simple<- glmmTMB(cbind(Success, seed_disposed - Success)~ Stand * context + Seed_sp + context, family = betabinomial(),   data= data_cleaned_2)

model_removal_treatment_effect_simple2<- glmmTMB(cbind(Success, seed_disposed - Success)~ Stand * context+Seed_sp + context +(1 | Camera) ,family = betabinomial(),   data= data_cleaned_2)

model_removal_treatment_effect_simple3<- glmmTMB(cbind(Success, seed_disposed - Success)~ Stand * context+Seed_sp + context +(1 | Week) , family = betabinomial(),   data= data_cleaned_2)

model_removal_treatment_effect<- glmmTMB(cbind(Success, seed_disposed - Success)~ Stand * context+Seed_sp + context +(1 | Camera)  + (1|Week),family = betabinomial(),   data= data_cleaned_2)

model_removal_treatment_effect_simple_int<- glmmTMB(cbind(Success, seed_disposed - Success)~ Stand * context+Seed_sp * context,  family = betabinomial(),   data= data_cleaned_2)

model_removal_treatment_effect_simple2_int<- glmmTMB(cbind(Success, seed_disposed - Success)~ Stand * context+Seed_sp * context +(1 | Camera) ,family = betabinomial(),   data= data_cleaned_2)

model_removal_treatment_effect_simple3_int<- glmmTMB(cbind(Success, seed_disposed - Success)~ Stand * context+Seed_sp * context +(1 | Week) , family = betabinomial(),   data= data_cleaned_2)

model_removal_treatment_effect_int<- glmmTMB(cbind(Success, seed_disposed - Success)~ Stand * context+Seed_sp * context +(1 | Camera)  + (1|Week),family = betabinomial(),   data= data_cleaned_2)

model_removal_treatment_effect_simple_og<- glmmTMB(cbind(Success, seed_disposed - Success)~ Stand + context + Seed_sp, family = betabinomial(),   data= data_cleaned_2)

model_removal_treatment_effect_simple2_og<- glmmTMB(cbind(Success, seed_disposed - Success)~ Stand +Seed_sp + context +(1 | Camera) ,family = betabinomial(),   data= data_cleaned_2)

model_removal_treatment_effect_simple3_og<- glmmTMB(cbind(Success, seed_disposed - Success)~ Stand +Seed_sp + context +(1 | Week) , family = betabinomial(),   data= data_cleaned_2)

model_removal_treatment_effect_og<- glmmTMB(cbind(Success, seed_disposed - Success)~ Stand +Seed_sp + context +(1 | Camera)  + (1|Week),family = betabinomial(),   data= data_cleaned_2)

model_removal_treatment_effect_simple_int_og<- glmmTMB(cbind(Success, seed_disposed - Success)~ Stand +Seed_sp * context,  family = betabinomial(),   data= data_cleaned_2)

model_removal_treatment_effect_simple2_int_og<- glmmTMB(cbind(Success, seed_disposed - Success)~ Stand +Seed_sp * context +(1 | Camera) ,family = betabinomial(),   data= data_cleaned_2)

model_removal_treatment_effect_simple3_int_og<- glmmTMB(cbind(Success, seed_disposed - Success)~ Stand +Seed_sp * context +(1 | Week) , family = betabinomial(),   data= data_cleaned_2)

model_removal_treatment_effect_int_og<- glmmTMB(cbind(Success, seed_disposed - Success)~ Stand +Seed_sp * context +(1 | Camera)  + (1|Week),family = betabinomial(),   data= data_cleaned_2)


AIC(model_removal_treatment_effect_null,model_removal_treatment_effect_simple,model_removal_treatment_effect_simple2,model_removal_treatment_effect_simple3,model_removal_treatment_effect, model_removal_treatment_effect_simple_int,model_removal_treatment_effect_simple2_int,model_removal_treatment_effect_simple3_int,model_removal_treatment_effect_int, model_removal_treatment_effect_simple_og,model_removal_treatment_effect_simple2_og,model_removal_treatment_effect_simple3_og,model_removal_treatment_effect_og, model_removal_treatment_effect_simple_int_og,model_removal_treatment_effect_simple2_int_og,model_removal_treatment_effect_simple3_int_og,model_removal_treatment_effect_int_og ) %>%  arrange(AIC)

anova(model_removal_treatment_effect, model_removal_treatment_effect_int) #not statistically different, so will use this once instead

sim_res_m4 <- simulateResiduals(model_removal_treatment_effect_int)
plot(sim_res_m4)#looks good
testOverdispersion(sim_res_m4)
testZeroInflation(sim_res_m4)

summary(model_removal_treatment_effect_int)

Anova(model_removal_treatment_effect_int, type= "II")


model_4_table<-as.data.frame( summary(model_removal_treatment_effect_int)$coefficients$cond)
lo<-model_4_table$Estimate - qnorm(0.975)*model_4_table$`Std. Error`
hi<-model_4_table$Estimate + qnorm(0.975)*model_4_table$`Std. Error`
model_4_table <-data.frame(
  Term= rownames (model_4_table), 
  Estimate= sprintf("%.3f", model_4_table[,"Estimate"]),
  SE= sprintf("%.3f", model_4_table[,"Std. Error"]),
  `95% CI`= sprintf("[%.3f, %.3f]",lo,hi),
  z= sprintf("%.3f", model_4_table[,"z value"]),
  p= ifelse(model_4_table[,"Pr(>|z|)"] <0.001, "<0.001",sprintf("%.3f", model_4_table[,"Pr(>|z|)"])),row.names = NULL)
model_4_table



Anova(model_removal_treatment_effect_int, type = "II")   # is Stand:context significant?


#is there a difference between the treatment group?
em4_treatment<- emmeans(model_removal_treatment_effect_int, ~ context, type = "response")
treatment_comp_m4<-pairs(em4_treatment); treatment_comp_m4
write.csv(emmeans_table_format(treatment_comp_m4), "SeedPredationUS/Datasets/estimated_probabilities_m4_treatmentTRUE.csv")
#is there a difference between the treatment group the stand?
em4_stand<- emmeans(model_removal_treatment_effect_int, ~ Stand* context, type = "response")
stand_comp_m4<-pairs(em4_stand); stand_comp_m4
write.csv(emmeans_table_format(stand_comp_m4), "SeedPredationUS/Datasets/estimated_probabilities_m4_standTRUE.csv")
#Is there a difference between the treatment group per species?
em4_species <- emmeans(model_removal_treatment_effect_int, ~ Seed_sp * context, type = "response")
all_comp_m4<-pairs(em4_species, by= "Seed_sp"); all_comp_m4
write.csv(emmeans_table_format(all_comp_m4), "SeedPredationUS/Datasets/estimated_probabilities_m4_species_TRUE.csv")

m <- model_removal_treatment_effect_int
stand_levels <- c("Low", "Mid", "High")
sp_levels <- c("Thuja plicata", "Tsuga heterophylla", "Pseudotsuga menziesii","Abies amabilis", "Callitropsis nootkatensis", "Abies lasiocarpa")

fam <- "serif"

em_st <- as.data.frame(emmeans(m, ~ context | Stand,   type = "response")) %>%
  mutate(Stand = factor(Stand, levels = stand_levels))
em_sp <- as.data.frame(emmeans(m, ~ context | Seed_sp, type = "response")) %>%
  mutate(Seed_sp = factor(Seed_sp, levels = sp_levels))

plot_ctx <- function(df, xvar, xlab, x_face) {
  ggplot(df, aes(x = .data[[xvar]], y = prob, shape = context, fill = context)) +
    geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                  position = position_dodge(0.5), width = 0.2) +
    geom_point(position = position_dodge(0.5), size = 4, colour = "black") +
    scale_shape_manual(values = c("Same-range only" = 21, "Mixed" = 23)) +
    scale_fill_manual(values  = c("Same-range only" = "white", "Mixed" = "black")) +
    scale_y_continuous(breaks = seq(0, 1, 0.2), labels = scales::percent_format(accuracy = 1)) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = xlab, y = "Predicted probability of seed removal", shape = "Context", fill = "Context") +
    theme_minimal(base_size = 16) +
    theme(axis.text.x  = element_text(family = fam, size = 15, face = x_face, angle = 45, hjust = 1),
          axis.text.y  = element_text(family = fam, size = 15, face = "bold"),
          axis.title   = element_text(family = fam, size = 17, face = "bold"),
          legend.title = element_text(family = fam, size = 15),
          legend.text  = element_text(family = fam, size = 15),
          panel.grid.minor = element_blank())
}

p_stand   <- plot_ctx(em_st,  "Stand",   "Elevation (Stand)", "bold")
p_species <- plot_ctx(em_sp,  "Seed_sp", "Seed species",      "italic") +
  theme(axis.title.y = element_blank())

m4_emmeans_plot <- (p_stand | p_species) +
  plot_layout(guides = "collect", widths = c(1, 2))
m4_emmeans_plot

#ggsave("m4_emmeans_plot.png", plot = m4_emmeans_plot, path = "SeedPredationUS/Plots/")


#PANEL for Figure X

ylab_2lines <- "Predicted probability\nof seed removal"

right_col <- (p_stand / p_species) +
  plot_layout(heights = c(1, 1.3), tag_level = "new") &
  labs(y = ylab_2lines)

panel_R3b <- (elev_group_plot | right_col) +
  plot_layout(widths = c(1, 1.3), guides = "collect") +
  plot_annotation(tag_levels = c("A", "1"), tag_sep = ".") &
  theme(legend.position      = "right",
        legend.box           = "vertical",   
        legend.box.just      = "left",       
        legend.justification = "center",
        plot.tag = element_text(family = "serif", size = 22, face = "bold"))

panel_R3b

ggsave(file.path("SeedPredationUS/Plots", "panel_R3b.png"), plot = panel_R3b,)


# 10. Additional plots for manuscript ----------------------------------------------------

sp_levels <- c("Thuja plicata", "Tsuga heterophylla", "Pseudotsuga menziesii","Abies amabilis", "Callitropsis nootkatensis", "Abies lasiocarpa")

ranges<-data.frame(seed_id= c( "Abies lasiocarpa","Abies amabilis","Callitropsis nootkatensis","Pseudotsuga menziesii", "Tsuga heterophylla","Thuja plicata"), min_elevation=  c(1200,600,750,0,0,0),max_elevation =  c(2000,1500,2000,1500,1200,1100))%>%
  mutate(seed_id = factor(seed_id),seed_id= reorder (seed_id, min_elevation))

stand_table <- data.frame(
  stand = c("TO11","TA01","TO04","TB13","AV02","AO03","AG05","AV06","AB08","AX15","AV14","PP17","AM16","AE10","AR07","PARA","SPRY","SUNR"),
  species_present = c("PSME,THPL,TSHE","ABAM,PSME,THPL,TSHE","ABAM,PSME,THPL,TSHE,",
                      "ABAM,PSME,THPL,TSHE","ABAM,PSME,THPL,TSHE","ABAM,PSME,THPL,TSHE",
                      "ABAM,PSME,THPL,TSHE","ABAM,CANO,PSME,THPL,TSHE","ABAM,CANO,PSME,THPL,TSHE",
                      "ABAM,CANO,PSME,THPL,TSHE","ABAM,CANO,TSHE","ABAM,ABLA,CANO,PSME,TSHE,",
                      "ABAM,CANO,TSHE,","ABAM,ABLA,CANO","ABAM,CANO","ABAM,ABLA,CANO",
                      "ABAM,ABLA,CANO","ABAM,ABLA,CANO"),
  elevation = c(564,630,643,792,835,854,913,1035,1042,1062,1081,1128,1170,1431,1435,1595,1762,1797))

#For separating the stands we have and the ones that are from the PSP monitoring campain
removal_stands <- c(TO04 = "Low", AV06 = "Mid", AE10 = "High")

stands <- stand_table %>%
  filter(stand %in% names(removal_stands)) %>%
  mutate(Stand = removal_stands[stand])

other_stands <- stand_table %>%
  filter(!stand %in% names(removal_stands))

#Species names
code_names <- c(THPL = "Thuja plicata",
                TSHE = "Tsuga heterophylla",
                PSME = "Pseudotsuga menziesii",
                ABAM = "Abies amabilis",
                CANO = "Callitropsis nootkatensis",
                ABLA = "Abies lasiocarpa")
abbrev_sp <- function(x) sub("^(\\w)\\w*\\s", "\\1. ", x)


#additional things for the plots: 
stand_breaks<-stand_table$elevation

stand_labels<-paste0(stand_table$Stand, "\n", stand_table$elevation, " m")

stand_table_plot<-stand_table%>%
  filter(!stand %in% c("TO04","AV06", "AE10"))


sp_levels <- c("Thuja plicata", "Tsuga heterophylla", "Pseudotsuga menziesii","Abies amabilis", "Callitropsis nootkatensis", "Abies lasiocarpa")




# One row per stand x species present
presence <- stand_table %>%
  separate_rows(species_present, sep = ",") %>%
  mutate(species_present = trimws(species_present)) %>%
  filter(species_present != "") %>%
  mutate(seed_id = factor(code_names[species_present], levels = sp_levels))

#PLOT
plot_elevation_stands <- ggplot() +
  geom_vline(data = stands, aes(xintercept = elevation),linetype = "dashed", colour = "grey30", linewidth = 1.4) +
  geom_vline(data = other_stands, aes(xintercept = elevation), linetype = "dashed", colour = "lightblue", linewidth = 1.2) +
  geom_segment(data = ranges, aes(x = min_elevation, xend = max_elevation, y = seed_id, yend = seed_id),linewidth = 7, lineend = "butt", colour = "grey85") +
  geom_point(data = presence, aes(x = elevation, y = seed_id), size = 2.5, colour = "black") +
  geom_text(data = stands, aes(x = elevation, y = Inf, label = Stand),angle = 90, vjust = -0.4, hjust = 1.1, size = 6, family = "serif") +
  scale_x_continuous(breaks = seq(0, 2000, by = 250)) +
  scale_y_discrete(limits= sp_levels,labels = abbrev_sp, drop = FALSE) +
  labs(x = "Elevation (m a.s.l.)", y = NULL) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        axis.text.y = element_text(face = "italic", size = 18),
        axis.text.x= element_text(size = 16, face = "bold"),
        axis.title.x  = element_text(size = 20, face = "bold", margin = margin(t = 10)),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(40, 20, 10, 10))
plot_elevation_stands

#ggsave("plot_elevation_stands.png", plot = plot_elevation_stands,
       path = "SeedPredationUS/Plots/", width = 12, height = 6, dpi = 300)
