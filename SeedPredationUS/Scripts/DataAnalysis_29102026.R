####Code for CAMERA TRAP EXPERIMENT US 2017
####Last modified : 28.01.2025
####ETHZ Eléonore Perret
####Code for measuring seed removal.  


R.version.string                      # R version
packageVersion("glmmTMB")
packageVersion("DHARMa")
packageVersion("emmeans")
packageVersion("car")
rstudioapi::versionInfo()$version     # RStudio version (run inside RStudio)
sessionInfo()                         # everything at once
citation("glmmTMB")                   # the reference for each package
citation("DHARMa"); citation("emmeans")

# Loading librairies ------------------------------------------------------
# install.packages("dplyr")
# install.packages("ggplot2")
#install.packages("DHARMa")
#install.packages("emmeans")
#install.packages("extrafont")
# install.packages("car")
# install.packages("ggtext")
#install.packages("vegan")
# install.packages("lubridate")

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
font_impfont_impfont_import()

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
load("SeedPredationUS/Datasets/predator_data_clean.RData")
load("SeedPredationUS/Datasets/predator_data_clean_tray.RData")


# Function ----------------------------------------------------------------

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
# CAMERA TRAP DATA Information : Process_data ------------------------------------------------------------
#Because I have empty rows after the row 27 (this comes from excel), I will first delete all the rows below
seed_predation <- seed_predation %>% slice(1:27)

#Changing names for clarity and plots 
# For Stand
predators_data$Stand <- factor(
  predators_data$Stand, 
  levels = c("TO04", "AV06", "AE10"), 
  labels = c("Low", "Mid", "High")
)
# For Species_ID
predators_data$Species_ID <- factor(
  predators_data$Species_ID,
  levels = c("Peromyscus maniculatus", "Tamias sp", "Lepus americanus", "Bird", "Flying squirrel", "Shrew?", "vole?", "Zapus?", "???"),
  labels = c("Peromyscus maniculatus", "Tamias spp.", "Lepus americanus", "Birds", "Glaucomys oregonensis", "Shrew spp.", "Vole spp.", "Zapus spp.", "Not identified")
)


# Filter to rows where an animal was present and the Species_ID is not missing or "NA" and were the animal was on the tray
predator_data_clean <- predators_data %>%
  filter(Animal_presence == "Yes", !is.na(Species_ID), Species_ID != "NA")
predator_data_clean_tray <- predators_data %>%
  filter(Animal_presence == "Yes", !is.na(Species_ID), Species_ID != "NA")%>%
  filter(On_tray == "On")




save(predator_data_clean, file = "SeedPredationUS/Datasets/predator_data_clean.RData")
save(predator_data_clean_tray, file = "SeedPredationUS/Datasets/predator_data_clean_tray.RData")

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
  Stand = c("Low", "Mid", "High"),
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

predator_density_site_species_standardized%>%
  group_by(Stand)%>%
  summarise(mean_stand= mean(predator_detections/Weeks_sampled))



# SEED DATA: Process data -End Product (data_cleaned_2.RData) ---------------------------------------------------

# Data Cleaning 

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



# Plots : study site ------------------------------------------------------
stands<-data.frame(
  Stand= c("Low", "Mid", "High"),
  elevation = c( 643, 1035, 1431)
)

ranges<-data.frame(
  seed_id= c( "Abies lasiocarpa","Abies amabilis","Callitropsis nootkatensis","Pseudotsuga menziesii", "Tsuga heterophylla","Thuja plicata"),
  min_elevation=  c(1200,600,750,0,0,0),
  max_elevation =  c(2000,1500,2000,1500,1200,1100)
)
ranges<-ranges%>%
  mutate(seed_id = factor(seed_id),
         seed_id= reorder (seed_id, min_elevation))

elevation_plot<-ggplot(ranges)+
  geom_vline(data= stands, aes(xintercept= elevation), linetype="dashed", color= "grey30")+
  geom_segment(aes(x=min_elevation, xend= max_elevation, y= seed_id, yend= seed_id), linewidth= 7, lineend= "butt")+
  geom_text(data=stands, aes(x=elevation, y=Inf, label= Stand), angle= 90, vjust= -0.4, hjust= 1.1, size= 3.5, family= "serif")+
  scale_x_continuous(breaks = seq(0,2000, by= 250))+
  theme_minimal()+
  theme(axis.text.y= element_text(face= "italic", size= 11),
        panel.grid.major.y = element_blank(),
        text= element_text(family="serif"),
        plot.margin = margin(10,20,10,10))
elevation_plot
ggsave("elevation_plot.png", plot= elevation_plot,path= "SeedPredationUS/Plots/" )


# 1. Stand table 
stand_table <- data.frame(
  stand = c("TO11","TA01","TO04","TB13","AV02","AO03","AG05","AV06","AB08",
            "AX15","AV14","PP17","AM16","AE10","AR07","PARA","SPRY","SUNR"),
  species_present = c("PSME,THPL,TSHE","ABAM,PSME,THPL,TSHE","ABAM,PSME,THPL,TSHE,",
                      "ABAM,PSME,THPL,TSHE","ABAM,PSME,THPL,TSHE","ABAM,PSME,THPL,TSHE",
                      "ABAM,PSME,THPL,TSHE","ABAM,CANO,PSME,THPL,TSHE","ABAM,CANO,PSME,THPL,TSHE",
                      "ABAM,CANO,PSME,THPL,TSHE","ABAM,CANO,TSHE","ABAM,ABLA,CANO,PSME,TSHE,",
                      "ABAM,CANO,TSHE,","ABAM,ABLA,CANO","ABAM,CANO","ABAM,ABLA,CANO",
                      "ABAM,ABLA,CANO","ABAM,ABLA,CANO"),
  elevation = c(564,630,643,792,835,854,913,1035,1042,1062,1081,1128,1170,
                1431,1435,1595,1762,1797)
)
##Check elevations

code_names <- c(ABAM = "Abies amabilis",
                ABLA = "Abies lasiocarpa",
                CANO = "Callitropsis nootkatensis",
                PSME = "Pseudotsuga menziesii",
                TSHE = "Tsuga heterophylla",
                THPL = "Thuja plicata")

# One row per stand x species 
presence <- stand_table %>%
  separate_rows(species_present, sep = ",") %>%
  mutate(species_present = trimws(species_present)) %>%
  filter(species_present != "") %>%
  mutate(seed_id = factor(code_names[species_present],
                          levels = levels(ranges$seed_id)))

stand_breaks<-stand_table$elevation
stand_labels<-paste0(stand_table$Stand, "\n", stand_table$elevation, " m")

stand_table_plot<-stand_table%>%
  filter(!stand %in% c("TO04","AV06", "AE10"))

plot_elevation_stands<-ggplot() +
  geom_vline(data = stands, aes(xintercept = elevation),
             linetype = "dashed", color = "grey30", linewidth= 1.4) +
  geom_vline(data = stand_table_plot, aes(xintercept = elevation),
             linetype = "dashed", color = "lightblue") +
  geom_segment(data = ranges,
               aes(x = min_elevation, xend = max_elevation,
                   y = seed_id, yend = seed_id),
               linewidth = 7, lineend = "butt", color = "grey85") +
  geom_point(data = presence, aes(x = elevation, y = seed_id),
             size = 2.5, color = "black") +
  geom_text(data = stands, aes(x = elevation, y = Inf, label = Stand),angle = 90, vjust = -0.4, hjust = 1.1, size = 6, family = "serif") +
  scale_x_continuous(breaks = seq(0, 2000, by = 250)) +
  labs(x = "Elevation (m a.s.l.)", y = NULL) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(legend.position = "bottom",
    axis.text.y        = element_text(face = "italic", size = 16),
    panel.grid.major.y = element_blank(),
    text               = element_text(family = "serif"),
    plot.margin        = margin(40, 20, 10, 10)
  )
plot_elevation_stands

ggsave("plot_elevation_stands.png", plot= plot_elevation_stands,path= "SeedPredationUS/Plots/" )


#Add seed data all in my dataset?
seed2018<-SeedData_all%>%
  filter(year%in% c("2018","2019"))%>%
  filter(spp%in%c("ABAM", "ABLA", "CANO", "PSME", "TSHE", "THPL"))%>%
  filter(stand%in% c("TO04", "AV06", "AE10"))%>%
  group_by(spp, stand, year)%>%
  summarise(mean_seed= sum(loose_sds_filled+loose_sds_unfilled+cones_sds_filled+cones_sds_unfilled))

# Plots seed predator -----------------------------------------------------


#Plots : 
#Color for species
species_colors <- c(
  "Peromyscus maniculatus" = "#4477AA",  
  "Tamias sp." = "#66CCEE",               
  "Lepus americanus" = "#228833",         
  "Birds" = "#CCBB44",                    
  "Glaucomys oregonensis" = "#EE6677",  
  "Shrew spp." = "#AA3377",                
  "Vole spp." = "#BBBBBB",                 
  "Zapus spp." = "#44AA99",                
  "Not identified" = "#DDDDDD"           
)

#Normal (overall detections)
ggplot(predator_density_site_species, aes(x = Stand, y = predator_detections, fill = Species_ID)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = species_colors) +
  labs(title = "Predator Detections per Site and Species",
       x = "Site (Stand)",
       y = "Number of Detections",
       fill = "Species") +
  theme_minimal()

#Standardized
predator_density_site_species_standardized$Stand <- factor(
  predator_density_site_species_standardized$Stand, 
  levels = c("Low", "Mid", "High")
)
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
#Huge difference!
#Also different when considering only species on the tray. 


#Looking at the mean detection per week for each species and stands
ggplot(predator_density_site_species_standardized, aes(x = Species_ID, y = detections_per_week, fill = Stand)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values= c("Low" = "black", "Mid" = "blue", "High" = "lightblue"))+
  labs(y = "Mean detections per Week", x = "Species", fill= "Stand") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Now looking at it from a donught plot perspective
data_pie <- predator_density_site_species_standardized %>%  
  group_by(Species_ID) %>%
  summarise(total_detections = sum(detections_per_week))

ggplot(data_pie, aes(x = 2, y = total_detections, fill = Species_ID)) +
  geom_col(color = "white") +
  coord_polar(theta = "y", start = 0) +
  xlim (0,3) +
  scale_fill_manual(values = species_colors, drop= FALSE) +
  theme_void() +
  labs(title = "Predator composition (doughnut chart)", fill = "Species") +
  theme(legend.position = "right")


#New plot with differences per site
data_doughnut <- predator_density_site_species_standardized %>%
  filter(Species_ID != "Not identified") %>%  # remove unidentified species
  group_by(Stand, Species_ID) %>%
  # group_by(Species_ID) %>%
  summarise(total_detections = sum(detections_per_week), .groups = "drop")
ggplot(data_doughnut, aes(x = 2, y = total_detections, fill = Species_ID)) +
  geom_col(color = "white") +
  coord_polar(theta = "y", start = 0) +
  xlim(0.5, 2.5) +
  scale_fill_manual(values = species_colors) +
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
  scale_fill_manual(values = species_colors) +
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
  scale_fill_manual(values = species_colors) +
  facet_wrap(~Stand) +
  theme_void() +
  labs(title = "Predator detections per site (doughnut chart)",
       fill = "Species") +
  theme(legend.position = "right")


#New plot 

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

d<-predator_density_site_species_standardized %>%
  mutate(Species_ID= factor(Species_ID, levels= names(species_colors)), Stand= factor(Stand, levels= c("Low", "Mid", "High")))%>%
  group_by(Stand, Species_ID)%>%
  summarise(total=sum(detections_per_week), .groups= "drop")%>%
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
  geom_text(data= rare, aes(x=1.3, y=y, label= label), inherit.aes = FALSE, size= 4.8)+
  coord_polar(theta = "y", start= 0)+
  xlim(0.5, 2.5)+
  scale_fill_manual(values= species_colors, labels= common_names, drop= FALSE)+
  facet_wrap(~Stand, labeller= as_labeller(c(Low = "Low elevation",Mid = "Mid elevation",High = "High elevation" )))+
  labs(fill= NULL)+
  theme_void()+
  theme(legend.text = element_markdown())+
  theme(legend.position = "right", strip.text = element_text(size= 12), text= element_text(family = "serif"), margin = margin(t=8, b=14), plot.margin=margin(10,10,10,10))
dounghnut_plot

ggsave("seed_predator_plot.png", plot= dounghnut_plot,path= "SeedPredationUS/Plots/" )

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

#removal by week
week_removal <- data_cleaned_2 %>%
  group_by(Week)%>%
  mutate(removal_prop = Success / seed_disposed) %>% 
  summarize(mean_removal= mean(removal_prop, na.rm = TRUE))%>%
  arrange(mean_removal)
week_removal

#removal by camera
camera_removal <- data_cleaned_2 %>%
  group_by(Camera)%>%
  mutate(removal_prop = Success / seed_disposed) %>% 
  summarize(mean_removal= mean(removal_prop, na.rm = TRUE))%>%
  arrange(mean_removal)
camera_removal



# Research question 1: Seed predator density analysis ------------------------------------------


predator_data_clean%>%distinct(Stand, Camera)
predator_data_clean%>%distinct(Stand, Week)%>%arrange(Stand,Week)

weeks_sampled<-predator_data_clean %>%
  distinct(Stand, Week)

all_species<- levels(droplevels(predator_data_clean$Species_ID))

weekly <-predator_data_clean %>% 
  count(Stand, Week, Species_ID, name= "n_det") %>%
  right_join(crossing(weeks_sampled, Species_ID= factor(all_species, levels= all_species)),
             by= c("Stand", "Week", "Species_ID"))%>%
  mutate(n_det= replace_na(n_det,0),
         Stand= factor(Stand, levels= c("Low", "Mid", "High")),
         stand_week= interaction(Stand, Week))

weekly$Stand<-relevel(factor(weekly$Stand), ref= "High")

model_predators<-glmmTMB(n_det~Stand+ Species_ID + (1|stand_week), family= nbinom2(), data= weekly)

#Checking for the residuals
model_predators_resid <- simulateResiduals(model_predators)
plot(model_predators_resid)
testOverdispersion(model_predators_resid)#overdispersed so not ideal

model_predators_stand<-glmmTMB(n_det ~Stand + Species_ID + (1|Week) + (1|Species_ID:Stand), 
                               family= nbinom2(), data= weekly)

#checking other models in case : 

model_predators_stand_null<-glmmTMB(n_det ~1, 
                                    family= nbinom2(), data= weekly)
model_predators_stand_simple<-glmmTMB(n_det ~Stand + Species_ID , 
                                      family= nbinom2(), data= weekly)
model_predators_stand_simple1<-glmmTMB(n_det ~Stand + Species_ID + (1|Week) , 
                                       family= nbinom2(), data= weekly)
model_predators_stand_simple2<-glmmTMB(n_det ~Stand + Species_ID + (1|Species_ID:Stand), 
                                       family= nbinom2(), data= weekly)
model_predators_stand2<-glmmTMB(n_det ~Stand + Species_ID + (1|stand_week) + (1|Species_ID:Stand), 
                                family= nbinom2(), data= weekly)

AIC(model_predators_stand_null, model_predators_stand_simple, model_predators_stand_simple1, model_predators_stand_simple2, model_predators_stand, model_predators_stand2) %>%
  arrange(AIC)#best AIC is model_predator_stand_simple2

model_predators_resid2 <- simulateResiduals(model_predators_stand_simple2)
plot(model_predators_resid2)
testOverdispersion(model_predators_resid2)
testZeroInflation(model_predators_resid2)
plotResiduals(model_predators_stand_simple2, form = ~.)

summary(model_predators_stand_simple2)

#testing for species and stand differences
Anova(model_predators_stand_simple2, type= "II")

#testing the random effects
VarCorr(model_predators_stand_simple2)

#saving model RQ1
model_1_table<-as.data.frame(
  summary(model_predators_stand_simple2)$coefficients$cond
)
lo<-model_1_table$Estimate - qnorm(0.975)*model_1_table$`Std. Error`
hi<-model_1_table$Estimate + qnorm(0.975)*model_1_table$`Std. Error`
model_1_table <-data.frame(
  Term= rownames (model_1_table), 
  Estimate= sprintf("%.3f", model_1_table[,"Estimate"]),
  SE= sprintf("%.3f", model_1_table[,"Std. Error"]),
  `95% CI`= sprintf("[%.3f, %.3f]",lo,hi),
  z= sprintf("%.3f", model_1_table[,"z value"]),
  p= ifelse(model_1_table[,"Pr(>|z|)"] <0.001, "<0.001",
      sprintf("%.3f", model_1_table[,"Pr(>|z|)"])),
  row.names = NULL
)
model_1_table



#Checking the estimates
emmeans_predator_model_sp<-emmeans(model_predators_stand_simple2, ~Species_ID, type= "response")
emmeans_sp_model1<-pairs(emmeans_predator_model_sp, adjust= "tukey")
write.csv(emmeans_table_format(emmeans_sp_model1), "SeedPredationUS/Datasets/estimated_probabilities_m1_sp.csv", row.names = FALSE)
contrast(emmeans_predator_model_sp, "trt.vs.ctrl", ref= "Peromyscus maniculatus")

emmeans_predator_model_st<-emmeans(model_predators_stand_simple2, ~Stand + Species_ID, type= "response")
emmeans_st_model1<-pairs(emmeans_predator_model_st, adjust= "tukey")
write.csv(emmeans_table_format(emmeans_st_model1), "SeedPredationUS/Datasets/estimated_probabilities_m1_st.csv")

emmeans_st_model1<-pairs(emmeans_predator_model_st, adjust= "tukey", by= "Species_ID")







# Research question 2: Seed preferences------------------------------------------------------------------
#How limiting is seed predation (as measured by seed removal), and how does this vary among dominant conifer species and across elevations?

data_cleaned_2$Stand<-relevel(factor(data_cleaned_2$Stand), ref= "High")

#First, I defined the most logical model based on my knowledge
model_removal_species_full<- glmmTMB(cbind(Success, seed_disposed - Success)~
                                       Stand + Seed_sp + 
                                       (1 | Camera)  + (1|Week),
                                     family = betabinomial(),   data= data_cleaned_2)


#checking now if it would make sense to have a simpler model based on AIC score
model_removal_species_null<-glmmTMB(cbind(Success, seed_disposed - Success)~1,
                                    family = betabinomial(),   data= data_cleaned_2)

model_removal_species_simple<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand + Seed_sp ,
                                family = betabinomial(), 
                                data= data_cleaned_2)

model_removal_species_simple2<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand + Seed_sp 
                                        + (1|Camera) ,
                                       family = betabinomial(), 
                                       data= data_cleaned_2)

model_removal_species_simple3<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand + Seed_sp 
                                        + (1|Week) ,
                                        family = betabinomial(), 
                                        data= data_cleaned_2)

AIC(model_removal_species_null, model_removal_species_simple, model_removal_species_simple2, model_removal_species_simple3, model_removal_species_full  ) %>%
  arrange(AIC)

data_cleaned_3<-data_cleaned_2%>%
  filter(Week!="Fourth")

Anova(model_removal_species_full, type= "II")

#the best model still remains the full one : 
model_removal_species<- glmmTMB(cbind(Success, seed_disposed - Success)~
                                  Stand + Seed_sp + 
                                  (1 | Camera)  + (1|Week),
                                family = betabinomial(),  data= data_cleaned_2)

#Simulation the residuals to check for distribution of residuals
SimOUtput<-simulateResiduals(model_removal_species)
plot(SimOUtput)
testOverdispersion(SimOUtput)
testZeroInflation(SimOUtput) 

plotResiduals(model_removal_species, form = ~.)

summary(model_removal_species)

model_2_table<-as.data.frame(
  summary(model_removal_species)$coefficients$cond
)
lo<-model_2_table$Estimate - qnorm(0.975)*model_2_table$`Std. Error`
hi<-model_2_table$Estimate + qnorm(0.975)*model_2_table$`Std. Error`
model_2_table <-data.frame(
  Term= rownames (model_2_table), 
  Estimate= sprintf("%.3f", model_2_table[,"Estimate"]),
  SE= sprintf("%.3f", model_2_table[,"Std. Error"]),
  `95% CI`= sprintf("[%.3f, %.3f]",lo,hi),
  z= sprintf("%.3f", model_2_table[,"z value"]),
  p= ifelse(model_2_table[,"Pr(>|z|)"] <0.001, "<0.001",
            sprintf("%.3f", model_2_table[,"Pr(>|z|)"])),
  row.names = NULL
)
model_2_table
#POST-HOC tests
em_stand <- emmeans(model_removal_species, ~ Stand , type = "response")
em_stand_df <- as.data.frame(em_stand)
stand_contrasts <- pairs(em_stand, adjust = "tukey")
write.csv(emmeans_table_format(stand_contrasts), "SeedPredationUS/Datasets/estimated_probabilities_m2_st.csv")
em_seeds <- emmeans(model_removal_species, ~ Seed_sp , type = "response")
em_seeds_df <- as.data.frame(em_seeds)
seed_contrasts <- pairs(em_seeds, adjust = "tukey")
summary(seed_contrasts)
write.csv(emmeans_table_format(seed_contrasts), "SeedPredationUS/Datasets/estimated_probabilities_m2_sp.csv")
em_both <- emmeans(model_removal_species, ~ Stand + Seed_sp, type = "response")
all_contrasts <- pairs(em_both, adjust = "tukey", by= "Seed_sp")
write.csv(emmeans_table_format(all_contrasts), "SeedPredationUS/Datasets/estimated_probabilities_m2.csv")


#PLOT prep
#data
em_df <- as.data.frame(em_both)
em_df <- em_df %>%
  mutate(Stand = factor(Stand, levels = c("Low", "Mid", "High")))
em_df <- em_df %>%
  mutate(Seed_sp = factor(Seed_sp, levels = c("Thuja plicata","Tsuga heterophylla","Pseudotsuga menziesii","Abies amabilis","Callitropsis nootkatensis","Abies lasiocarpa")))
stand_means <- em_df %>% # Compute mean probability per stand (horizontal line across species)
  group_by(Stand) %>%
  summarise(mean_prob = mean(prob, na.rm = TRUE))

#details
dodge_width <- 0.5
species_shapes <- c(16, 17, 15, 18, 8, 3, 4)
species_names <- unique(em_df$Seed_sp)
species_shapes <- setNames(species_shapes[seq_along(species_names)], species_names)

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
    limits = c(0,1) ,
    breaks = seq(0, 1, by = 0.1),
    labels = scales::percent_format(accuracy = 1), 
    expand = c(0,0.01))+
  labs(
    #title = "Estimated Probability of Seed Removal by Elevation and Species",
    x = "Elevation (Stand)",
    y = "Predicted Probability of Seed Removal",
    shape = "Seed Species"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x  = element_text(family = "serif", size = 16, face = "italic",
                                angle = 45, hjust = 1),
    axis.text.y  = element_text(family = "serif", size = 16, face = "bold"),
    axis.title   = element_text(family = "serif", size = 16, face = "bold"),
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 16),
    legend.position = "right")+
  # theme(
  #   panel.grid.major.x = element_blank(),
  #   panel.grid.minor.x = element_blank(),
  #   panel.grid.major.y = element_line(color = "grey80", linewidth = 0.6),
  #   axis.text.x = element_text(family = "Times New Roman",size = 14, face = "bold"),
  #   axis.text.y = element_text(family = "Times New Roman", size = 14, face= "bold"),
  #   axis.title = element_text(family = "Times New Roman",size = 14, face = "bold"),
  #   legend.title = element_text(size = 14),
  #   legend.text = element_text(size = 14),
  #   plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
  #   legend.position = "right"
  # )+
  guides(shape=guide_legend(theme=theme(legend.text = element_text(face="italic", size= 12))))
g1

ggsave("model_rq2.png", plot= g1,path= "SeedPredationUS/Plots/" )





# Research question 3a: Elevation group --------------------------------------------------
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
model_elevation_null <- glmmTMB(
  cbind(Success, seed_disposed - Success) ~ 1 ,
  family = betabinomial,
  data = data_cleaned_2
)

model_elevation_simple <- glmmTMB(
  cbind(Success, seed_disposed - Success) ~ Elevation_group + Stand  ,
  family = betabinomial(),
  data = data_cleaned_2
)

model_elevation_simple_int <- glmmTMB(
  cbind(Success, seed_disposed - Success) ~ Elevation_group * Stand  ,
  family = betabinomial(),
  data = data_cleaned_2
)

model_elevation_simple2 <- glmmTMB(
  cbind(Success, seed_disposed - Success) ~ Elevation_group + Stand +
    (1 | Seed_sp),
  family = betabinomial (),
  data = data_cleaned_2
)

model_elevation_simple3 <- glmmTMB(
  cbind(Success, seed_disposed - Success) ~ Elevation_group + Stand +
    (1 | Seed_sp) + (1|Week),
  family = betabinomial(),
  data = data_cleaned_2
)

model_elevation_simple4 <- glmmTMB(
  cbind(Success, seed_disposed - Success) ~ Elevation_group + Stand +
     (1|Week) + (1|Camera) ,
  family = betabinomial(),
  data = data_cleaned_2
)

model_elevation_simple5 <- glmmTMB(
  cbind(Success, seed_disposed - Success) ~ Elevation_group + Stand +
    (1|Seed_sp) + (1|Camera) ,
  family = betabinomial(),
  data = data_cleaned_2
)

model_elevation_full <- glmmTMB(
  cbind(Success, seed_disposed - Success) ~ Elevation_group + Stand +
    (1 | Seed_sp) + (1|Week) + (1|Camera) ,
  family = betabinomial(),
  data = data_cleaned_2
)

model_elevation_full_int <- glmmTMB(
  cbind(Success, seed_disposed - Success) ~ Elevation_group * Stand +
    (1 | Seed_sp) + (1|Week) + (1|Camera) ,
  family = betabinomial(),
  data = data_cleaned_2
)

AIC(model_elevation_null, model_elevation_simple, model_elevation_simple_int, model_elevation_simple2, model_elevation_simple3, model_elevation_simple4, model_elevation_simple5, model_elevation_full,model_elevation_full_int)%>%
  arrange(AIC)

#best model based on AIC scoring is the full one, but does it fit?
anova(model_elevation_full, model_elevation_full_int)

sim_res <- simulateResiduals(model_elevation_full)
plot(sim_res)#looks good
testOverdispersion(sim_res)
testZeroInflation(sim_res)
plotResiduals(model_elevation_full, form= ~.)

summary(model_elevation_full)
model_3_table<-as.data.frame(
  summary(model_elevation_full)$coefficients$cond
)
Anova(model_elevation_full, type="II")

lo<-model_3_table$Estimate - qnorm(0.975)*model_3_table$`Std. Error`
hi<-model_3_table$Estimate + qnorm(0.975)*model_3_table$`Std. Error`
model_3_table <-data.frame(
  Term= rownames (model_3_table), 
  Estimate= sprintf("%.3f", model_3_table[,"Estimate"]),
  SE= sprintf("%.3f", model_3_table[,"Std. Error"]),
  `95% CI`= sprintf("[%.3f, %.3f]",lo,hi),
  z= sprintf("%.3f", model_3_table[,"z value"]),
  p= ifelse(model_3_table[,"Pr(>|z|)"] <0.001, "<0.001",
            sprintf("%.3f", model_3_table[,"Pr(>|z|)"])),
  row.names = NULL
)
model_3_table

# POST-HOC Test
em3 <- emmeans(model_elevation_full, ~ Elevation_group + Stand, type = "response")
em_m3_elevation_stands<-pairs(em3)
write.csv(emmeans_table_format(em_m3_elevation_stands), "SeedPredationUS/Datasets/estimated_probabilities_m3.csv")
em3_stands <- emmeans(model_elevation_full, ~ Stand, type = "response")
em_m3_elevation_stand<-pairs(em3_stands)
write.csv(emmeans_table_format(em_m3_elevation_stand), "SeedPredationUS/Datasets/estimated_probabilities_m3_st.csv")
em3_elevation<- emmeans(model_elevation_full, ~ Elevation_group, type = "response")
em_m3_elevation<-pairs(em3_elevation)
write.csv(emmeans_table_format(em_m3_elevation), "SeedPredationUS/Datasets/estimated_probabilities_m3_elev.csv")
em3_table<-pairs(em3)
em_df3 <- as.data.frame(em3)

em_df3$Stand <- factor (em_df3$Stand, levels = c("Low", "Mid","High"))

write.csv(emmeans_table_format(em3_table), "SeedPredationUS/Datasets/estimated_probabilities_m3.csv")

# Plot of removal probability by stand and elevation group. 
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
  labs(
    #title = "Predicted Seed Removal Probability by Elevation Group and Stand",
    y = "Predicted Probability of Seed Removal",
    x = "Elevation (Stand)",
    color = "Seed Elevation Group"
  ) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(family = "serif", size = 16, face = "bold",
                                angle = 45, hjust = 1),
    axis.text.y  = element_text(family = "serif", size = 16, face = "bold"),
    axis.title   = element_text(family = "serif", size = 16, face = "bold"),
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 16),
    legend.position = "right"
)
elev_group_plot

ggsave("elev_group_plot.png", plot= elev_group_plot,path= "SeedPredationUS/Plots/" )


# Research question 3b: Treatment ---------------------------------------------------------------
#**Model 4**: Are species more removed when they are around conspecific?

data_cleaned_2$context<- ifelse(data_cleaned_2$Treatment=="All", "Mixed", "Same-range only")
data_cleaned_2$context <-factor(data_cleaned_2$context, levels = c("Same-range only", "Mixed"))


model_removal_treatment_effect_null<- glmmTMB(cbind(Success, seed_disposed - Success)~
                                             1,
                                           family = betabinomial(),   data= data_cleaned_2)

model_removal_treatment_effect_simple<- glmmTMB(cbind(Success, seed_disposed - Success)~
                                             Stand + Seed_sp + context, 
                                             family = betabinomial(),   data= data_cleaned_2)

model_removal_treatment_effect_simple2<- glmmTMB(cbind(Success, seed_disposed - Success)~
                                             Stand + Seed_sp + context +
                                             (1 | Camera) ,
                                           family = betabinomial(),   data= data_cleaned_2)

model_removal_treatment_effect_simple3<- glmmTMB(cbind(Success, seed_disposed - Success)~
                                                   Stand + Seed_sp + context +
                                                   (1 | Week) ,
                                                 family = betabinomial(),   data= data_cleaned_2)

model_removal_treatment_effect<- glmmTMB(cbind(Success, seed_disposed - Success)~
                                           Stand + Seed_sp + context +
                                           (1 | Camera)  + (1|Week),
                                         family = betabinomial(),   data= data_cleaned_2)



model_removal_treatment_effect_simple_int<- glmmTMB(cbind(Success, seed_disposed - Success)~
                                                  Stand + Seed_sp * context, 
                                                family = betabinomial(),   data= data_cleaned_2)

model_removal_treatment_effect_simple2_int<- glmmTMB(cbind(Success, seed_disposed - Success)~
                                                   Stand + Seed_sp * context +
                                                   (1 | Camera) ,
                                                 family = betabinomial(),   data= data_cleaned_2)

model_removal_treatment_effect_simple3_int<- glmmTMB(cbind(Success, seed_disposed - Success)~
                                                   Stand + Seed_sp * context +
                                                   (1 | Week) ,
                                                 family = betabinomial(),   data= data_cleaned_2)

model_removal_treatment_effect_int<- glmmTMB(cbind(Success, seed_disposed - Success)~
                                           Stand + Seed_sp * context +
                                           (1 | Camera)  + (1|Week),
                                         family = betabinomial(),   data= data_cleaned_2)



AIC(model_removal_treatment_effect_null,model_removal_treatment_effect_simple,model_removal_treatment_effect_simple2,model_removal_treatment_effect_simple3,model_removal_treatment_effect, model_removal_treatment_effect_simple_int,model_removal_treatment_effect_simple2_int,model_removal_treatment_effect_simple3_int,model_removal_treatment_effect_int ) %>%
  arrange(AIC)

sim_res_m4 <- simulateResiduals(model_removal_treatment_effect)
plot(sim_res_m4)#looks good
testOverdispersion(sim_res_m4)
testZeroInflation(sim_res_m4)
plotResiduals(model_removal_treatment_effect, form= ~.)

summary(model_removal_treatment_effect)

Anova(model_removal_treatment_effect, type= "II")
Anova(model_removal_treatment_effect_int, type = "II")

model_4_table<-as.data.frame(
  summary(model_removal_treatment_effect)$coefficients$cond
)
lo<-model_4_table$Estimate - qnorm(0.975)*model_4_table$`Std. Error`
hi<-model_4_table$Estimate + qnorm(0.975)*model_4_table$`Std. Error`
model_4_table <-data.frame(
  Term= rownames (model_4_table), 
  Estimate= sprintf("%.3f", model_4_table[,"Estimate"]),
  SE= sprintf("%.3f", model_4_table[,"Std. Error"]),
  `95% CI`= sprintf("[%.3f, %.3f]",lo,hi),
  z= sprintf("%.3f", model_4_table[,"z value"]),
  p= ifelse(model_4_table[,"Pr(>|z|)"] <0.001, "<0.001",
            sprintf("%.3f", model_4_table[,"Pr(>|z|)"])),
  row.names = NULL
)
model_4_table

em4 <- emmeans(model_removal_treatment_effect, ~ Stand + Seed_sp + context, type = "response")
em_df4 <- as.data.frame(em4)



# POST-HOC Test
em4 <- emmeans(model_removal_treatment_effect, ~ Stand + Seed_sp + context, type = "response")
em4_table<-pairs(em4)
write.csv(emmeans_table_format(em4_table), "SeedPredationUS/Datasets/estimated_probabilities_m4.csv")

em4_table<-pairs(em4, by = c("context", "Seed_sp"))

em4_st <- emmeans(model_removal_treatment_effect, ~ Stand, type = "response")
em4_table_st<-pairs(em4_st)
write.csv(emmeans_table_format(em4_table_st), "SeedPredationUS/Datasets/estimated_probabilities_m4_st.csv")

em4_sp <- emmeans(model_removal_treatment_effect_int, ~ context|Seed_sp, type = "response")
em4_table_sp<-pairs(em4_sp)

write.csv(emmeans_table_format(em4_table_sp), "SeedPredationUS/Datasets/estimated_probabilities_m4_sp.csv")

em4_cont<- emmeans(model_removal_treatment_effect, ~ context, type = "response")
em4_table_cont<-pairs(em4_cont)
write.csv(emmeans_table_format(em4_table_cont), "SeedPredationUS/Datasets/estimated_probabilities_m4_cont.csv")


em4_summary <- emmeans(model_removal_treatment_effect, ~ Stand +Seed_sp + context, type = "response")
em4_summary_table<-pairs(em4_summary, by = "Stand")
write.csv(emmeans_table_format(em4_summary_table), "SeedPredationUS/Datasets/estimated_probabilities_m4_final.csv")

em4_summary2 <- emmeans(model_removal_treatment_effect, ~  Seed_sp + context, type = "response")
em4_summary_table2<-pairs(em4_summary2, by = "Seed_sp")
write.csv(emmeans_table_format(em4_summary_table2), "SeedPredationUS/Datasets/estimated_probabilities_m4_final2.csv")

em_df4 <- as.data.frame(em4)

em_df4$Stand <- factor (em_df4$Stand, levels = c("Low", "Mid","High"))

em_m4_plot<-as.data.frame((emmeans(model_removal_treatment_effect, ~context |Seed_sp, type= "response")))

em_m4_plot <- em_m4_plot %>%
  mutate(Seed_sp = factor(Seed_sp, levels = c("Thuja plicata","Tsuga heterophylla","Pseudotsuga menziesii","Abies amabilis","Callitropsis nootkatensis","Abies lasiocarpa")))


m4_plot<-ggplot(em_m4_plot, aes(Seed_sp, prob, shape = context, colour = context, fill = context)) +
  geom_pointrange(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                  position = position_dodge(0.5), size = 0.8, linewidth = 0.8) +
  scale_shape_manual(values = c("Same-range only" = 21, "Mixed" = 23)) +
  scale_colour_manual(values = c("Same-range only" = "black", "Mixed" = "black")) +
  scale_fill_manual(values = c("Same-range only" = "white", "Mixed" = "black")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1),
                     labels = scales::percent_format(accuracy = 1),
                     expand = c(0, 0.01)) +
  labs(x = "Seed species",
       y = "Predicted Probability of Seed Removal",
       shape = "Context", colour = "Context", fill = "Context") +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(family = "serif", size = 16, face = "bold.italic",
                                angle = 45, hjust = 1),
    axis.text.y  = element_text(family = "serif", size = 16, face = "bold"),
    axis.title   = element_text(family = "serif", size = 16, face = "bold"),
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 16),
    legend.position = "right"
  )
m4_plot
ggsave("m4_plot.png", plot= m4_plot,path= "SeedPredationUS/Plots/" )

# Additional analysis: 30 min. intervals -----------------------------------------------------

#1. decide if to use this one only, and check if it changes the dounghnut plot

# additional model
# every 30 min detections
pdat<-predator_data_clean%>%
  mutate(tod_sec= as.numeric(difftime(Time, floor_date(Time, "day"), units= "secs")),
         datetime= Date + seconds(tod_sec), 
         cam_id = paste(Stand, Camera, sep= "_"))

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
    right_join(crossing(weeks_sampled, Species_ID= factor(all_species, levels= all_species)), 
               by= c("Stand", "Week", "Species_ID"))%>%
    mutate(counts = replace_na(counts, 0)) %>%
    group_by(Stand, Species_ID) %>%
    summarise(total = sum(counts), weeks_detected = sum(counts > 0), weeks_sampled = n(),
              mean_per_week = mean(counts), se_per_week = sd(counts) / sqrt(n()), .groups = "drop") %>%
    group_by(Stand) %>%
    mutate(pct_of_stand = 100 * total / sum(total)) %>%
    ungroup() %>%
    mutate(Stand = factor(Stand, levels = c("Low", "Mid", "High"))) %>%
    arrange(Stand, desc(total))
}

table_raw    <- summary_tables_predator_overall(pdat)
table_raw%>%
  group_by(Species_ID)%>%
  summarise(mean(total))
table_events <- summary_tables_predator_overall(events30)

# side-by-side comparison
table_both_normal_30 <- full_join(table_raw, table_events,
                                  by = c("Stand", "Species_ID", "weeks_sampled"),
                                  suffix = c("_images", "_events"))
write.csv(table_both_normal_30, "SeedPredationUS/Datasets/table_both_normal_30.csv")



#creating a model with only the events with 30 min intervals
weekly30 <-events30 %>% 
  count(Stand, Week, Species_ID, name= "n_det") %>%
  right_join(crossing(weeks_sampled, Species_ID= factor(all_species, levels= all_species)),
             by= c("Stand", "Week", "Species_ID"))%>%
  mutate(n_det= replace_na(n_det,0),
         Stand= factor(Stand, levels= c("Low", "Mid", "High")),
         stand_week= interaction(Stand, Week))

weekly30$Stand<-relevel(factor(weekly30$Stand), ref= "High")

model_predators_stand_30<-glmmTMB(n_det ~Stand + Species_ID + (1|Week) + (1|Species_ID:Stand), 
                                  family= nbinom2(), data= weekly30)

#testing other models
#checking other models in case : 
model_predators_stand2_30<-glmmTMB(n_det ~Stand + Species_ID + (1|stand_week) + (1|Species_ID:Stand), 
                                   family= nbinom2(), data= weekly30)
model_predators_stand_null_30<-glmmTMB(n_det ~1, 
                                       family= nbinom2(), data= weekly30)
model_predators_stand_simple_30<-glmmTMB(n_det ~Stand + Species_ID , 
                                         family= nbinom2(), data= weekly30)
model_predators_stand_simple1_30<-glmmTMB(n_det ~Stand + Species_ID + (1|Week) , 
                                          family= nbinom2(), data= weekly30)
model_predators_stand_simple2_30<-glmmTMB(n_det ~Stand + Species_ID + (1|Species_ID:Stand), 
                                          family= nbinom2(), data= weekly30)
model_predators_stand2_30<-glmmTMB(n_det ~Stand + Species_ID + (1|stand_week) + (1|Species_ID:Stand), 
                                   family= nbinom2(), data= weekly30)

AIC(model_predators_stand_null_30, model_predators_stand_simple_30, model_predators_stand_simple1_30, model_predators_stand_simple2_30, model_predators_stand_30, model_predators_stand2_30) %>%
  arrange(AIC)#best AIC is model_predators_stand2_30

model_predators_resid30_2 <- simulateResiduals(model_predators_stand2_30)
plot(model_predators_resid30_2)
testOverdispersion(model_predators_resid30_2)
testZeroInflation(model_predators_resid30_2)
plotResiduals(model_predators_stand2_30, form = ~.)

summary(model_predators_stand2_30)

#testing for species and stand differences
Anova(model_predators_stand2_30, type= "II")
#testing the random effects
VarCorr(model_predators_stand2_30)

#saving model RQ1
model_1_table_30<-as.data.frame(
  summary(model_predators_stand2_30)$coefficients$cond
)
lo<-model_1_table_30$Estimate - qnorm(0.975)*model_1_table_30$`Std. Error`
hi<-model_1_table_30$Estimate + qnorm(0.975)*model_1_table_30$`Std. Error`
model_1_table_30 <-data.frame(
  Term= rownames (model_1_table_30), 
  Estimate= sprintf("%.3f", model_1_table_30[,"Estimate"]),
  SE= sprintf("%.3f", model_1_table_30[,"Std. Error"]),
  `95% CI`= sprintf("[%.3f, %.3f]",lo,hi),
  z= sprintf("%.3f", model_1_table_30[,"z value"]),
  p= ifelse(model_1_table_30[,"Pr(>|z|)"] <0.001, "<0.001",
            sprintf("%.3f", model_1_table_30[,"Pr(>|z|)"])),
  row.names = NULL
)
model_1_table_30

#Checking the estimates
emmeans_predator_model_sp_30<-emmeans(model_predators_stand2_30, ~Species_ID, type= "response")
emmeans_sp_model1_30<-pairs(emmeans_predator_model_sp_30, adjust= "tukey")
write.csv(emmeans_table_format(emmeans_sp_model1_30), "SeedPredationUS/Datasets/estimated_probabilities_m1_sp_30.csv")
contrast(emmeans_predator_model_sp_30, "trt.vs.ctrl", ref= "Peromyscus maniculatus")

emmeans_predator_model_st_30<-emmeans(model_predators_stand2_30, ~Stand + Stand, type= "response")
emmeans_st_model1_30<-pairs(emmeans_predator_model_st_30, adjust= "tukey")
write.csv(emmeans_table_format(emmeans_st_model1_30), "SeedPredationUS/Datasets/estimated_probabilities_m1_st_30.csv")

# Additional analysis: SAC and Chao ------------------------------------------------------------


#Other things to test 

#Species accumulation curve
spp_curve<-setdiff(all_species, "Not identified")

wide<-weekly %>%
  select(Stand, Week, Species_ID, n_det)%>%
  pivot_wider(names_from= Species_ID, values_from= "n_det")%>%
  arrange(Stand, Week)

community <- as.data.frame(wide[,spp_curve])

# set.seed(123)
# acc<-specaccum(community, method= "random", permutations= 1000)
# acc_df<-data.frame(samples= acc$sites, richness= acc$richness, sd= acc$sd)
# now<-acc_df[nrow(acc_df),]
# ggplot(acc_df, aes(samples, richness))+
#   geom_ribbon(aes(ymin= richness -1.96*sd, ymax= richness +1.96*sd), alpha= 0.2)+
#   geom_line(linewidth= 1)+
#   geom_point(data=now, size= 3)+
#   annotate("text", x= now$samples, y= now$richness, label= paste0("Current:", now$samples, "stand-weeks,",round(now$richness),"taxa"), hjust=1.05, vjust=-1)+
#   labs(x= "Number of stand-weeks samples", y= "cumulative number of taxa")+
#   theme_minimal()

#Chao index
specpool(community)
specpool(community, pool= wide$Stand)

# Additional analysis: Treatment ALL only for seed removal rates ----------
#Checking how much the data changes when just plotting the "All" treatment ONLY

#Checking models
model_removal_species_all_null<-glmmTMB(cbind(Success, seed_disposed - Success)~1,
                                        family = betabinomial(),   data= data_cleaned_2[data_cleaned_2$Treatment=="All",])

model_removal_species_all_simple<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand + Seed_sp ,
                                           family = betabinomial(), 
                                           data= data_cleaned_2[data_cleaned_2$Treatment=="All",])

model_removal_species_all_simple2<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand + Seed_sp 
                                            + (1|Camera) ,
                                            family = betabinomial(), 
                                            data= data_cleaned_2[data_cleaned_2$Treatment=="All",])

model_removal_species_all_simple3<- glmmTMB(cbind(Success, seed_disposed - Success)~Stand + Seed_sp 
                                            + (1|Week) ,
                                            family = betabinomial(), 
                                            data= data_cleaned_2[data_cleaned_2$Treatment=="All",])

model_removal_species_all_full<- glmmTMB(cbind(Success, seed_disposed - Success)~
                                           Stand + Seed_sp + 
                                           (1 | Camera)  + (1|Week),
                                         family = betabinomial(),   data= data_cleaned_2[data_cleaned_2$Treatment=="All",])

AIC(model_removal_species_all_null, model_removal_species_all_simple, model_removal_species_all_simple2, model_removal_species_all_simple3, model_removal_species_all_full ) %>%
  arrange(AIC)

#best model is full model but not the best when looking at dharma plot, but then different than the original one (above). What to do?

#Simulation the residuals to check for distribution of residuals
SimOUtput2<-simulateResiduals(model_removal_species_all_simple2)
plot(SimOUtput2) #but not great
testOverdispersion(SimOUtput2)
testZeroInflation(SimOUtput2) 
plotResiduals(model_removal_species_all_full, form = ~.) 

summary(model_removal_species_all_full)

model_2all_table<-as.data.frame(
  summary(model_removal_species_all_full)$coefficients$cond
)
lo<-model_2all_table$Estimate - qnorm(0.975)*model_2all_table$`Std. Error`
hi<-model_2all_table$Estimate + qnorm(0.975)*model_2all_table$`Std. Error`
model_2all_table <-data.frame(
  Term= rownames (model_2all_table), 
  Estimate= sprintf("%.3f", model_2all_table[,"Estimate"]),
  SE= sprintf("%.3f", model_2all_table[,"Std. Error"]),
  `95% CI`= sprintf("[%.3f, %.3f]",lo,hi),
  z= sprintf("%.3f", model_2all_table[,"z value"]),
  p= ifelse(model_2all_table[,"Pr(>|z|)"] <0.001, "<0.001",
            sprintf("%.3f", model_2all_table[,"Pr(>|z|)"])),
  row.names = NULL
)
model_2all_table

#POST-HOC tests
em_stand_alltreat <- emmeans(model_removal_species_all_full, ~ Stand , type = "response")
em_stand_df_alltreat <- as.data.frame(em_stand_alltreat)
stand_contrasts_alltreat <- pairs(em_stand_alltreat, adjust = "tukey")
write.csv(emmeans_table_format(stand_contrasts_alltreat), "SeedPredationUS/Datasets/estimated_probabilities_m2_st_alltreat.csv")
em_seeds_alltreat <- emmeans(model_removal_species_all_simple2, ~ Seed_sp , type = "response")
em_seeds_df_alltreat <- as.data.frame(em_seeds_alltreat)
seed_contrasts_alltreat <- pairs(em_seeds_alltreat, adjust = "tukey")
write.csv(emmeans_table_format(seed_contrasts_alltreat), "SeedPredationUS/Datasets/estimated_probabilities_m2_sp_alltreat.csv")
em_both_alltreat <- emmeans(model_removal_species_all_simple2, ~ Stand + Seed_sp, type = "response")
all_contrasts_alltreat <- pairs(em_both_alltreat, adjust = "tukey")
write.csv(emmeans_table_format(all_contrasts_alltreat), "SeedPredationUS/Datasets/estimated_probabilities_m2_alltreat.csv")

#Prepare plot
em_df_alltreat <- as.data.frame(em_both_alltreat)
em_df_alltreat <- em_df_alltreat %>%
  mutate(Stand = factor(Stand, levels = c("Low", "Mid", "High")))
em_df_alltreat <- em_df_alltreat %>%
  mutate(Seed_sp = factor(Seed_sp, levels = c("Thuja plicata","Tsuga heterophylla","Pseudotsuga menziesii","Abies amabilis","Callitropsis nootkatensis","Abies lasiocarpa")))
stand_means_alltreat <- em_df_alltreat %>% # Compute mean probability per stand (horizontal line across species)
  group_by(Stand) %>%
  summarise(mean_prob = mean(prob, na.rm = TRUE))

#Plotting both models together to see how different they are
em_df_plot<-em_df%>%
  mutate(Model="Original")
em_df_treament<-em_df_alltreat%>%
  mutate(Model="Treatment All")
em_combined<-bind_rows(em_df_plot, em_df_treament)

stand_means_both<-em_combined %>%
  group_by(Model,Stand)%>%
  summarise(mean_prob= mean(prob, na.rm= TRUE), .groups= "drop")%>%
  arrange(Model, mean_prob)
stand_means_both

gcomparison<-ggplot(em_combined, aes(x = Stand, y = prob, shape = Seed_sp, color= Model)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0, dodge.width = 0.6),
              size = 5, alpha = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.6), width = 0.2) +
  # Add mean line per stand
  geom_segment(data = stand_means_both, aes(x = as.numeric(Stand) -0.4, xend= as.numeric(Stand) +0.4, y = mean_prob, yend = mean_prob, color= Model),linewidth= 1.2, alpha = 0.8, inherit.aes = FALSE) +
  scale_color_manual(values = c("Original" = "black", "Treatment All" = "blue"))+
  scale_shape_manual(values = species_shapes) +
  scale_y_continuous(
    limits = c(0,1) ,
    breaks = seq(0, 1, by = 0.1),
    labels = scales::percent_format(accuracy = 1), 
    expand = c(0,0.01))+
  labs(
    #title = "Estimated Probability of Seed Removal by Elevation and Species",
    x = "Elevation (Stand)",
    y = "Predicted Probability of Seed Removal",
    shape = "Seed Species",
    color= "Model"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x  = element_text(family = "serif", size = 16, face = "italic",
                                angle = 45, hjust = 1),
    axis.text.y  = element_text(family = "serif", size = 16, face = "bold"),
    axis.title   = element_text(family = "serif", size = 16, face = "bold"),
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 16),
    legend.position = "right")+
  # theme(
  #   panel.grid.major.x = element_blank(),
  #   panel.grid.minor.x = element_blank(),
  #   panel.grid.major.y = element_line(color = "grey80", linewidth = 0.6),
  #   axis.text.x = element_text(family = "Times New Roman",size = 14, face = "bold"),
  #   axis.text.y = element_text(family = "Times New Roman", size = 14, face= "bold"),
  #   axis.title = element_text(family = "Times New Roman",size = 14, face = "bold"),
  #   legend.title = element_text(size = 14),
  #   legend.text = element_text(size = 14),
  #   plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
  #   legend.position = "right"
  # )+
  guides(shape=guide_legend(theme=theme(legend.text = element_text(face="italic", size= 16))))
gcomparison

ggsave("model_comparison.png", plot= gcomparison,path= "SeedPredationUS/Plots/" )


# Additional analysis: Seed removal by weight --------------------------------------------------


# Research question 2: Seed preferences by weight

removal_weight<-data_cleaned_2%>%
  group_by(Seed_sp)%>%
  summarise(seeds_offered = sum (seed_disposed),
            seeds_removed= sum(Success),
            g_offered = sum (mass_placed),
            g_removed= sum(mass_removed),
            g_pct_removed= 100* g_removed/g_offered, .groups = "drop")%>%
  arrange(g_pct_removed)
write.csv(removal_weight, "SeedPredationUS/Datasets/removal_weight.csv")

selection_ratio<-data_cleaned_2%>%
  filter(Treatment=="All") %>%
  group_by(Stand, Seed_sp)%>%
  summarise(g_off= sum(mass_placed), g_rem= sum(mass_removed), .groups = "drop")%>%
  group_by(Stand)%>%
  mutate(share_offered= g_off/sum(g_off),
         share_eaten = g_rem/ sum(g_rem),
         selection= share_eaten/share_offered)%>%
  arrange(selection)
selection_ratio

selection_ratio_both<-data_cleaned_2%>%
  filter(Treatment=="All") %>%
  group_by(Stand, Seed_sp)%>%
  summarise(n_off= sum(seed_disposed), n_rem= sum(seed_removed),g_off= sum(mass_placed), g_rem= sum(mass_removed), .groups = "drop")%>%
  group_by(Stand)%>%
  mutate(share_offered_g= g_off/sum(g_off),
         share_eaten_g = g_rem/ sum(g_rem),
         mass= share_eaten_g/share_offered_g,
         share_offered_n= n_off/sum(n_off),
         share_eaten_n = n_rem/ sum(n_rem),
         number= share_eaten_n/share_offered_n)%>%
  arrange(mass)
selection_ratio_both

selection_ratio_both <- selection_ratio_both %>%
  mutate(across(where(is.numeric), ~ sprintf("%.3f", .x)))

write.csv(selection_ratio_both, "SeedPredationUS/Datasets/selection_ratio_both.csv")

