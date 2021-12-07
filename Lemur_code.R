

############# Set up #################

# linear mixed-effects models of germination and growth parameters

# We fitted linear mixed-effects models in R 3.6.1 (R Core Team 2018) with the packages “lme4” 
# and “lmerTest”  to investigate how each parameter differed between defecated and control seeds


library(lme4)
library(ggplot2)
library(ggfortify)
library(Hmisc)
library(ggpubr)
library(MuMIn)
library(tidyverse)
library(survival)
library(knitr)

data <- read.csv("mouse_lemur_data.csv", header = TRUE)
data$species_number <- as.numeric(as.factor(data$scientificName))
data$seedling_mm <- as.numeric(data$seedling_length_mm)
data$germ_time <- as.numeric(data$germination_time)
data$ratio <- as.numeric( factor(data$germination_state) ) - 1


########################################################################
#
#
#
#
#
#
##################### Subsetting data #######################
#
#
#
#
#
#
#########################################################################




# Subsetting the data to only include seeds dispersed by microcebus rufus
rufus <- subset(data, disperser == "microcebus_rufus")
rufus_petri <- subset(rufus, experiment == "Petri dish")
nrow(rufus_petri)
# 685
rufus_petri_yes <- subset(rufus_petri, germination_state == "yes")
nrow(rufus_petri_yes)
# 121
# This creates summary statistics for seedling growth for each species
rufus_petri_yes_summary <- rufus_petri_yes %>% 
  group_by(scientificName, treatment) %>%
  summarise(seedling_mean = mean(seedling_mm), germ_time = mean(germ_time),
            N = n())
####  This line modifies the summary statistics to include only the species for which there is a defecated and control  
rufus_petri_yes_summary_modified <- rufus_petri_yes_summary[-c(3, 4, 5, 6, 7, 12), ]
rufus_petri_yes_summary_modified
sum(rufus_petri_yes_summary_modified$N)
# 88

# Number of total observations in experiment is 685
# Number of observations germinated is 121
# Number of observations after only including species 
# with defecated and control seeds is 88
# Paper states 150 is total sample size
# Paper states 88 are actually analyzed

# The researchers seemed to have used the final summary (n=88) for
# the number of observations actually analyzed (n=88)

# I do not know where the number 150 as the total number of observations comes from

rufus_ground <- subset(rufus, experiment == "Forest ground")
nrow(rufus_ground)
# 231
rufus_ground_yes <- subset(rufus_ground, germination_state == "yes")
nrow(rufus_ground_yes)
# 7
# This experiment does not need a filtering for only species with defecated 
# and control seeds because they are all the same species

# This might useful later though

rufus_ground_yes_summary <- rufus_ground_yes %>% 
  group_by(scientificName, treatment) %>%
  summarise(seedling_mean = mean(seedling_mm), germ_time = mean(germ_time),
            N = n())

# Number of total observations in experiment is 231
# Number of observations germinated is 7
# Number of observations after only including species 
# with defecated and control seeds is 7 (they are all the same species)
# Paper states 75 is total sample size
# Paper states 7 are actually analyzed

# The researchers seemed to have used the final summary (n=7) for
# the number of observations actually analyzed (n=7)

# I do not know where the number 75 as the total number of observations comes from

# Subsetting the data to only include seeds dispersed by microcebus jollyae
jollyae <- subset(data, disperser == "microcebus_jollyae")
# Subsetting the jollyae data to only include seeds in the petri dish experiment
jollyae_petri <- subset(jollyae, experiment == "Petri dish")
nrow(jollyae_petri)
# 528
jollyae_petri_yes <- subset(jollyae_petri, germination_state == "yes")
nrow(jollyae_petri_yes)
# 70
# This creates summary statistics for seedling growth for each species
jollyae_petri_yes_summary <- jollyae_petri_yes %>% 
  group_by(scientificName, treatment) %>%
  summarise(seedling_mean = mean(seedling_mm), germ_time = mean(germ_time),
            N = n())
####  This line modifies the summary statistics to include only the species for which there is a defecated and control  
jollyae_petri_yes_summary_modified <- jollyae_petri_yes_summary[-c(1, 2, 3), ]
jollyae_petri_yes_summary_modified
sum(jollyae_petri_yes_summary_modified$N)
# 25

# Number of total observations in experiment is 528
# Number of observations germinated is 70
# Number of observations after only including species 
# with defecated and control seeds is 25
# Paper states 528 is total sample size 
# Paper states 70 are actually analyzed

# The researchers seemed to have used all of the observations germinated (n=70) for
# the number of observations actually analyzed (70)

# Subsetting the jollyae data to only include seeds in the shaded plot experiment
jollyae_closed <- subset(jollyae, experiment == "Closed")
nrow(jollyae_closed)
# 694
jollyae_closed_yes <- subset(jollyae_closed, germination_state == "yes")
nrow(jollyae_closed_yes)
# 61
# This creates summary statistics for seedling growth for each species
jollyae_closed_yes_summary <- jollyae_closed_yes %>% 
  group_by(scientificName, treatment) %>%
  summarise(seedling_mean = mean(seedling_mm), germ_time = mean(germ_time),
            N = n())
####  This line modifies the summary statistics to include only the species for which there is a defecated and control  
jollyae_closed_yes_summary_modified <- jollyae_closed_yes_summary[-c(5, 6), ]
jollyae_closed_yes_summary_modified
sum(jollyae_closed_yes_summary_modified$N)
#43

# Number of total observations in experiment is 694
# Number of observations germinated is 61
# Number of observations after only including species 
# with defecated and control seeds is 43
# Paper states 377 is total sample size 
# Paper states 47 are actually analyzed

# The researchers seemed to have added the 4 seeds from	voampoalahy from the final summary (n=43)
# to make the number of observations analyzed 47

# I do not know where the number 377 as the total observations in the experiment comes from

# Subsetting the jollyae data to only include seeds in the semi-shaded plot experiment
jollyae_semi <- subset(jollyae, experiment == "Semi-closed")
nrow(jollyae_semi)
# 660
jollyae_semi_yes <- subset(jollyae_semi, germination_state == "yes")
nrow(jollyae_semi_yes)
# 233
# This creates summary statistics for seedling growth for each species
jollyae_semi_yes_summary <- jollyae_semi_yes %>% 
  group_by(scientificName, treatment) %>%
  summarise(seedling_mean = mean(seedling_mm), germ_time = mean(germ_time),
            N = n())
####  This line modifies the summary statistics to include only the species for which there is a defecated and control  
jollyae_semi_yes_summary_modified <- jollyae_semi_yes_summary
jollyae_semi_yes_summary_modified
sum(jollyae_semi_yes_summary_modified$N)

# Number of total observations in experiment is 660
# Number of observations germinated is 233
# Number of observations after only including species 
# with defecated and control seeds is 233
# Paper states 660 is total sample size 
# Paper states 233 are actually analyzed

# The researchers seemed to have used the final summary (n=233) for
# the number of observations actually analyzed (n=233). There were no single 
# experiment species so there are no differences between the number germinated 
# and the final summary


########################################################################
#
#
#
#
#
#
##################### Analysis 1: mixed effect models #######################
#
#
#
#
#
#
#########################################################################

# I am starting with the mixed effect model comparing seedling length across the 
# fixed effect of treatment and the random effect of species

# Running the mixed effect model for rufus dispersed seeds in the petri dish experiment

# The study did not tell me wether the random effects of the model were slope, 
# intercept or slope and intercept. I tried all three for the first experiment. 

# REML = FALSE, intercept model
lme_rufus_petri_yes_summary_modified1 <- lmer(data = rufus_petri_yes_summary_modified, seedling_mean ~ treatment + (1 | scientificName), REML = FALSE)
summary(lme_rufus_petri_yes_summary_modified1)

# β = 13.549
# z = 
# p < 
# R2M = 
# R2C = 

# REML = FALSE , slope model
lme_rufus_petri_yes_summary_modified2 <- lmer(data = rufus_petri_yes_summary_modified, seedling_mean ~ treatment + (1 + treatment | scientificName), REML = FALSE)
summary(lme_rufus_petri_yes_summary_modified2)
# will not run 

# Error: number of observations (=8) <= number of random effects (=8) 
# for term (1 + treatment | scientificName); the random-effects parameters 
# and the residual variance (or scale parameter) are probably unidentifiable

# REML = FALSE , slope and intercept model
lme_rufus_petri_yes_summary_modified3 <- lmer(data = rufus_petri_yes_summary_modified, seedling_mean ~ treatment + (1 | scientificName) + (1 + treatment | scientificName), REML = FALSE)
summary(lme_rufus_petri_yes_summary_modified3)
# will not run

# Error: number of observations (=8) <= number of random effects (=8) 
# for term (1 + treatment | scientificName); the random-effects parameters 
# and the residual variance (or scale parameter) are probably unidentifiable

# Based on some research, the error is telling me that I do not have a sufficient 
# number of observations to support a model which is this complex. I will only use intercept 
# models for the rest of the analysis. 

# After we have determined the model we will use, we can get the other parameters by running statitical test

null1 <- lmer(data = rufus_petri_yes_summary_modified, seedling_mean ~ 1 + (1 | scientificName), REML = FALSE)
summary(null1)

anova(null1, lme_rufus_petri_yes_summary_modified1, test = "Chisq")
r.squaredGLMM(lme_rufus_petri_yes_summary_modified1)

# β = 13.549
# z = 3.18796
# p = 0.0742
# R2M = 0.2205152
# R2C = 0.6833415

# Now I will run the rufus dispered forest ground experiment

lme_rufus_ground_yes_summary_modified1 <- lmer(data = rufus_ground_yes_summary_modified, seedling_mean ~ treatment + (1 | scientificName), REML = FALSE)
summary(lme_rufus_ground_yes_summary_modified1)

# Impossible to do analysis with no more than one treatment 

# Now I will do the analysis for the the experiment from jollyae dispersed seeds in the petri dish

lme_jollyae_petri_yes_summary_modified1 <- lmer(data = jollyae_petri_yes_summary_modified, seedling_mean ~ treatment + (1 | scientificName), REML = FALSE)
summary(lme_jollyae_petri_yes_summary_modified1)

# seedling length = 0, cannot compare between treatments

# Now I will do the analysis for the the experiment from jollyae dispersed seeds in the semi-shaded plot experiment

lme_jollyae_semi_yes_summary_modified1 <- lmer(data = jollyae_semi_yes_summary_modified, seedling_mean ~ treatment + (1 | scientificName), REML = FALSE)
summary(lme_jollyae_semi_yes_summary_modified1)

null2 <- lmer(data = jollyae_semi_yes_summary_modified, seedling_mean ~ 1 + (1 | scientificName), REML = FALSE)
summary(null2)

anova(null2, lme_jollyae_semi_yes_summary_modified1, test = "Chisq")
r.squaredGLMM(lme_jollyae_semi_yes_summary_modified1)

# β = 1.774
# z = 0.7167
# p = 0.3972 
# R2M = 0.04079902
# R2C = 0.636132

lme_jollyae_closed_yes_summary_modified1 <- lmer(data = jollyae_closed_yes_summary_modified, seedling_mean ~ treatment + (1 | scientificName), REML = FALSE)
summary(lme_jollyae_closed_yes_summary_modified1)

null3 <- lmer(data = jollyae_closed_yes_summary_modified, seedling_mean ~ 1 + (1 | scientificName), REML = FALSE)
summary(null3)

anova(null3, lme_jollyae_closed_yes_summary_modified1, test = "Chisq")
r.squaredGLMM(lme_jollyae_closed_yes_summary_modified1)


# β = 7.300
# z = 6.1518
# p = 0.01313
# R2M = 0.8297392
# R2C = 0.8297654

#
#
#
#
# I will now code for the mixed effect model comparing germination time across the 
# fixed effect of treatment and the random effect of species
#
#
#
#

# Running the mixed effect model for rufus dispersed seeds in the petri dish experiment

lme_rufus_petri_yes_summary_modified1_germ_time <- lmer(data = rufus_petri_yes_summary_modified, germ_time ~ treatment + (1 | scientificName), REML = FALSE)
summary(lme_rufus_petri_yes_summary_modified1_germ_time)

null4 <- lmer(data = rufus_petri_yes_summary_modified, germ_time ~ 1 + (1 | scientificName), REML = FALSE)
summary(null4)

anova(null4, lme_rufus_petri_yes_summary_modified1_germ_time, test = "Chisq")
r.squaredGLMM(lme_rufus_petri_yes_summary_modified1_germ_time)


# β = -15.97
# z = 0.9894
# p = 0.3199
# R2M = 0.1307745
# R2C = 0.1307745

# Not doing the analysis for Microcebus rufus forest ground experiment (see above)

# Running the mixed effect model for jollyae dispersed seeds in the petri dish experiment

lme_jollyae_petri_yes_summary_modified1_germ_time <- lmer(data = jollyae_petri_yes_summary_modified, germ_time ~ treatment + (1 | scientificName), REML = FALSE)
summary(lme_jollyae_petri_yes_summary_modified1_germ_time)

# Error: grouping factors must have > 1 sampled level

# So since it is the mean germination time, the comparison only has two points. 
# I cannot run this analysis

# Running the mixed effect model for jollyae dispersed seeds in the semi-shaded plot experiment

lme_jollyae_semi_yes_summary_modified1_germ_time <- lmer(data = jollyae_semi_yes_summary_modified, germ_time ~ treatment + (1 | scientificName), REML = FALSE)
summary(lme_jollyae_semi_yes_summary_modified1_germ_time)

null5 <- lmer(data = jollyae_semi_yes_summary_modified, germ_time ~ 1 + (1 | scientificName), REML = FALSE)
summary(null5)

anova(null5, lme_jollyae_semi_yes_summary_modified1_germ_time, test = "Chisq")
r.squaredGLMM(lme_jollyae_semi_yes_summary_modified1_germ_time)


# β = -5.183 
# z = 0.4874
# p = 0.4851
# R2M = 0.04586151
# R2C = 0.3807093

lme_jollyae_closed_yes_summary_modified1_germ_time <- lmer(data = jollyae_closed_yes_summary_modified, germ_time ~ treatment + (1 | scientificName), REML = FALSE)
summary(lme_jollyae_closed_yes_summary_modified1_germ_time)

null6 <- lmer(data = jollyae_closed_yes_summary_modified, germ_time ~ 1 + (1 | scientificName), REML = FALSE)
summary(null6)

anova(null6, lme_jollyae_closed_yes_summary_modified1_germ_time, test = "Chisq")
r.squaredGLMM(lme_jollyae_closed_yes_summary_modified1_germ_time)


# β = 0.6833
# z = 0.01
# p = 0.923
# R2M = 0.003328367
# R2C = 0.003328367

#
#
#
#
# I will now code for the mixed effect model comparing Germination ratio across the 
# fixed effect of treatment and the random effect of species
#
#
#
#

# First I will run the model for seeds dispersed by Microcebus rufus in the petri dish experiment
# This code calculates the ratios of germinated seeds for all the indivdual species

rufus_petri_ratio_summary <- rufus_petri %>% 
  group_by(scientificName, treatment) %>%
  summarise( germ_ratio = mean(ratio),
            N = n())
rufus_petri_ratio_summary_modified <- rufus_petri_ratio_summary[-c(3, 4, 5, 6, 7, 8, 9, 10, 15), ]

# Now for the model

lme_rufus_petri_ratio_summary_modified <- lmer(data = rufus_petri_ratio_summary_modified, germ_ratio ~ treatment + (1 | scientificName), REML = FALSE)
summary(lme_rufus_petri_ratio_summary_modified)

null7 <- lmer(data = rufus_petri_ratio_summary_modified, germ_ratio ~ 1 + (1 | scientificName), REML = FALSE)
summary(null7)

anova(null7, lme_rufus_petri_ratio_summary_modified, test = "Chisq")
r.squaredGLMM(lme_rufus_petri_ratio_summary_modified)

# β = 0.2451
# z = 2.1965
# p = 0.1383
# R2M = 0.2085819
# R2C = 0.5011694

# First I will run the model for seeds dispersed by Microcebus rufus in the forest ground experiment
# This code calculates the ratios of germinated seeds for all the indivdual species

rufus_ground_ratio_summary <- rufus_ground %>% 
  group_by(scientificName, treatment) %>%
  summarise( germ_ratio = mean(ratio),
             N = n())
rufus_ground_ratio_summary_modified <- rufus_ground_ratio_summary[-c(1, 2, 3, 8), ]

# Now for the model

lme_rufus_ground_ratio_summary_modified <- lmer(data = rufus_ground_ratio_summary_modified, germ_ratio ~ treatment + (1 | scientificName), REML = FALSE)
summary(lme_rufus_ground_ratio_summary_modified)

null8 <- lmer(data = rufus_ground_ratio_summary_modified, germ_ratio ~ 1 + (1 | scientificName), REML = FALSE)
summary(null8)

anova(null8, lme_rufus_ground_ratio_summary_modified, test = "Chisq")
r.squaredGLMM(lme_rufus_ground_ratio_summary_modified)

# β = 3.182e-01
# z = 1.6219
# p = 0.2028
# R2M = 0.4
# R2C = 0.4000418

# First I will run the model for seeds dispersed by Microcebus jollyae in the petri dish experiment
# This code calculates the ratios of germinated seeds for all the indivdual species

jollyae_petri_ratio_summary <- jollyae_petri %>% 
  group_by(scientificName, treatment) %>%
  summarise( germ_ratio = mean(ratio),
             N = n())
jollyae_petri_ratio_summary_modified <- jollyae_petri_ratio_summary

# Now for the model

lme_jollyae_petri_ratio_summary_modified <- lmer(data = jollyae_petri_ratio_summary_modified, germ_ratio ~ treatment + (1 | scientificName), REML = FALSE)
summary(lme_jollyae_petri_ratio_summary_modified)

null9 <- lmer(data = jollyae_petri_ratio_summary_modified, germ_ratio ~ 1 + (1 | scientificName), REML = FALSE)
summary(null9)

anova(null9, lme_jollyae_petri_ratio_summary_modified, test = "Chisq")
r.squaredGLMM(lme_jollyae_petri_ratio_summary_modified)

# β = 0.26584
# z = 4.0348
# p = 0.04457
# R2M = 0.42721
# R2C = 0.4633437

# First I will run the model for seeds dispersed by Microcebus jollyae in the semi-shaded experiment
# This code calculates the ratios of germinated seeds for all the indivdual species

jollyae_semi_ratio_summary <- jollyae_semi %>% 
  group_by(scientificName, treatment) %>%
  summarise( germ_ratio = mean(ratio),
             N = n())
jollyae_semi_ratio_summary_modified <- jollyae_semi_ratio_summary

# Now for the model

lme_jollyae_semi_ratio_summary_modified <- lmer(data = jollyae_semi_ratio_summary_modified, germ_ratio ~ treatment + (1 | scientificName), REML = FALSE)
summary(lme_jollyae_semi_ratio_summary_modified)

null10 <- lmer(data = jollyae_semi_ratio_summary_modified, germ_ratio ~ 1 + (1 | scientificName), REML = FALSE)
summary(null10)

anova(null10, lme_jollyae_semi_ratio_summary_modified, test = "Chisq")
r.squaredGLMM(lme_jollyae_semi_ratio_summary_modified)

# β = 0.18646
# z = 4.486
# p = 0.03417
# R2M = 0.1364723
# R2C = 0.8845932

# First I will run the model for seeds dispersed by Microcebus jollyae in the shaded experiment
# This code calculates the ratios of germinated seeds for all the indivdual species

jollyae_closed_ratio_summary <- jollyae_closed %>% 
  group_by(scientificName, treatment) %>%
  summarise( germ_ratio = mean(ratio),
             N = n())
jollyae_closed_ratio_summary_modified <- jollyae_closed_ratio_summary[-c(5), ]

# Now for the model

lme_jollyae_closed_ratio_summary_modified <- lmer(data = jollyae_closed_ratio_summary_modified, germ_ratio ~ treatment + (1 | scientificName), REML = FALSE)
summary(lme_jollyae_closed_ratio_summary_modified)

null11 <- lmer(data = jollyae_closed_ratio_summary_modified, germ_ratio ~ 1 + (1 | scientificName), REML = FALSE)
summary(null11)

anova(null11, lme_jollyae_closed_ratio_summary_modified, test = "Chisq")
r.squaredGLMM(lme_jollyae_closed_ratio_summary_modified)

# β = -0.008769 
# z = 0.0076
# p = 0.9305
# R2M = 0.00100877
# R2C = 0.07302369

########################################################################
#
#
#
#
#
#
##################### Graph 1: violins #######################
#
#
#
#
#
#
#########################################################################


# Mean seedling length after three-month monitoring of planted seed

# The violin represents the variation of germination time and seedling length after three months


ggplot(data = rufus_petri_yes, aes(x = treatment, y = seedling_mm)) +
  geom_point() + theme_bw() + geom_violin( aes(colour = treatment, fill = treatment)) +
  stat_summary(fun = mean, geom="point", shape=23, size=5, fill = "black") +
  labs(title = "Microcebus rufus",subtitle = "Petri dish", 
       y = "mean seedling length (mm)", x = "Treatment") +
  theme(plot.title = element_text(face = "italic")) + 
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  scale_color_manual(values=c("darkgreen", "darkorange3")) +
  scale_fill_manual(values=c("darkgreen", "darkorange3")) + 
  theme(legend.position = "None")
#  N = 37(75) 51(75) 


plot1 <- ggplot(data = jollyae_petri_yes, aes(x = treatment, y = seedling_mm)) +
   theme_bw() + geom_violin( aes(colour = treatment, fill = treatment)) +
  stat_summary(fun = mean, geom="point", shape=23, size=5, fill = "black") + 
  ylim(0,50) + 
  labs(title = "Microcebus jollyae",subtitle = "Petri dish", 
       y = "mean seedling length (mm)", x = "Treatment") +
  theme(plot.title = element_text(face = "italic")) + 
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  scale_color_manual(values=c("darkgreen", "darkorange3")) +
  scale_fill_manual(values=c("darkgreen", "darkorange3")) + 
  theme(legend.position = "None")
plot1

plot2 <- ggplot(data = jollyae_semi_yes, aes(x = treatment, y = seedling_mm)) +
   theme_bw() + geom_violin( aes(colour = treatment, fill = treatment)) +
  stat_summary(fun = mean, geom="point", shape=23, size=5, fill = "black") +
  ylim(0,50) +
  labs(title = "",subtitle = "Semi-shaded", 
       y = "mean seedling length (mm)", x = "Treatment") +
  theme(plot.title = element_text(face = "italic")) + 
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  scale_color_manual(values=c("darkgreen", "darkorange3")) +
  scale_fill_manual(values=c("darkgreen", "darkorange3")) + 
  theme(legend.position = "None") + 
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank())
plot2

plot3 <- ggplot(data = jollyae_closed_yes, aes(x = treatment, y = seedling_mm)) +
  theme_bw() + geom_violin( aes(colour = treatment, fill = treatment)) +
  stat_summary(fun = mean, geom="point", shape=23, size=5, fill = "black") +
  ylim(0,50) +
  labs(title = "",subtitle = "Shaded", 
       y = "mean seedling length (mm)", x = "Treatment") +
  theme(plot.title = element_text(face = "italic")) + 
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  scale_color_manual(values=c("darkgreen", "darkorange3")) +
  scale_fill_manual(values=c("darkgreen", "darkorange3")) + 
  theme(legend.position = "None") + 
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank())
plot3
ggarrange(plot1, plot2, plot3, ncol = 3, nrow = 1)


# Points of departure 
# Included all points (original graph cut of at 60 with an "a" on top)

# Notes 
# As seen by the font differences on the original graph, 
# the N numbers were written in word
# Semi-shaded violin plot points missing >10 on replication

########################################################################
#
#
#
#
#
#
##################### graph 2, survival analysis #######################
#
#
#
#
#
#
#########################################################################

# This is the survival analysis for seeds dispersed by microcebus rufus in the petri dish experiment 
rufus_petri_yes
##### Creating a status colum to indicate that all of the seeds germinated at some point
rufus_petri_yes$status <- rep(1, times = 121, length.out = NA, each = 1)

#### Next we use the survival function to make a survival curve 
rufus_petri_yes_km_fit <- survfit(Surv(germ_time, status) ~ treatment, data = rufus_petri_yes)
rufus_petri_yes_km_fit
# This give a nice summary of the analysis
summary(rufus_petri_yes_km_fit, times = c(1,15,30,45,60,75,90))

# Next we plot the survival fit and make it look like the graph in the paper
rufus_petri_yes_km_plot <- autoplot(rufus_petri_yes_km_fit) 
rufus_petri_yes_km_plot + labs(title = "Microcebus rufus",subtitle = "Petri dish experiment", 
                           y = "Probability of germinating", x = "Time (days)") +
  theme(plot.title = element_text(face = "italic")) +
  scale_color_manual(values=c("darkgreen", "darkorange3")) +
  scale_fill_manual(values=c("darkgreen", "darkorange3")) + 
  geom_text(x=75, y=.2, label="p < 0.0001") + theme(legend.position="bottom")

# One problem though is that this graph is upside down. I used scale_y_reverse() to reverse the scale,
# but now the numbers are wrong and the annotated p value is gone. 
rufus_petri_yes_km_plot + labs(title = "Microcebus rufus",subtitle = "Petri dish experiment", 
                               y = "Probability of germinating", x = "Time (days)") +
  theme(plot.title = element_text(face = "italic")) +
  scale_color_manual(values=c("darkgreen", "darkorange3")) +
  scale_fill_manual(values=c("darkgreen", "darkorange3")) + 
  geom_text(x=75, y=.75, label="p < 0.0001") + theme(legend.position="bottom") + scale_y_reverse()

# This the the Cox test with fragility
# The summary includes the Chi square value, the degrees of freedom and the p-value
# Those are all things which I need to create the table in the paper
rufus_petri_yes_cox <- coxph(Surv(germ_time, status) ~ treatment, data=rufus_petri_yes)
summary(rufus_petri_yes_cox)

#
#
#
############################## rufus ground yes survival analysis #######################################
#
#
#

# This is the survival analysis for seeds dispersed by microcebus rufus in the forest ground experiment 
rufus_ground_yes
nrow(rufus_ground_yes)
##### Creating a status colum to indicate that all of the seeds germinated at some point
rufus_ground_yes$status <- rep(1, times = 7, length.out = NA, each = 1)

#### Next we use the survival function to make a survival curve 
rufus_ground_yes_km_fit <- survfit(Surv(germ_time, status) ~ 1, data = rufus_ground_yes)
rufus_ground_yes_km_fit
# This gives a nice summary of the analysis
summary(rufus_ground_yes_km_fit, times = c(1,15,30,45,60,75,90))

# Next we plot the survival fit and make it look like the graph in the paper
rufus_ground_yes_km_plot <- autoplot(rufus_ground_yes_km_fit) 
rufus_ground_yes_km_plot + labs(title = "Microcebus rufus",subtitle = "Forest ground experiment", 
                               y = "Probability of germinating", x = "Time (days)") +
  theme(plot.title = element_text(face = "italic")) +
  scale_color_manual(values=c("darkorange3")) +
  scale_fill_manual(values=c("darkorange3")) + theme(legend.position="bottom")

# Does not have color

# One problem though is that this graph is upside down. I used scale_y_reverse() to reverse the scale,
# but now the numbers are wrong and the annotated p value is gone. 
rufus_ground_yes_km_plot + labs(title = "Microcebus rufus",subtitle = "Forest ground experiment", 
                               y = "Probability of germinating", x = "Time (days)") +
  theme(plot.title = element_text(face = "italic")) +
  scale_color_manual(values=c("darkorange3")) +
  scale_fill_manual(values=c( "darkorange3")) + theme(legend.position="bottom") + scale_y_reverse()

# Does not have color

# There is no chi square analysis for the Microcebus rufus forest ground experiment because there is only one treatment 

#
#
#
############################## jollyae petri yes survival analysis #######################################
#
#
#

# This is the survival analysis for seeds dispersed by microcebus jollyae in the petri dish experiment 
jollyae_petri_yes
nrow(jollyae_petri_yes)
##### Creating a status colum to indicate that all of the seeds germinated at some point
jollyae_petri_yes$status <- rep(1, times = 70, length.out = NA, each = 1)

#### Next we use the survival function to make a survival curve 
jollyae_petri_yes_km_fit <- survfit(Surv(germ_time, status) ~ treatment, data = jollyae_petri_yes)
jollyae_petri_yes_km_fit
# This give a nice summary of the analysis
summary(jollyae_petri_yes_km_fit, times = c(1,15,30,45,60,75,90))

# Next we plot the survival fit and make it look like the graph in the paper
jollyae_petri_yes_km_plot <- autoplot(jollyae_petri_yes_km_fit) 
jollyae_petri_yes_km_plot

# Error: Aesthetics can not vary with a ribbon


jollyae_petri_yes_km_plot + labs(title = "Microcebus jollyae",subtitle = "Petri dish experiment", 
                               y = "Probability of germinating", x = "Time (days)") +
  theme(plot.title = element_text(face = "italic")) +
  scale_color_manual(values=c("darkgreen", "darkorange3")) +
  scale_fill_manual(values=c("darkgreen", "darkorange3")) + 
  geom_text(x=75, y=.2, label="p < 0.0001") + theme(legend.position="bottom")

# One problem though is that this graph is upside down. I used scale_y_reverse() to reverse the scale,
# but now the numbers are wrong and the annotated p value is gone. 
jollyae_petri_yes_km_plot + labs(title = "Microcebus jollyae",subtitle = "Petri dish experiment", 
                               y = "Probability of germinating", x = "Time (days)") +
  theme(plot.title = element_text(face = "italic")) +
  scale_color_manual(values=c("darkgreen", "darkorange3")) +
  scale_fill_manual(values=c("darkgreen", "darkorange3")) + 
  geom_text(x=75, y=.75, label="p < 0.0001") + theme(legend.position="bottom") + scale_y_reverse()

# This the the Cox test with fragility
# The summary includes the Chi square value, the degrees of freedom and the p-value
# Those are all things which I need to create the table in the paper
jollyae_petri_yes_cox <- coxph(Surv(germ_time, status) ~ treatment, data=jollyae_petri_yes)
summary(jollyae_petri_yes_cox)
# X^2 = 0.02
# df = 1
# p = 0.9 

#
#
#
############################## jollyae semi yes survival analysis #######################################
#
#
#

# This is the survival analysis for seeds dispersed by microcebus jollyae in the semi-shaded plot experiment 
jollyae_semi_yes
nrow(jollyae_semi_yes)
##### Creating a status colum to indicate that all of the seeds germinated at some point
jollyae_semi_yes$status <- rep(1, times = 233, length.out = NA, each = 1)

#### Next we use the survival function to make a survival curve 
jollyae_semi_yes_km_fit <- survfit(Surv(germ_time, status) ~ treatment, data = jollyae_semi_yes)
jollyae_semi_yes_km_fit
# This give a nice summary of the analysis
summary(jollyae_semi_yes_km_fit, times = c(1,15,30,45,60,75,90))

# Next we plot the survival fit and make it look like the graph in the paper
jollyae_semi_yes_km_plot <- autoplot(jollyae_semi_yes_km_fit) 
jollyae_semi_yes_km_plot


jollyae_semi_yes_km_plot + labs(title = "Microcebus jollyae",subtitle = "Semi-shaded experiment", 
                                 y = "Probability of germinating", x = "Time (days)") +
  theme(plot.title = element_text(face = "italic")) +
  scale_color_manual(values=c("darkgreen", "darkorange3")) +
  scale_fill_manual(values=c("darkgreen", "darkorange3")) + 
  geom_text(x=75, y=.2, label="p = 0.2") + theme(legend.position="bottom")

# One problem though is that this graph is upside down. I used scale_y_reverse() to reverse the scale,
# but now the numbers are wrong and the annotated p value is gone. 
jollyae_semi_yes_km_plot + labs(title = "Microcebus jollyae",subtitle = "Semi-shaded experiment", 
                                 y = "Probability of germinating", x = "Time (days)") +
  theme(plot.title = element_text(face = "italic")) +
  scale_color_manual(values=c("darkgreen", "darkorange3")) +
  scale_fill_manual(values=c("darkgreen", "darkorange3")) + 
  geom_text(x=75, y=.75, label="p = 0.2") + theme(legend.position="bottom") + scale_y_reverse()

# This the the Cox test with fragility
# The summary includes the Chi square value, the degrees of freedom and the p-value
# Those are all things which I need to create the table in the paper
jollyae_semi_yes_cox <- coxph(Surv(germ_time, status) ~ treatment, data=jollyae_semi_yes)
summary(jollyae_semi_yes_cox)
# X^2 = 1.4
# df = 1
# p = 0.2

#
#
#
############################## jollyae closed yes survival analysis #######################################
#
#
#

# This is the survival analysis for seeds dispersed by microcebus jollyae in the Shaded plot experiment 
jollyae_closed_yes
nrow(jollyae_closed_yes)
##### Creating a status colum to indicate that all of the seeds germinated at some point
jollyae_closed_yes$status <- rep(1, times = 61, length.out = NA, each = 1)

#### Next we use the survival function to make a survival curve 
jollyae_closed_yes_km_fit <- survfit(Surv(germ_time, status) ~ treatment, data = jollyae_closed_yes)
jollyae_closed_yes_km_fit
# This give a nice summary of the analysis
summary(jollyae_closed_yes_km_fit, times = c(1,15,30,45,60,75,90))

# Next we plot the survival fit and make it look like the graph in the paper
jollyae_closed_yes_km_plot <- autoplot(jollyae_closed_yes_km_fit) 
jollyae_closed_yes_km_plot


jollyae_closed_yes_km_plot + labs(title = "Microcebus jollyae",subtitle = "Shaded experiment", 
                                y = "Probability of germinating", x = "Time (days)") +
  theme(plot.title = element_text(face = "italic")) +
  scale_color_manual(values=c("darkgreen", "darkorange3")) +
  scale_fill_manual(values=c("darkgreen", "darkorange3")) + 
  geom_text(x=75, y=.2, label="p = .7") + theme(legend.position="bottom")

# One problem though is that this graph is upside down. I used scale_y_reverse() to reverse the scale,
# but now the numbers are wrong and the annotated p value is gone. 
jollyae_closed_yes_km_plot + labs(title = "Microcebus jollyae",subtitle = "closed-shaded experiment", 
                                y = "Probability of germinating", x = "Time (days)") +
  theme(plot.title = element_text(face = "italic")) +
  scale_color_manual(values=c("darkgreen", "darkorange3")) +
  scale_fill_manual(values=c("darkgreen", "darkorange3")) + 
  geom_text(x=75, y=.75, label="p = .7") + theme(legend.position="bottom") + scale_y_reverse()

# This the the Cox test with fragility
# The summary includes the Chi square value, the degrees of freedom and the p-value
# Those are all things which I need to create the table in the paper
jollyae_closed_yes_cox <- coxph(Surv(germ_time, status) ~ treatment, data=jollyae_closed_yes)
summary(jollyae_closed_yes_cox)
# X^2 = 0.17
# df = 1
# p = .7


# https://rviews.rstudio.com/2017/09/25/survival-analysis-with-r/ 


################################# The End ########################################


