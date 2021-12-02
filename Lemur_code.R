

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
  summarise(seedling_mean = mean(seedling_mm),
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
  summarise(seedling_mean = mean(seedling_mm),
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
  summarise(seedling_mean = mean(seedling_mm),
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
  summarise(seedling_mean = mean(seedling_mm),
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



############## Analysis 1: mixed effect models ###################

rufus_petri_yes_summary <- rufus_petri_yes %>% 
  group_by(scientificName, treatment) %>%
  summarise(seedling_mean = mean(seedling_mm),
            N = n())

####  This line modifies the summary statistics to include only the species for which there is a germinated and control treatment

rufus_petri_yes_summary_modified <- rufus_petri_yes_summary[-c(3, 4, 5, 6, 7, 12), ]
rufus_petri_yes_summary_modified

# only certain species have a defecated and control treatment 

# N is finally 88


# REML = FALSE, intercept model
lme_rufus_petri_yes_summary_modified2 <- lmer(data = rufus_petri_yes_summary_modified, mean ~ treatment + (1 | scientificName), REML = FALSE)
summary(lme_rufus_petri_yes_summary_modified2)
# not the correct beta

# REML = TRUE , slope model
lme_rufus_petri_yes_summary_modified3 <- lmer(data = rufus_petri_yes_summary_modified, mean ~ treatment + (1 + treatment | scientificName))
summary(lme_rufus_petri_yes_summary_modified3)
# will not run, too many effects

# REML = FALSE , slope model
lme_rufus_petri_yes_summary_modified4 <- lmer(data = rufus_petri_yes_summary_modified, mean ~ treatment + (1 + treatment | scientificName), REML = FALSE)
summary(lme_rufus_petri_yes_summary_modified4)
# will not run, too many effects

# REML = TRUE , slope and intercept
lme_rufus_petri_yes_summary_modified4 <- lmer(data = rufus_petri_yes_summary_modified, mean ~ treatment + (1 + treatment | scientificName))
summary(lme_rufus_petri_yes_summary_modified4)
# will not run, too many effects

# REML = FALSE , slope and intercept model
lme_rufus_petri_yes_summary_modified5 <- lmer(data = rufus_petri_yes_summary_modified, mean ~ treatment + (1 | scientificName) + (1 + treatment | scientificName), REML = FALSE)
summary(lme_rufus_petri_yes_summary_modified4)
# will not run, too many effects

################ Graph 1: violins ###################

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
# Christmas colors
# Included all points (original graph cut of at 60 with an "a" on top)

# Notes 
# As seen by the font differences on the original graph, 
# the N numbers were written in word
# Semi-shaded violin plot points missing >10 on replication


##################### graph 2, survival analysis #######################
# This is the survival analysis. 
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



# https://rviews.rstudio.com/2017/09/25/survival-analysis-with-r/ 


################################# The End ########################################


