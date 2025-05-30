##########################################################################
## File: 00b_preparing_data_for_JASP.R
## Data Preparation for the separate Baysian analysis in JASP
# authors: Christine Nussbaum, Jessica Senftleben
# date 05/2025


# clear directory
rm(list=ls())


# load required packages
library(tidyverse) # version 1.3.1

#Experiment data (after some preprocessing, e.g. removing participants with too many omissions)
load(file="input/amateurs_Exp_processed.RData")

#remove participant with a Degree in Music Science
D <- subset (D, Code!="AEB56L") # instrumentalist


# each participant (N = 88) completed 312 trials, resulting in a dataset with 27456 rows


## Meaning of Variables

# Subject                       individual participant code assigned by PsyToolkit
# Code                          self-generated Code by the participant
# Group                         group - 1 = singers; 2 = instrumentalists 
# SpId                          speaker ID
# Emo                           speaker emotion; hap = happiness, ple = pleasure, fea = fear, sad = sadness, avg = average
# Word                          speaker pseudoword, w01 = /belam/, w02 = /molen/,   w05 = /loman/
# MType                         speaker morph type, full = full morphing, f0 = F0 morph, tbr = timbre morph 
# SpSex                         speaker sex, f = female, m = male
# filename                      stimulus' filename
# CB                            counterbalancing condition (assignment of response keys to emotion)
# ACC                           accuracy: 0 = incorrect, 1 = correct
# Resp                          response (keypress -> emotion response depends on CB condition)
# RT                            reaction time (5000 corresponds to omission)

#remove average trials
D <- D %>% filter(Emo != "avg")



#---------------------------------------------------------------------------------------------#
#                PartI: Extract averaged information for the Bayesian analyses                #
#---------------------------------------------------------------------------------------------#

### aggregate data for ANOVA
D <- D %>% group_by(Subject, Group, Emo, MType) %>% summarise(ACC = mean(ACC))

D2 <- D %>% group_by(Subject, Group, MType) %>% summarise(ACC = mean(ACC))


#for the Bayesian ANOVA
D_wide_anova <- D %>% pivot_wider(names_from = c(Emo, MType), names_sep = "_", values_from = ACC)
write.csv(D_wide_anova, file = "JASP/D_wide_anova_JASP.txt")

#for the Baysian t.tests
D_wide_ttest <- D2 %>% pivot_wider(names_from = c(MType), names_sep = "_", values_from = ACC)
D_wide_ttest$Mean <- (D_wide_ttest$f0 + D_wide_ttest$tbr + D_wide_ttest$full)/3 # average across all MTypes

write.csv(D_wide_ttest, file = "JASP/D_wide_ttest_JASP.txt")


#---------------------------------------------------------------------------------------------#
#                PartIII: Extract averaged information for the Bayesian analyses              #
#---------------------------------------------------------------------------------------------#

# clear directory
rm(list=ls())

#Experiment data (after some preprocessing, e.g. removing participants with too many omissions)
load(file="input/allgroups_Exp_processed.RData")

#remove average trials
D <- D %>% filter(Emo != "avg")


### aggregate data for ANOVA
D <- D %>% group_by(Subject, Group, Emo, MType) %>% summarise(ACC = mean(ACC))

D2 <- D %>% group_by(Subject, Group, MType) %>% summarise(ACC = mean(ACC))


#for the Bayesian ANOVA
D_wide_anova <- D %>% pivot_wider(names_from = c(Emo, MType), names_sep = "_", values_from = ACC)


write.csv(D_wide_anova, file = "JASP/D_wide_anova_JASP_PartIII.txt")


#for the Baysian t.tests
D_wide_ttest <- D2 %>% pivot_wider(names_from = c(MType), names_sep = "_", values_from = ACC)
D_wide_ttest$Mean <- (D_wide_ttest$f0 + D_wide_ttest$tbr + D_wide_ttest$full)/3 # average across all MTypes

#only professionals and amateurs 
D_wide_ttest_MA <- D_wide_ttest %>% filter(Group != "C")
write.csv(D_wide_ttest_MA, file = "JASP/D_wide_test_MA_JASP_PartIII.txt")

#only nonmusicians and amateurs 
D_wide_ttest_CA <- D_wide_ttest %>% filter(Group != "M")
write.csv(D_wide_ttest_CA, file = "JASP/D_wide_ttest_CA_JASP_PartIII.txt")


### End of Script