##########################################################################
## File: 1d_PROMS_data_analysis_allgroups_partIII.R
## This script analysis for the PROMS Data for all groups (musicians vs. nonmusicians vs. amateurs)
# authors: Christine Nussbaum (christine.nussbaum@uni-jena.de), Jessica Senftleben
# date 10/2022, 02/2024, 05/2025

# clear directory
rm(list=ls())

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# load required packages
library(tidyverse) # version 1.3.1
library(ez) # version 4.4-0
library(effectsize) # 0.4.5

# load relevant functions
source("functions/mySummary.R") 
source("functions/tracedEZout.R")

# set some relevant setting 
options(scipen = 999)


#-------------------------------------------------#
#         Data preparation of PROMS Data          #
#-------------------------------------------------#

#Experiment data (after some preprocessing, e.g. removing participants with too many omissions)
load(file="input/amateurs_PROMS_preprocessed.RData")
P2 <- P # rename the dataset P2

load(file="input/orig_data_Nussbaum2024/PROMS_preprocessed.RData")

## merging data sets
# make amateurs into one group ("A")
P2$Group[P2$Group=="2"] <-"A"
P2$Group[P2$Group=="1"] <-"A"
P2 <- P2 %>% mutate(Group = as.factor(Group))

# merge 
P <- rbind(P, P2)

# change group for participant with music degree to professionals
P$Group[P$Code=="AEB56L"] <- "M"

save(P, file="input/allgroups_PROMS_preprocessed.RData")

rm(P2)

## Meaning of Variables

# Subject        individual participant code assigned by PsyToolkit
# Code           self-generated Code by the participant
# Group          group - M = Professionals; C = NonMusicians/Controls; A = Amateurs
# Same           is it a same or a different trial (DS = Same, DD = different)
# Stimulus       name of the stimulus
# Response       keypress of the participant (1 = definitely same, 2 = maybe same, 3 = dont know, 4 = maybe different, 5 = definitely different)
# RT             reaction time
# test           subtest (melody, pitch, timbre, rhythm)
# TrialNo        Trial number (1 to 18 for each subtest)
# Resp           coded accuracy

###--------------------------------------------------------------------------------------------------------------###
###                         Calculating Signal-Detection Parameters  and Confidence Measure                      ###
###--------------------------------------------------------------------------------------------------------------###

# Calculating Parameters of Signal Detection Theory

P$Hits <- ifelse(P$Same == "DS" & P$Resp== 1, 1, 0)
P$Hits <- ifelse(P$Same == "DD", NA, P$Hits)

P$CR <- ifelse(P$Same == "DD" & P$Resp == 1, 1, 0)
P$CR <- ifelse(P$Same == "DS", NA, P$CR)

P$Miss <- ifelse(P$Same == "DS" & P$Resp== 0, 1, 0)
P$Miss <- ifelse(P$Same == "DD", NA, P$Miss)

P$FA <- ifelse(P$Same == "DD" & P$Resp== 0, 1, 0)
P$FA <- ifelse(P$Same == "DS", NA, P$FA)

P$Sure <- ifelse(P$Response == 1 | P$Response == 5, 1,0)  # all responses where participants were "sure"
P$Unsure <- ifelse(P$Response == 2 | P$Response == 4, 1,0) # all responses where participants were "unsure"
P$DontKnow <- ifelse(P$Response == 3, 1,0) # all responses where participants responded with "don't know"


##incorporate the "unsure"-option into Hits and False Alarms - in correspondence with Dr. Hannah Strau? from Innsbruck (developers of the PROMS)

P$Hits_c <- ifelse(P$Hits == 1 & P$Unsure == 1, 0.5, P$Hits)

table(P$Hits_c, P$Hits)

P$FA_c <- ifelse(P$FA == 1 & P$Unsure == 1, 0.5, P$FA)

table(P$FA_c, P$FA)

### Calculate the confidence measure: 

# for same Trials: 1 = 1, 2 = 0.75, 3 = 0.5, 4 = 0.25, 5 = 0
# for different Trials: 1 = 0, 2 = 0.25, 3 = 0.5, 4 = 0.75, 5 = 1

#extract SameTrials and recode them
DS <- P %>% filter(Same == "DS")
DS$Confidence <-  recode(DS$Response, `1` = 1, `2` = 0.75, `3` = 0.5,  `4` = 0.25, `5` = 0)


#extract Different Trials and recode them
DD <- P %>% filter(Same == "DD")
DD$Confidence <-  recode(DD$Response, `1` = 0, `2` = 0.25, `3` = 0.5,  `4` = 0.75, `5` = 1)

P2 <- rbind(DD, DS)

#subtract 0.5 from it, to center it round 0

P2$Confidence <- P2$Confidence - 0.5

# merge them back to the main data frame:

P <- merge(P, P2)
rm(P2, DS, DD)


#### Aggregate the data

P_agg <- P %>% group_by(Code, Group, Test) %>% summarise(Resp = sum(Resp),
                                                         Hits = mean(Hits_c, na.rm= TRUE),
                                                         CR = mean(CR, na.rm=TRUE), 
                                                         Miss = mean(Miss, na.rm= TRUE), 
                                                         FA = mean(FA_c, na.rm=TRUE),
                                                         Sure = sum(Sure), 
                                                         Unsure = sum(Unsure),
                                                         DontKnow = sum(DontKnow),
                                                         Confidence = mean(Confidence), 
                                                         N = length(Trialno)) # should always be 18

####  calculate the z parameters

# correction for values of 0 and 1

P_agg$Hits <- ifelse(P_agg$Hits == 1, (90-0.5)/90, P_agg$Hits)
P_agg$FA <- ifelse(P_agg$FA == 0, 0.5/90, P_agg$FA)
P_agg$FA <- ifelse(P_agg$FA == 1, (90-0.5)/90, P_agg$FA)

# calculation of z-values
# help: dnorm, qnorm, pnorm, rnorm

P_agg$Hz <- qnorm(P_agg$Hits)
P_agg$FAz <- qnorm(P_agg$FA)


#calculate d-prime and criterion
#d-prime
P_agg$dprime <- P_agg$Hz - P_agg$FAz

#criterion
P_agg$crit <- (-0.5*(P_agg$Hz + P_agg$FAz))

B <- as.data.frame(P_agg)
rm(P_agg)
B$Group <- as.factor(B$Group)


###--------------------------------------------------------------------------------------------------------------###
###                                            Statistical Analysis                                              ###
###--------------------------------------------------------------------------------------------------------------###

# extract descriptive data
DPrime_summary <- mySummary(B, dprime, Group, Test)
Confidence_summary <- mySummary(B, Confidence, Group, Test)
Confidence_summary_2 <- mySummary(B, Confidence, Group)

###---------------------------------###
###          CONFIDENCE             ###                                 
###---------------------------------###

# mixed ANOVA on Confidence for Group x PROMS-SubTest
a <- ezANOVA(data=B, dv=.(Confidence), wid = Code, within = Test, between = .(Group), type=3, detailed = TRUE)
b = tracedEzOut(a, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")
#this analysis return a warning about unequal groups, since there are 40 musicians but 38 non-musicians and 88 amateurs

#########################################################
#expected output 
#       Effect                                       Text
#1 (Intercept) F(1, 163) = 3070.118, p < .001, np2 = .950
#2       Group F(2, 163) =   31.325, p < .001, np2 = .278
#3        Test F(3, 489) =  136.217, p < .001, np2 = .455
#4  Group:Test F(6, 489) =    4.541, p < .001, np2 = .053
#########################################################

o2 <- F_to_omega2(unlist(c(a$ANOVA[6])),  unlist(c(a$ANOVA[2])),  unlist(c(a$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2


## Main Effect Group (Confidence)
# perform pairwise t-tests on Confidence, Bonferroni-corrected a-level: .05/3 = .0167
MA <- B %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., Confidence ~ Group)
d_MA <- t_to_d(MA$statistic, MA$parameter, paired = FALSE)
MC <- B %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., Confidence ~ Group)
d_MC <- t_to_d(MC$statistic, MC$parameter, paired = FALSE)
AC <- B %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., Confidence ~ Group)
d_AC <- t_to_d(AC$statistic, AC$parameter, paired = FALSE)

## Main Effect Subtest (Confidence)
# perform pairwise t-tests on Confidence, Bonferroni-corrected a-level: .05/6 = .008
melpit <- B %>% filter(Test == "melody" | Test == "pitch") %>% t.test(data = ., Confidence ~ Test)
d_melpit <- t_to_d(melpit$statistic, melpit$parameter, paired = FALSE)
melrhy <- B %>% filter(Test == "melody" | Test == "rhythm") %>% t.test(data = ., Confidence ~ Test)
d_melrhy <- t_to_d(melrhy$statistic, melrhy$parameter, paired = FALSE)
meltim <- B %>% filter(Test == "melody" | Test == "timbre") %>% t.test(data = ., Confidence ~ Test)
d_meltim <- t_to_d(meltim$statistic, meltim$parameter, paired = FALSE)
pitrhy <- B %>% filter(Test == "pitch" | Test == "rhythm") %>% t.test(data = ., Confidence ~ Test)
d_pitrhy <- t_to_d(pitrhy$statistic, pitrhy$parameter, paired = FALSE)
pittim <- B %>% filter(Test == "pitch" | Test == "timbre") %>% t.test(data = ., Confidence ~ Test)
d_pittim <- t_to_d(pittim$statistic, pittim$parameter, paired = FALSE)
rhytim <- B %>% filter(Test == "rhythm" | Test == "timbre") %>% t.test(data = ., Confidence ~ Test)
d_rhytim <- t_to_d(rhytim$statistic, rhytim$parameter, paired = FALSE)

# info to read output easier
info <- c("dprime descriptives","Confidence descriptives","Confidence: ANOVA","Confidence: Group differences",
          "Professionals vs Amateurs","Professionals vs NonMusicians","Amateurs vs NonMusicians",
          "melody vs pitch","melody vs rhythm","melody vs timbre",
          "pitch vs rhythm","pitch vs timbre","rhythm vs timbre")

# capture output
capture.output(info[1], as.matrix(DPrime_summary), info[2], as.matrix(Confidence_summary), as.matrix(Confidence_summary_2), 
               info[3], b, o2, info[4],info[5], MA, d_MA, info[6], MC, d_MC,info[7], AC, d_AC, 
               info[8],melpit,d_melpit, info[9],melrhy,d_melrhy,info[10],meltim,d_meltim,
               info[11],pitrhy,d_pitrhy,info[12],pittim,d_pittim, info[13],rhytim,d_rhytim,
               file="output/musicality/allgroups_PROMS_summary_dprime_and_confidence.txt")

rm(DPrime_summary, Confidence_summary, Confidence_summary_2,
   info, a, b, o2, MA, d_MA, MC, d_MC,AC, d_AC,melpit,d_melpit, melrhy,d_melrhy,meltim,d_meltim, pitrhy,d_pitrhy,pittim,d_pittim, rhytim,d_rhytim)

## Interaction Group x Subtest (Confidence)
#->the effect of Group is tested for each Subtest separately

############## descriptive data  #################################
#   Test  Group Confidence     SD     N      SE     CI
#1  melody M         0.228  0.0773    40 0.0122  0.0247
#2  melody C         0.0658 0.0753    38 0.0122  0.0247
#3  melody A         0.157  0.0991    88 0.0106  0.0210
#4  pitch  M         0.268  0.0633    40 0.0100  0.0202
#5  pitch  C         0.178  0.0635    38 0.0103  0.0209
#6  pitch  A         0.235  0.0740    88 0.00788 0.0157
#7  rhythm M         0.328  0.0782    40 0.0124  0.0250
#8  rhythm C         0.266  0.0759    38 0.0123  0.0249
#9  rhythm A         0.315  0.0879    88 0.00938 0.0186
#10 timbre M         0.320  0.0815    40 0.0129  0.0261
#11 timbre C         0.261  0.0927    38 0.0150  0.0305
#12 timbre A         0.294  0.0806    88 0.00859 0.0171
#############################################################

## Subtest melody only ######################################

melody <- B %>% filter(Test == "melody")

#####################
###### ANOVA ########
# data = melody
# dv = Confidence
# wid = Code 
# between = Group

ANOVAModel <- ezANOVA(data=melody, wid=Code, dv=Confidence, between= Group, type =3, detailed=TRUE)
mel = tracedEzOut(ANOVAModel, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")
#this analysis return a warning about unequal groups, since there are 40 musicians but 38 non-musicians and 88 amateurs

o2_mel <- F_to_omega2(unlist(c(ANOVAModel$ANOVA[6])),  unlist(c(ANOVAModel$ANOVA[2])),  unlist(c(ANOVAModel$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2


#########################################################
#expected output 
#--- FORMATTED RESULTS ------------------------------------`
#       Effect                                       Text
#1 (Intercept) F(1, 163) = 133.380, p < .001, np2 = .450
#2       Group F(2, 163) =  26.128, p < .001, np2 = .243
##########################################################

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_mel <- melody %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., Confidence ~ Group)
d_MA_mel <- t_to_d(MA_mel$statistic, MA_mel$parameter, paired = FALSE)
MC_mel <- melody %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., Confidence ~ Group)
d_MC_mel <- t_to_d(MC_mel$statistic, MC_mel$parameter, paired = FALSE)
AC_mel <- melody %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., Confidence ~ Group)
d_AC_mel <- t_to_d(AC_mel$statistic, AC_mel$parameter, paired = FALSE)

## Subtest pitch only ######################################

pitch <- B %>% filter(Test == "pitch")

#####################
###### ANOVA ########
# data = pitch
# dv = Confidence
# wid = Code 
# between = Group

ANOVAModel <- ezANOVA(data=pitch, wid=Code, dv=Confidence, between= Group, type =3, detailed=TRUE)
pit = tracedEzOut(ANOVAModel, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")
#this analysis return a warning about unequal groups, since there are 40 musicians but 38 non-musicians and 88 amateurs

o2_pit <- F_to_omega2(unlist(c(ANOVAModel$ANOVA[6])),  unlist(c(ANOVAModel$ANOVA[2])),  unlist(c(ANOVAModel$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2

#########################################################
#expected output 
#--- FORMATTED RESULTS ------------------------------------`
#       Effect                                       Text
#1 (Intercept) F(1, 163) = 1547.007, p < .001, np2 = .905
#2       Group F(2, 163) =   16.872, p < .001, np2 = .172
##########################################################

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_pit <- pitch %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., Confidence ~ Group)
d_MA_pit <- t_to_d(MA_pit$statistic, MA_pit$parameter, paired = FALSE)
MC_pit <- pitch %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., Confidence ~ Group)
d_MC_pit <- t_to_d(MC_pit$statistic, MC_pit$parameter, paired = FALSE)
AC_pit <- pitch %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., Confidence ~ Group)
d_AC_pit <- t_to_d(AC_pit$statistic, AC_pit$parameter, paired = FALSE)


## Subtest timbre only ######################################

timbre <- B %>% filter(Test == "timbre")

#####################
###### ANOVA ########
# data = timbre
# dv = Confidence
# wid = Code 
# between = Group

ANOVAModel <- ezANOVA(data=timbre, wid=Code, dv=Confidence, between= Group, type =3, detailed=TRUE)
tim = tracedEzOut(ANOVAModel, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")
#this analysis return a warning about unequal groups, since there are 40 musicians but 38 non-musicians and 88 amateurs

o2_tim <- F_to_omega2(unlist(c(ANOVAModel$ANOVA[6])),  unlist(c(ANOVAModel$ANOVA[2])),  unlist(c(ANOVAModel$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2

#########################################################
#expected output 
#--- FORMATTED RESULTS ------------------------------------`
#       Effect                                       Text
#1 (Intercept) F(1, 163) = 1742.581, p < .001, np2 = .914
#2       Group F(2, 163) =    4.886, p = .009, np2 = .057
##########################################################

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_tim <- timbre %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., Confidence ~ Group)
d_MA_tim <- t_to_d(MA_tim$statistic, MA_tim$parameter, paired = FALSE)
MC_tim <- timbre %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., Confidence ~ Group)
d_MC_tim <- t_to_d(MC_tim$statistic, MC_tim$parameter, paired = FALSE)
AC_tim <- timbre %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., Confidence ~ Group)
d_AC_tim <- t_to_d(AC_tim$statistic, AC_tim$parameter, paired = FALSE)


## Subtest rhythm only ######################################

rhythm <- B %>% filter(Test == "rhythm")

#####################
###### ANOVA ########
# data = rhythm
# dv = Confidence
# wid = Code 
# between = Group

ANOVAModel <- ezANOVA(data=rhythm, wid=Code, dv=Confidence, between= Group, type =3, detailed=TRUE)
rhy = tracedEzOut(ANOVAModel, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")
#this analysis return a warning about unequal groups, since there are 40 musicians but 38 non-musicians and 88 amateurs

o2_rhy <- F_to_omega2(unlist(c(ANOVAModel$ANOVA[6])),  unlist(c(ANOVAModel$ANOVA[2])),  unlist(c(ANOVAModel$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2

#########################################################
#expected output 
#--- FORMATTED RESULTS ------------------------------------`
#       Effect                                       Text
#1 (Intercept) F(1, 163) = 1913.834, p < .001, np2 = .922
#2       Group F(2, 163) =    6.226, p = .002, np2 = .071
##########################################################

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_rhy <- rhythm %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., Confidence ~ Group)
d_MA_rhy <- t_to_d(MA_rhy$statistic, MA_rhy$parameter, paired = FALSE)
MC_rhy <- rhythm %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., Confidence ~ Group)
d_MC_rhy <- t_to_d(MC_rhy$statistic, MC_rhy$parameter, paired = FALSE)
AC_rhy <- rhythm %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., Confidence ~ Group)
d_AC_rhy <- t_to_d(AC_rhy$statistic, AC_rhy$parameter, paired = FALSE)


# save all the t.test on melody

#add descriptions for output:
info <- c("Confidence",
          "Professionals vs Amateurs","Professionals vs NonMusicians","Amateurs vs NonMusicians")

# save output to textfile
capture.output(info[1], mel, o2_mel, info[2], MA_mel, d_MA_mel, info[3],MC_mel, d_MC_mel, info[4], AC_mel, d_AC_mel,
               file="output/musicality/allgroups_PROMS_melody.txt")

#remove objects to keep environment tidy
rm(info,melody, mel, o2_mel, MA_mel, d_MA_mel,MC_mel, d_MC_mel, AC_mel, d_AC_mel)


# save all the t.test on pitch
#add descriptions for output:
info <- c("Confidence","Professionals vs Amateurs","Professionals vs NonMusicians","Amateurs vs NonMusicians")

# save output to textfile
capture.output(info[1], pit, o2_pit, info[2], MA_pit, d_MA_pit, info[3],MC_pit, d_MC_pit, info[4], AC_pit, d_AC_pit,
               file="output/musicality/allgroups_PROMS_pitch.txt")

#remove objects to keep environment tidy
rm(info,pit,o2_pit, pitch,MA_pit, d_MA_pit,MC_pit, d_MC_pit, AC_pit, d_AC_pit)


# save all the t.test on rhythm

#add descriptions for output:
info <- c("Confidence",
          "Professionals vs Amateurs","Professionals vs NonMusicians","Amateurs vs NonMusicians")

# save output to textfile
capture.output(info[1], rhy, o2_rhy,info[2], MA_rhy, d_MA_rhy, info[3],MC_rhy, d_MC_rhy, info[4], AC_rhy, d_AC_rhy,
               file="output/musicality/allgroups_PROMS_rhythm.txt")

#remove objects to keep environment tidy
rm(info,rhy,o2_rhy, rhythm, MA_rhy, d_MA_rhy,MC_rhy, d_MC_rhy, AC_rhy, d_AC_rhy)


# save all the t.test on timbre

#add descriptions for output:
info <- c("Confidence",
          "Professionals vs Amateurs","Professionals vs NonMusicians","Amateurs vs NonMusicians")

# save output to textfile
capture.output(info[1], tim,o2_tim, info[2], MA_tim, d_MA_tim, info[3],MC_tim, d_MC_tim, info[4], AC_tim, d_AC_tim, 
               file="output/musicality/allgroups_PROMS_timbre.txt")

#remove objects to keep environment tidy
rm(info,tim, o2_tim,timbre, MA_tim, d_MA_tim,MC_tim, d_MC_tim, AC_tim, d_AC_tim)

rm(ANOVAModel)


# save the prepared dataset for further analyses
PROMS <- B
save(PROMS, file="input/allgroups_PROMS_prepared.RData")

## End of Script