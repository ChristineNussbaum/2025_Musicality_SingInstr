##########################################################################
## File: 06b_PROMS_data_analysis_allgroups.R
## This script analysis for the PROMS Data for all groups (musicians vs. nonmusicians vs. amateurs)
# authors: Christine Nussbaum (christine.nussbaum@uni-jena.de), Jessica Senftleben
# date 10/2022, 02/2024

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
load(file="input/musicians vs nonmusicians/PROMS_preprocessed.RData")

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
###          CONFIDENCE                                             
###---------------------------------###

# mixed ANOVA on Confidence for Group x PROMS-SubTest
a <- ezANOVA(data=B, dv=.(Confidence), wid = Code, within = Test, between = .(Group), type=3, detailed = TRUE)
b = tracedEzOut(a, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")
#this analysis return a warning about unequal groups, since there are 39 musicians but 38 non-musicians and 89 amateurs

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
MA <- B %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., Confidence ~ Group, paired=FALSE)
d_MA <- t_to_d(MA$statistic, MA$parameter, paired = FALSE)
MC <- B %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., Confidence ~ Group, paired=FALSE)
d_MC <- t_to_d(MC$statistic, MC$parameter, paired = FALSE)
AC <- B %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., Confidence ~ Group, paired=FALSE)
d_AC <- t_to_d(AC$statistic, AC$parameter, paired = FALSE)

## Main Effect Subtest (Confidence)
# perform pairwise t-tests on Confidence, Bonferroni-corrected a-level: .05/6 = .008
melpit <- B %>% filter(Test == "melody" | Test == "pitch") %>% t.test(data = ., Confidence ~ Test, paired=FALSE)
d_melpit <- t_to_d(melpit$statistic, melpit$parameter, paired = FALSE)
melrhy <- B %>% filter(Test == "melody" | Test == "rhythm") %>% t.test(data = ., Confidence ~ Test, paired=FALSE)
d_melrhy <- t_to_d(melrhy$statistic, melrhy$parameter, paired = FALSE)
meltim <- B %>% filter(Test == "melody" | Test == "timbre") %>% t.test(data = ., Confidence ~ Test, paired=FALSE)
d_meltim <- t_to_d(meltim$statistic, meltim$parameter, paired = FALSE)
pitrhy <- B %>% filter(Test == "pitch" | Test == "rhythm") %>% t.test(data = ., Confidence ~ Test, paired=FALSE)
d_pitrhy <- t_to_d(pitrhy$statistic, pitrhy$parameter, paired = FALSE)
pittim <- B %>% filter(Test == "pitch" | Test == "timbre") %>% t.test(data = ., Confidence ~ Test, paired=FALSE)
d_pittim <- t_to_d(pittim$statistic, pittim$parameter, paired = FALSE)
rhytim <- B %>% filter(Test == "rhythm" | Test == "timbre") %>% t.test(data = ., Confidence ~ Test, paired=FALSE)
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
#this analysis return a warning about unequal groups, since there are 39 musicians but 38 non-musicians and 89 amateurs

o2_mel <- F_to_omega2(unlist(c(ANOVAModel$ANOVA[6])),  unlist(c(ANOVAModel$ANOVA[2])),  unlist(c(ANOVAModel$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2


#########################################################
#expected output 
#--- FORMATTED RESULTS ------------------------------------`
#       Effect                                       Text
#1 (Intercept) F(1, 163) = 133.380, p < .001, np2 = .450
#2       Group F(2, 163) =  26.128, p < .001, np2 = .243
##########################################################

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_mel <- melody %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., Confidence ~ Group, paired=FALSE)
d_MA_mel <- t_to_d(MA_mel$statistic, MA_mel$parameter, paired = FALSE)
MC_mel <- melody %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., Confidence ~ Group, paired=FALSE)
d_MC_mel <- t_to_d(MC_mel$statistic, MC_mel$parameter, paired = FALSE)
AC_mel <- melody %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., Confidence ~ Group, paired=FALSE)
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
#this analysis return a warning about unequal groups, since there are 39 musicians but 38 non-musicians and 89 amateurs

o2_pit <- F_to_omega2(unlist(c(ANOVAModel$ANOVA[6])),  unlist(c(ANOVAModel$ANOVA[2])),  unlist(c(ANOVAModel$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2

#########################################################
#expected output 
#--- FORMATTED RESULTS ------------------------------------`
#       Effect                                       Text
#1 (Intercept) F(1, 163) = 1547.007, p < .001, np2 = .905
#2       Group F(2, 163) =   16.872, p < .001, np2 = .172
##########################################################

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_pit <- pitch %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., Confidence ~ Group, paired=FALSE)
d_MA_pit <- t_to_d(MA_pit$statistic, MA_pit$parameter, paired = FALSE)
MC_pit <- pitch %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., Confidence ~ Group, paired=FALSE)
d_MC_pit <- t_to_d(MC_pit$statistic, MC_pit$parameter, paired = FALSE)
AC_pit <- pitch %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., Confidence ~ Group, paired=FALSE)
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
#this analysis return a warning about unequal groups, since there are 39 musicians but 38 non-musicians and 89 amateurs

o2_tim <- F_to_omega2(unlist(c(ANOVAModel$ANOVA[6])),  unlist(c(ANOVAModel$ANOVA[2])),  unlist(c(ANOVAModel$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2

#########################################################
#expected output 
#--- FORMATTED RESULTS ------------------------------------`
#       Effect                                       Text
#1 (Intercept) F(1, 163) = 1742.581, p < .001, np2 = .914
#2       Group F(2, 163) =    4.886, p = .009, np2 = .057
##########################################################

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_tim <- timbre %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., Confidence ~ Group, paired=FALSE)
d_MA_tim <- t_to_d(MA_tim$statistic, MA_tim$parameter, paired = FALSE)
MC_tim <- timbre %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., Confidence ~ Group, paired=FALSE)
d_MC_tim <- t_to_d(MC_tim$statistic, MC_tim$parameter, paired = FALSE)
AC_tim <- timbre %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., Confidence ~ Group, paired=FALSE)
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
#this analysis return a warning about unequal groups, since there are 39 musicians but 38 non-musicians and 89 amateurs

o2_rhy <- F_to_omega2(unlist(c(ANOVAModel$ANOVA[6])),  unlist(c(ANOVAModel$ANOVA[2])),  unlist(c(ANOVAModel$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2

#########################################################
#expected output 
#--- FORMATTED RESULTS ------------------------------------`
#       Effect                                       Text
#1 (Intercept) F(1, 163) = 1913.834, p < .001, np2 = .922
#2       Group F(2, 163) =    6.226, p = .002, np2 = .071
##########################################################

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_rhy <- rhythm %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., Confidence ~ Group, paired=FALSE)
d_MA_rhy <- t_to_d(MA_rhy$statistic, MA_rhy$parameter, paired = FALSE)
MC_rhy <- rhythm %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., Confidence ~ Group, paired=FALSE)
d_MC_rhy <- t_to_d(MC_rhy$statistic, MC_rhy$parameter, paired = FALSE)
AC_rhy <- rhythm %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., Confidence ~ Group, paired=FALSE)
d_AC_rhy <- t_to_d(AC_rhy$statistic, AC_rhy$parameter, paired = FALSE)


###---------------------------------###
###          Other Variables                                            
###---------------------------------###

### melody: pairwise comparisons

# dprime:  pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_melodyI <- melody %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., dprime ~ Group, paired=FALSE)
d_MA_melodyI <- t_to_d(MA_melodyI$statistic, MA_melodyI$parameter, paired = FALSE)
MC_melodyI <- melody %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., dprime ~ Group, paired=FALSE)
d_MC_melodyI <- t_to_d(MC_melodyI$statistic, MC_melodyI$parameter, paired = FALSE)
AC_melodyI <- melody %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., dprime ~ Group, paired=FALSE)
d_AC_melodyI <- t_to_d(AC_melodyI$statistic, AC_melodyI$parameter, paired = FALSE)

# Response Accuracy: pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_melodyII <- melody %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., Resp ~ Group, paired=FALSE)
d_MA_melodyII <- t_to_d(MA_melodyII$statistic, MA_melodyII$parameter, paired = FALSE)
MC_melodyII <- melody %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., Resp ~ Group, paired=FALSE)
d_MC_melodyII <- t_to_d(MC_melodyII$statistic, MC_melodyII$parameter, paired = FALSE)
AC_melodyII <- melody %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., Resp ~ Group, paired=FALSE)
d_AC_melodyII <- t_to_d(AC_melodyII$statistic, AC_melodyII$parameter, paired = FALSE)

# Criterion: pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_melodyIII <- melody %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., crit ~ Group, paired=FALSE)
d_MA_melodyIII <- t_to_d(MA_melodyIII$statistic, MA_melodyIII$parameter, paired = FALSE)
MC_melodyIII <- melody %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., crit ~ Group, paired=FALSE)
d_MC_melodyIII <- t_to_d(MC_melodyIII$statistic, MC_melodyIII$parameter, paired = FALSE)
AC_melodyIII <- melody %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., crit ~ Group, paired=FALSE)
d_AC_melodyIII <- t_to_d(AC_melodyIII$statistic, AC_melodyIII$parameter, paired = FALSE)

# unsure Responses: pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_melodyIV <- melody %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., Unsure ~ Group, paired=FALSE)
d_MA_melodyIV <- t_to_d(MA_melodyIV$statistic, MA_melodyIV$parameter, paired = FALSE)
MC_melodyIV <- melody %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., Unsure ~ Group, paired=FALSE)
d_MC_melodyIV <- t_to_d(MC_melodyIV$statistic, MC_melodyIV$parameter, paired = FALSE)
AC_melodyIV <- melody %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., Unsure ~ Group, paired=FALSE)
d_AC_melodyIV <- t_to_d(AC_melodyIV$statistic, AC_melodyIV$parameter, paired = FALSE)

# DontKnow Responses: pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_melodyV <- melody %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., DontKnow ~ Group, paired=FALSE)
d_MA_melodyV <- t_to_d(MA_melodyV$statistic, MA_melodyV$parameter, paired = FALSE)
MC_melodyV <- melody %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., DontKnow ~ Group, paired=FALSE)
d_MC_melodyV <- t_to_d(MC_melodyV$statistic, MC_melodyV$parameter, paired = FALSE)
AC_melodyV <- melody %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., DontKnow ~ Group, paired=FALSE)
d_AC_melodyV <- t_to_d(AC_melodyV$statistic, AC_melodyV$parameter, paired = FALSE)

# save all the t.test on melody

#add descriptions for output:
info <- c("Confidence",
          "Professionals vs Amateurs","Professionals vs NonMusicians","Amateurs vs NonMusicians",
          "dprime","Response Accuracy", "Criterion","unsure Responses","DontKnow Responses")

# save output to textfile
capture.output(info[1], mel, o2_mel, info[2], MA_mel, d_MA_mel, info[3],MC_mel, d_MC_mel, info[4], AC_mel, d_AC_mel, 
               info[5],info[2],MA_melodyI,d_MA_melodyI, info[3],MC_melodyI,d_MC_melodyI, info[4],AC_melodyI,d_AC_melodyI,
               info[6],info[2],MA_melodyII,d_MA_melodyII, info[3],MC_melodyII,d_MC_melodyII, info[4],AC_melodyII,d_AC_melodyII,
               info[7],info[2],MA_melodyIII,d_MA_melodyIII, info[3],MC_melodyIII,d_MC_melodyIII, info[4],AC_melodyIII,d_AC_melodyIII,
               info[8],info[2],MA_melodyIV,d_MA_melodyIV, info[3],MC_melodyIV,d_MC_melodyIV, info[4],AC_melodyIV,d_AC_melodyIV,
               info[9],info[2],MA_melodyV,d_MA_melodyV, info[3],MC_melodyV,d_MC_melodyV, info[4],AC_melodyV,d_AC_melodyV,
               file="output/musicality/allgroups_PROMS_melody.txt")

#remove objects to keep environment tidy
rm(info,melody, mel, o2_mel, MA_mel, d_MA_mel,MC_mel, d_MC_mel, AC_mel, d_AC_mel, 
   MA_melodyI,d_MA_melodyI, MC_melodyI,d_MC_melodyI, AC_melodyI,d_AC_melodyI,
   MA_melodyII,d_MA_melodyII, MC_melodyII,d_MC_melodyII, AC_melodyII,d_AC_melodyII,
   MA_melodyIII,d_MA_melodyIII, MC_melodyIII,d_MC_melodyIII, AC_melodyIII,d_AC_melodyIII,
   MA_melodyIV,d_MA_melodyIV, MC_melodyIV,d_MC_melodyIV, AC_melodyIV,d_AC_melodyIV,
   MA_melodyV,d_MA_melodyV, MC_melodyV,d_MC_melodyV, AC_melodyV,d_AC_melodyV)

### pitch: pairwise comparisons

# dprime:  pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_pitchI <- pitch %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., dprime ~ Group, paired=FALSE)
d_MA_pitchI <- t_to_d(MA_pitchI$statistic, MA_pitchI$parameter, paired = FALSE)
MC_pitchI <- pitch %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., dprime ~ Group, paired=FALSE)
d_MC_pitchI <- t_to_d(MC_pitchI$statistic, MC_pitchI$parameter, paired = FALSE)
AC_pitchI <- pitch %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., dprime ~ Group, paired=FALSE)
d_AC_pitchI <- t_to_d(AC_pitchI$statistic, AC_pitchI$parameter, paired = FALSE)

# Response Accuracy: pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_pitchII <- pitch %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., Resp ~ Group, paired=FALSE)
d_MA_pitchII <- t_to_d(MA_pitchII$statistic, MA_pitchII$parameter, paired = FALSE)
MC_pitchII <- pitch %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., Resp ~ Group, paired=FALSE)
d_MC_pitchII <- t_to_d(MC_pitchII$statistic, MC_pitchII$parameter, paired = FALSE)
AC_pitchII <- pitch %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., Resp ~ Group, paired=FALSE)
d_AC_pitchII <- t_to_d(AC_pitchII$statistic, AC_pitchII$parameter, paired = FALSE)

# Criterion: pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_pitchIII <- pitch %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., crit ~ Group, paired=FALSE)
d_MA_pitchIII <- t_to_d(MA_pitchIII$statistic, MA_pitchIII$parameter, paired = FALSE)
MC_pitchIII <- pitch %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., crit ~ Group, paired=FALSE)
d_MC_pitchIII <- t_to_d(MC_pitchIII$statistic, MC_pitchIII$parameter, paired = FALSE)
AC_pitchIII <- pitch %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., crit ~ Group, paired=FALSE)
d_AC_pitchIII <- t_to_d(AC_pitchIII$statistic, AC_pitchIII$parameter, paired = FALSE)

# unsure Responses: pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_pitchIV <- pitch %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., Unsure ~ Group, paired=FALSE)
d_MA_pitchIV <- t_to_d(MA_pitchIV$statistic, MA_pitchIV$parameter, paired = FALSE)
MC_pitchIV <- pitch %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., Unsure ~ Group, paired=FALSE)
d_MC_pitchIV <- t_to_d(MC_pitchIV$statistic, MC_pitchIV$parameter, paired = FALSE)
AC_pitchIV <- pitch %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., Unsure ~ Group, paired=FALSE)
d_AC_pitchIV <- t_to_d(AC_pitchIV$statistic, AC_pitchIV$parameter, paired = FALSE)

# DontKnow Responses: pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_pitchV <- pitch %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., DontKnow ~ Group, paired=FALSE)
d_MA_pitchV <- t_to_d(MA_pitchV$statistic, MA_pitchV$parameter, paired = FALSE)
MC_pitchV <- pitch %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., DontKnow ~ Group, paired=FALSE)
d_MC_pitchV <- t_to_d(MC_pitchV$statistic, MC_pitchV$parameter, paired = FALSE)
AC_pitchV <- pitch %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., DontKnow ~ Group, paired=FALSE)
d_AC_pitchV <- t_to_d(AC_pitchV$statistic, AC_pitchV$parameter, paired = FALSE)

# save all the t.test on pitch
#add descriptions for output:
info <- c("Confidence","Professionals vs Amateurs","Professionals vs NonMusicians","Amateurs vs NonMusicians",
          "dprime","Response Accuracy", "Criterion","unsure Responses","DontKnow Responses")

# save output to textfile
capture.output(info[1], pit, o2_pit, info[2], MA_pit, d_MA_pit, info[3],MC_pit, d_MC_pit, info[4], AC_pit, d_AC_pit, 
               info[5],info[2],MA_pitchI,d_MA_pitchI, info[3],MC_pitchI,d_MC_pitchI, info[4],AC_pitchI,d_AC_pitchI,
               info[6],info[2],MA_pitchII,d_MA_pitchII, info[3],MC_pitchII,d_MC_pitchII, info[4],AC_pitchII,d_AC_pitchII,
               info[7],info[2],MA_pitchIII,d_MA_pitchIII, info[3],MC_pitchIII,d_MC_pitchIII, info[4],AC_pitchIII,d_AC_pitchIII,
               info[8],info[2],MA_pitchIV,d_MA_pitchIV, info[3],MC_pitchIV,d_MC_pitchIV, info[4],AC_pitchIV,d_AC_pitchIV,
               info[9],info[2],MA_pitchV,d_MA_pitchV, info[3],MC_pitchV,d_MC_pitchV, info[4],AC_pitchV,d_AC_pitchV,
               file="output/musicality/allgroups_PROMS_pitch.txt")

#remove objects to keep environment tidy
rm(info,pit,o2_pit, pitch,MA_pit, d_MA_pit,MC_pit, d_MC_pit, AC_pit, d_AC_pit, 
   MA_pitchI,d_MA_pitchI, MC_pitchI,d_MC_pitchI, AC_pitchI,d_AC_pitchI,
   MA_pitchII,d_MA_pitchII, MC_pitchII,d_MC_pitchII, AC_pitchII,d_AC_pitchII,
   MA_pitchIII,d_MA_pitchIII, MC_pitchIII,d_MC_pitchIII, AC_pitchIII,d_AC_pitchIII,
   MA_pitchIV,d_MA_pitchIV, MC_pitchIV,d_MC_pitchIV, AC_pitchIV,d_AC_pitchIV,
   MA_pitchV,d_MA_pitchV, MC_pitchV,d_MC_pitchV, AC_pitchV,d_AC_pitchV)

### rhythm: pairwise comparisons

# dprime:  pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_rhythmI <- rhythm %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., dprime ~ Group, paired=FALSE)
d_MA_rhythmI <- t_to_d(MA_rhythmI$statistic, MA_rhythmI$parameter, paired = FALSE)
MC_rhythmI <- rhythm %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., dprime ~ Group, paired=FALSE)
d_MC_rhythmI <- t_to_d(MC_rhythmI$statistic, MC_rhythmI$parameter, paired = FALSE)
AC_rhythmI <- rhythm %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., dprime ~ Group, paired=FALSE)
d_AC_rhythmI <- t_to_d(AC_rhythmI$statistic, AC_rhythmI$parameter, paired = FALSE)

# Response Accuracy: pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_rhythmII <- rhythm %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., Resp ~ Group, paired=FALSE)
d_MA_rhythmII <- t_to_d(MA_rhythmII$statistic, MA_rhythmII$parameter, paired = FALSE)
MC_rhythmII <- rhythm %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., Resp ~ Group, paired=FALSE)
d_MC_rhythmII <- t_to_d(MC_rhythmII$statistic, MC_rhythmII$parameter, paired = FALSE)
AC_rhythmII <- rhythm %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., Resp ~ Group, paired=FALSE)
d_AC_rhythmII <- t_to_d(AC_rhythmII$statistic, AC_rhythmII$parameter, paired = FALSE)

# Criterion: pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_rhythmIII <- rhythm %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., crit ~ Group, paired=FALSE)
d_MA_rhythmIII <- t_to_d(MA_rhythmIII$statistic, MA_rhythmIII$parameter, paired = FALSE)
MC_rhythmIII <- rhythm %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., crit ~ Group, paired=FALSE)
d_MC_rhythmIII <- t_to_d(MC_rhythmIII$statistic, MC_rhythmIII$parameter, paired = FALSE)
AC_rhythmIII <- rhythm %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., crit ~ Group, paired=FALSE)
d_AC_rhythmIII <- t_to_d(AC_rhythmIII$statistic, AC_rhythmIII$parameter, paired = FALSE)

# unsure Responses: pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_rhythmIV <- rhythm %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., Unsure ~ Group, paired=FALSE)
d_MA_rhythmIV <- t_to_d(MA_rhythmIV$statistic, MA_rhythmIV$parameter, paired = FALSE)
MC_rhythmIV <- rhythm %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., Unsure ~ Group, paired=FALSE)
d_MC_rhythmIV <- t_to_d(MC_rhythmIV$statistic, MC_rhythmIV$parameter, paired = FALSE)
AC_rhythmIV <- rhythm %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., Unsure ~ Group, paired=FALSE)
d_AC_rhythmIV <- t_to_d(AC_rhythmIV$statistic, AC_rhythmIV$parameter, paired = FALSE)

# DontKnow Responses: pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_rhythmV <- rhythm %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., DontKnow ~ Group, paired=FALSE)
d_MA_rhythmV <- t_to_d(MA_rhythmV$statistic, MA_rhythmV$parameter, paired = FALSE)
MC_rhythmV <- rhythm %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., DontKnow ~ Group, paired=FALSE)
d_MC_rhythmV <- t_to_d(MC_rhythmV$statistic, MC_rhythmV$parameter, paired = FALSE)
AC_rhythmV <- rhythm %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., DontKnow ~ Group, paired=FALSE)
d_AC_rhythmV <- t_to_d(AC_rhythmV$statistic, AC_rhythmV$parameter, paired = FALSE)

# save all the t.test on rhythm

#add descriptions for output:
info <- c("Confidence",
          "Professionals vs Amateurs","Professionals vs NonMusicians","Amateurs vs NonMusicians",
          "dprime","Response Accuracy", "Criterion","unsure Responses","DontKnow Responses")

# save output to textfile
capture.output(info[1], rhy, o2_rhy,info[2], MA_rhy, d_MA_rhy, info[3],MC_rhy, d_MC_rhy, info[4], AC_rhy, d_AC_rhy, 
               info[5],info[2],MA_rhythmI,d_MA_rhythmI, info[3],MC_rhythmI,d_MC_rhythmI, info[4],AC_rhythmI,d_AC_rhythmI,
               info[6],info[2],MA_rhythmII,d_MA_rhythmII, info[3],MC_rhythmII,d_MC_rhythmII, info[4],AC_rhythmII,d_AC_rhythmII,
               info[7],info[2],MA_rhythmIII,d_MA_rhythmIII, info[3],MC_rhythmIII,d_MC_rhythmIII, info[4],AC_rhythmIII,d_AC_rhythmIII,
               info[8],info[2],MA_rhythmIV,d_MA_rhythmIV, info[3],MC_rhythmIV,d_MC_rhythmIV, info[4],AC_rhythmIV,d_AC_rhythmIV,
               info[9],info[2],MA_rhythmV,d_MA_rhythmV, info[3],MC_rhythmV,d_MC_rhythmV, info[4],AC_rhythmV,d_AC_rhythmV,
               file="output/musicality/allgroups_PROMS_rhythm.txt")

#remove objects to keep environment tidy
rm(info,rhy,o2_rhy, rhythm, MA_rhy, d_MA_rhy,MC_rhy, d_MC_rhy, AC_rhy, d_AC_rhy, 
   MA_rhythmI,d_MA_rhythmI, MC_rhythmI,d_MC_rhythmI, AC_rhythmI,d_AC_rhythmI,
   MA_rhythmII,d_MA_rhythmII, MC_rhythmII,d_MC_rhythmII, AC_rhythmII,d_AC_rhythmII,
   MA_rhythmIII,d_MA_rhythmIII, MC_rhythmIII,d_MC_rhythmIII, AC_rhythmIII,d_AC_rhythmIII,
   MA_rhythmIV,d_MA_rhythmIV, MC_rhythmIV,d_MC_rhythmIV, AC_rhythmIV,d_AC_rhythmIV,
   MA_rhythmV,d_MA_rhythmV, MC_rhythmV,d_MC_rhythmV, AC_rhythmV,d_AC_rhythmV)

# timbre: pairwise comparisons

# dprime:  pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_timbreI <- timbre %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., dprime ~ Group, paired=FALSE)
d_MA_timbreI <- t_to_d(MA_timbreI$statistic, MA_timbreI$parameter, paired = FALSE)
MC_timbreI <- timbre %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., dprime ~ Group, paired=FALSE)
d_MC_timbreI <- t_to_d(MC_timbreI$statistic, MC_timbreI$parameter, paired = FALSE)
AC_timbreI <- timbre %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., dprime ~ Group, paired=FALSE)
d_AC_timbreI <- t_to_d(AC_timbreI$statistic, AC_timbreI$parameter, paired = FALSE)

# Response Accuracy: pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_timbreII <- timbre %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., Resp ~ Group, paired=FALSE)
d_MA_timbreII <- t_to_d(MA_timbreII$statistic, MA_timbreII$parameter, paired = FALSE)
MC_timbreII <- timbre %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., Resp ~ Group, paired=FALSE)
d_MC_timbreII <- t_to_d(MC_timbreII$statistic, MC_timbreII$parameter, paired = FALSE)
AC_timbreII <- timbre %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., Resp ~ Group, paired=FALSE)
d_AC_timbreII <- t_to_d(AC_timbreII$statistic, AC_timbreII$parameter, paired = FALSE)

# Criterion: pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_timbreIII <- timbre %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., crit ~ Group, paired=FALSE)
d_MA_timbreIII <- t_to_d(MA_timbreIII$statistic, MA_timbreIII$parameter, paired = FALSE)
MC_timbreIII <- timbre %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., crit ~ Group, paired=FALSE)
d_MC_timbreIII <- t_to_d(MC_timbreIII$statistic, MC_timbreIII$parameter, paired = FALSE)
AC_timbreIII <- timbre %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., crit ~ Group, paired=FALSE)
d_AC_timbreIII <- t_to_d(AC_timbreIII$statistic, AC_timbreIII$parameter, paired = FALSE)

# unsure Responses: pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_timbreIV <- timbre %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., Unsure ~ Group, paired=FALSE)
d_MA_timbreIV <- t_to_d(MA_timbreIV$statistic, MA_timbreIV$parameter, paired = FALSE)
MC_timbreIV <- timbre %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., Unsure ~ Group, paired=FALSE)
d_MC_timbreIV <- t_to_d(MC_timbreIV$statistic, MC_timbreIV$parameter, paired = FALSE)
AC_timbreIV <- timbre %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., Unsure ~ Group, paired=FALSE)
d_AC_timbreIV <- t_to_d(AC_timbreIV$statistic, AC_timbreIV$parameter, paired = FALSE)

# DontKnow Responses: pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_timbreV <- timbre %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., DontKnow ~ Group, paired=FALSE)
d_MA_timbreV <- t_to_d(MA_timbreV$statistic, MA_timbreV$parameter, paired = FALSE)
MC_timbreV <- timbre %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., DontKnow ~ Group, paired=FALSE)
d_MC_timbreV <- t_to_d(MC_timbreV$statistic, MC_timbreV$parameter, paired = FALSE)
AC_timbreV <- timbre %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., DontKnow ~ Group, paired=FALSE)
d_AC_timbreV <- t_to_d(AC_timbreV$statistic, AC_timbreV$parameter, paired = FALSE)

# save all the t.test on timbre

#add descriptions for output:
info <- c("Confidence",
          "Professionals vs Amateurs","Professionals vs NonMusicians","Amateurs vs NonMusicians",
          "dprime","Response Accuracy", "Criterion","unsure Responses","DontKnow Responses")

# save output to textfile
capture.output(info[1], tim,o2_tim, info[2], MA_tim, d_MA_tim, info[3],MC_tim, d_MC_tim, info[4], AC_tim, d_AC_tim, 
               info[5],info[2],MA_timbreI,d_MA_timbreI, info[3],MC_timbreI,d_MC_timbreI, info[4],AC_timbreI,d_AC_timbreI,
               info[6],info[2],MA_timbreII,d_MA_timbreII, info[3],MC_timbreII,d_MC_timbreII, info[4],AC_timbreII,d_AC_timbreII,
               info[7],info[2],MA_timbreIII,d_MA_timbreIII, info[3],MC_timbreIII,d_MC_timbreIII, info[4],AC_timbreIII,d_AC_timbreIII,
               info[8],info[2],MA_timbreIV,d_MA_timbreIV, info[3],MC_timbreIV,d_MC_timbreIV, info[4],AC_timbreIV,d_AC_timbreIV,
               info[9],info[2],MA_timbreV,d_MA_timbreV, info[3],MC_timbreV,d_MC_timbreV, info[4],AC_timbreV,d_AC_timbreV,
               file="output/musicality/allgroups_PROMS_timbre.txt")

#remove objects to keep environment tidy
rm(info,tim, o2_tim,timbre, MA_tim, d_MA_tim,MC_tim, d_MC_tim, AC_tim, d_AC_tim, 
   MA_timbreI,d_MA_timbreI, MC_timbreI,d_MC_timbreI, AC_timbreI,d_AC_timbreI,
   MA_timbreII,d_MA_timbreII, MC_timbreII,d_MC_timbreII, AC_timbreII,d_AC_timbreII,
   MA_timbreIII,d_MA_timbreIII, MC_timbreIII,d_MC_timbreIII, AC_timbreIII,d_AC_timbreIII,
   MA_timbreIV,d_MA_timbreIV, MC_timbreIV,d_MC_timbreIV, AC_timbreIV,d_AC_timbreIV,
   MA_timbreV,d_MA_timbreV, MC_timbreV,d_MC_timbreV, AC_timbreV,d_AC_timbreV)

rm(ANOVAModel)


# save the prepared dataset for further analyses
PROMS <- B
save(PROMS, file="input/allgroups_PROMS_prepared.RData")

# End of Script

