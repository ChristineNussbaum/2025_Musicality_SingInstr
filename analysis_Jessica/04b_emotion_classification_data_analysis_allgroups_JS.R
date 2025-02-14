##########################################################################
## File: 04b_emotion_classification_data_analysis_allgroups_JS.R
## This script analysis the emotion classification performance of professionals,
##    amateurs and non-musicians
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
source("functions/tracedEZout.R")
source("functions/mySummary.R") 

# set some relevant setting 
options(scipen = 999) # supresses the e^-notation

#---------------------------------------------------------------------------------
#get the preprocessed data: 

#Experiment data (after some preprocessing, e.g. removing participants with too many omissions)
load(file="input/amateurs_Exp_processed.RData")
load(file="input/musicians vs nonmusicians/Exp_processed.RData")

## merging data sets
# # make amateurs into one group ("A")
D2$Group[D2$Group=="2"] <-"A"
D2$Group[D2$Group=="1"] <-"A"
D2 <- D2 %>% mutate(Group = as.factor(Group))

# merge 
D <- rbind(D, D2)

# change group of participant with degree in music science to musician
D$Group[D$Code=="AEB56L"] <- "M" # instrumentalists AEB56L

save(D, file="input/allgroups_Exp_processed.RData")

rm(D2)


# each participant (N = 166) completed 312 trials, resulting in a dataset with 51792 rows


## Meaning of Variables

# Subject                       individual participant code assigned by PsyToolkit
# Code                          self-generated Code by the participant
# Group                         group - M = Professionals; C = Non-Musicians; A = Amateurs 
# SpId                          speaker ID
# Emo                           speaker emotion; hap = happiness, ple = pleasure, fea = fear, sad = sadness, avg = average
# Word                          speaker pseudoword, w01 = /belam/, w02 = /molen/,   w05 = /loman/
# MType                         speaker morph type, full = full morphing, f0 = F0 morph, tbr = timbre morph 
# SpSex                         speaker sex, f = female, m = male
# filename                      stimulus' filename
# CB                            counterbalancing condition (assignment of response keys to emotion - refer to supplemental tables S5)
# ACC                           accuracy: 0 = incorrect, 1 = correct
# Resp                          response (keypress -> emotion response depends on CB condition)
# RT                            reaction time (5000 corresponds to omission)


#---------------------------------------------------------------------------------------------#
#          Extract descriptive Information about emotion perception experiment                #
#---------------------------------------------------------------------------------------------#


#extract distribution of counterbalancing conditions
CB <- D[, c(2,3,10)]
CB <- unique(CB)

# capture output:
capture.output(table(CB$CB, CB$Group), file="output/sample_description/counterbalancing_allgroups.txt")
rm(CB)


#missing analysis
missings <- D %>% filter(RT >= 5000)
missings <- missings %>% group_by(Code, Group) %>% summarise(N = length(RT))
missings$Percent <- round((missings$N/312 *100), 1)
capture.output(as.matrix(missings), file="output/emotion_classification/allgroups_missings.txt")
rm(missings)

#---------------------------------------------------------------------------------#
#                   Analysis of Emotion Perception Accuracy                       #
#---------------------------------------------------------------------------------#

#extract emotion perception accuracy

#remove average trials for this analysis
D <- D %>% filter(Emo != "avg")



##############################################################################
########                  Statistical analysis                   #############
##############################################################################

### aggregate data for ANOVA
D <- D %>% group_by(Subject, Group, Emo, MType) %>% summarise(ACC = mean(ACC))

#166 * 4 *3 = 1992

### Define variables as appropriate data class and drop levels of factors 
D$Subject <- as.factor(as.character(D$Subject))
D$MType <- as.factor(as.character(D$MType))
D$Emo <- as.factor(as.character(D$Emo))
D$Group <- as.factor(as.character(D$Group))
D <-  droplevels(D)

#####################
###### ANOVA ########
# data = D
# dv = ACC
# wid = Subject 
# within = emotion, mType
# between = Group

a <- ezANOVA(data=D, dv=.(ACC), wid=.(Subject), within = .(Emo, MType), between = .(Group), type=3, detailed = TRUE)
b = tracedEzOut(a, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")
#this analysis return a warning about unequal groups, since there are 39 musicians but 38 non-musicians and 89 amateurs

####################################################################
#expected output (reduced):
#           Effect                                       Text
#1     (Intercept)  F(1, 163) = 14334.410, p < .001, np2 = .989
#2           Group  F(2, 163) =     1.958, p = .144, np2 = .023
#3             Emo  F(3, 489) =   130.240, p < .001, np2 = .444
#4           MType  F(2, 326) =  1357.803, p < .001, np2 = .893
#5       Group:Emo  F(6, 489) =     1.168, p = .322, np2 = .014
#6     Group:MType  F(4, 326) =     2.142, p = .089, np2 = .026
#7       Emo:MType  F(6, 978) =    40.949, p < .001, np2 = .201
#8 Group:Emo:MType F(12, 978) =     0.744, p = .688, np2 < .019
####################################################################

### calculate omega effect sizes: 
o2_emo <- F_to_omega2(unlist(c(a$ANOVA[6])),  unlist(c(a$ANOVA[2])),  unlist(c(a$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2

# save output to textfile
capture.output(b, o2_emo, file = "output/emotion_classification/allgroups_A_Behavior_ANOVA_I.txt")

rm(a,b, o2_emo)

##############################################################################
########                   Post-Hoc Analysis                     #############
##############################################################################


################
###Main effects


###############  PH1: Main effect of Group  ###############
#reaggreate dataset
PH1 <- D %>% group_by(Subject,Group) %>% summarise(ACC=mean(ACC))

#extract descriptive data
PH1_descriptive <- mySummary(PH1, ACC, Group)

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA <- PH1 %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., ACC ~ Group, paired=FALSE)
d_MA <- t_to_d(MA$statistic, MA$parameter, paired = FALSE)

MC <- PH1 %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., ACC ~ Group, paired=FALSE)
d_MC <- t_to_d(MC$statistic, MC$parameter, paired = FALSE)

AC <- PH1 %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., ACC ~ Group, paired=FALSE)
d_AC <- t_to_d(AC$statistic, AC$parameter, paired = FALSE)

#add descriptions for output:
info <- c("Professionals vs Non-Musicians vs Amateurs")

# save output to textfile
capture.output(info, as.matrix(PH1_descriptive), MA, d_MA, MC, d_MC, AC, d_AC, 
               file="output/emotion_classification/allgroups_A_PH1_ME_group.txt")

#remove objects to keep environment tidy
rm(info, PH1_descriptive, PH1, MA, d_MA, MC, d_MC, AC, d_AC)

#####################################################################
#expected output (reduced)
# Musicians vs NonMusicians
# t = -2.3406, df = 69.892, p-value = 0.02211
# Musicians vs Amateurs
# t = -0.71937, df = 115.16, p-value = 0.4734
# Amateurs vs NonMusicians
# t = 1.6304, df = 88.477, p-value = 0.1066
# Descriptive Data
#     Group         ACC         SD     N    SE            CI          
#[1,] "A"   "0.5937836" "0.06759030" "88" "0.007205151" "0.01432102"
#[2,] "C"   "0.5754751" "0.05309444" "38" "0.008613056" "0.01745171"
#[3,] "M"   "0.6007814" "0.04133495" "40" "0.006535629" "0.01321956"
#####################################################################


###############  PH2: Main effect of emotion  ###############

#reaggreate dataset
PH2 <- D %>% group_by(Subject,Emo) %>% summarise(ACC=mean(ACC))

#extract descriptive data
PH2_descriptive <- mySummary(PH2, ACC, Emo)

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/6 = .008
HapPle <- PH2 %>% filter(Emo == "hap" | Emo == "ple") %>% t.test(data = ., ACC ~ Emo, paired=TRUE)
d_HapPle <- t_to_d(HapPle$statistic, HapPle$parameter, paired = TRUE)
HapFea <- PH2 %>% filter(Emo == "hap" | Emo == "fea") %>% t.test(data = ., ACC ~ Emo, paired=TRUE)
d_HapFea <- t_to_d(HapFea$statistic, HapFea$parameter, paired = TRUE)
HapSad <- PH2 %>% filter(Emo == "hap" | Emo == "sad") %>% t.test(data = ., ACC ~ Emo, paired=TRUE)
d_HapSad <- t_to_d(HapSad$statistic, HapSad$parameter, paired = TRUE)
PleFea <- PH2 %>% filter(Emo == "ple" | Emo == "fea") %>% t.test(data = ., ACC ~ Emo, paired=TRUE)
d_PleFea <- t_to_d(PleFea$statistic, PleFea$parameter, paired = TRUE)
PleSad <- PH2 %>% filter(Emo == "ple" | Emo == "sad") %>% t.test(data = ., ACC ~ Emo, paired=TRUE)
d_PleSad <- t_to_d(PleSad$statistic, PleSad$parameter, paired = TRUE)
FeaSad <- PH2 %>% filter(Emo == "fea" | Emo == "sad") %>% t.test(data = ., ACC ~ Emo, paired=TRUE)
d_FeaSad <- t_to_d(FeaSad$statistic, FeaSad$parameter, paired = TRUE)

#add descriptions for output:
info <- c("Hap vs Ple", "Hap vs Fea", "Hap vs Sad", "Ple vs Fea", "Ple vs Sad", "Fea vs Sad",  "Descriptive Data")

# save output to textfile
capture.output(info[1], HapPle, d_HapPle, info[2],HapFea, d_HapFea, info[3], HapSad, d_HapSad, info[4], PleFea, d_PleFea, info[5], PleSad, d_PleSad, info[6], FeaSad, d_FeaSad, info[7], as.matrix(PH2_descriptive), 
               file="output/emotion_classification/allgroups_A_PH2_ME_Emo.txt")

#remove objects to keep environment tidy
rm(info, HapPle, HapFea, HapSad, PleFea, PleSad, FeaSad,PH2_descriptive, PH2,
   d_HapPle, d_HapFea, d_HapSad, d_PleFea, d_PleSad, d_FeaSad)


#########################################################################
#expected output (reduced)
# [1] "Hap vs Ple"
# t = 18.52, df = 165, p-value < 0.001
# [1] "Hap vs Fea"
# t = -17.54, df = 165, p-value < 0.001
# [1] "Hap vs Sad"
# t = 9.2972, df = 165, p-value < 0.001
# [1] "Ple vs Fea"
# t = 0.71572, df = 165, p-value = 0.4752
# [1] "Ple vs Sad"
# t = -8.6342, df = 165, p-value < 0.001
# [1] "Fea vs Sad"
# t = -8.1092, df = 165, p-value < 0.001
# [1] "Descriptive Data"
#      Emo   ACC          SD          N    SE           CI          
#[1,] "fea" "0.5107682" "0.1199924" "166" "0.009313214" "0.01838843"
#[2,] "hap" "0.7351721" "0.1057001" "166" "0.008203917" "0.01619819"
#[3,] "ple" "0.5020891" "0.1172179" "166" "0.009097871" "0.01796325"
#[4,] "sad" "0.6170855" "0.1243322" "166" "0.009650052" "0.01905350"
######################################################################


###############  PH3: Main effect of Morph-Type  ###############

#reaggreate dataset
PH3 <- D %>% group_by(Subject, MType) %>% summarise(ACC=mean(ACC))

#extract descriptive data
PH3_descriptive <- mySummary(PH3, ACC, MType)

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
F0Tbr   <- PH3 %>% filter(MType == "f0" | MType == "tbr") %>% t.test(data = ., ACC ~ MType, paired=TRUE)
d_F0Tbr <- t_to_d(F0Tbr$statistic, F0Tbr$parameter, paired = TRUE)
TbrFull <- PH3 %>% filter(MType == "tbr" | MType == "full") %>% t.test(data = ., ACC ~ MType, paired=TRUE)
d_TbrFull <- t_to_d(TbrFull$statistic, TbrFull$parameter, paired = TRUE)
F0Full  <- PH3 %>% filter(MType == "f0" | MType == "full") %>% t.test(data = ., ACC ~ MType, paired=TRUE)
d_F0Full <- t_to_d(F0Full$statistic, F0Full$parameter, paired = TRUE)

#add descriptions for output:
info <- c("F0 vs Tbr","Tbr vs Full"," F0 vs Full","Descriptive Data")

# save output to textfile
capture.output(info[1], F0Tbr, d_F0Tbr, info[2],TbrFull, d_TbrFull, info[3], F0Full, d_F0Full, info[4], as.matrix(PH3_descriptive), 
               file="output/emotion_classification/allgroups_A_PH3_ME_MType.txt")

#remove objects to keep environment tidy
rm(info, F0Tbr,TbrFull, F0Full, PH3_descriptive, PH3, d_F0Tbr, d_TbrFull, d_F0Full)


##########################################################################
#expected output (reduced)
# [1] "F0 vs Tbr"
# t = 31.001, df = 165, p-value < 0.001
# [1] "Tbr vs Full"
# t = 49.602, df = 165, p-value < 0.001
# [1] " F0 vs Full"
# t = -29.133, df = 165, p-value < 0.001
#[1] "Descriptive Data"
#      MType  ACC          SD           N    SE            CI          
#[1,] "f0"   "0.6178620" "0.07677508" "166" "0.005958902" "0.011765528"
#[2,] "full" "0.7445603" "0.08085426" "166" "0.006275508" "0.012390649"
#[3,] "tbr"  "0.4114138" "0.06440062" "166" "0.004998457" "0.009869182"
#########################################################################


################
###Interactions


###############  PH4: Interaction of Morph-Type and Group  ###############

#->the effect of Group is tested for each Morph-Type separately

#reaggreate dataset
PH4 <- D %>% group_by(Subject, MType, Group) %>% summarise(ACC=mean(ACC))

#extract descriptive data
PH4_descriptive <- mySummary(PH4, ACC, MType, Group)
PH4_descriptive

############## descriptive data  ##################
#  MType Group   ACC     SD     N      SE     CI
#1 f0    A     0.621 0.0864    88 0.00921 0.0183
#2 f0    C     0.597 0.0719    38 0.0117  0.0236
#3 f0    M     0.631 0.0525    40 0.00830 0.0168
#4 full  A     0.748 0.0886    88 0.00944 0.0188
#5 full  C     0.717 0.0731    38 0.0119  0.0240
#6 full  M     0.762 0.0635    40 0.0100  0.0203
#7 tbr   A     0.412 0.0703    88 0.00750 0.0149
#8 tbr   C     0.412 0.0564    38 0.00915 0.0185
#9 tbr   M     0.409 0.0590    40 0.00933 0.0189
###################################################

capture.output(as.matrix(PH4_descriptive), 
               file = "output/emotion_classification/allgroups_A_PH4_Int_MType_Group_descriptive_data.txt")


############## MType "Full" only: ######################################

PH4full <- PH4 %>% filter(MType == "full")

#####################
###### ANOVA ########
# data = PH4full
# dv = ACC
# wid = Subject 
# between = Group

ANOVAModel <- ezANOVA(data=PH4full, wid=Subject, dv=ACC, between= Group, type =3, detailed=TRUE)
full = tracedEzOut(ANOVAModel, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")
#this analysis return a warning about unequal groups, since there are 39 musicians but 38 non-musicians and 89 amateurs

##########################################################
#expected output 
#--- FORMATTED RESULTS ------------------------------------`
#       Effect                                       Text
#1 (Intercept) F(1, 163) = 12444.756, p < .001, np2 = .987
#2       Group F(2, 163) =     3.267, p = .041, np2 = .039
###########################################################


# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA <- PH4full %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., ACC ~ Group, paired=FALSE)
d_MA <- t_to_d(MA$statistic, MA$parameter, paired = FALSE)
MC <- PH4full %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., ACC ~ Group, paired=FALSE)
d_MC <- t_to_d(MC$statistic, MC$parameter, paired = FALSE)
AC <- PH4full %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., ACC ~ Group, paired=FALSE)
d_AC <- t_to_d(AC$statistic, AC$parameter, paired = FALSE)

##############################################
# expected output (reduced)
#[1] "Musicians vs Amateurs"
# t = -0.99428, df = 102.48, p-value = 0.3224
# [1] "Musicians vs NonMusicians"
# t = -2.8754, df = 73.341, p-value = 0.005279
# [1] "Amateurs vs NonMusicians"
# t = 2.0438, df = 84.384, p-value = 0.0441
###############################################

#add descriptions for output:
info <- c("ANOVA on Group for full-MorphType only","Professionals vs Amateurs",
          "Professionals vs NonMusicians","Amateurs vs NonMusicians")

# save output to textfile
capture.output(info[1], full, info[2], MA, d_MA, info[3],MC, d_MC, info[4], AC, d_AC, 
               file="output/emotion_classification/allgroups_A_PH4_Int_aMType_aMGroup_full.txt")

#remove objects to keep environment tidy
rm(info,full, MA, d_MA,MC,d_MC,AC,d_AC, PH4full, ANOVAModel)


############## MType "F0" only: ######################################

PH4f0 <- PH4 %>% filter(MType == "f0")

#####################
###### ANOVA ########
# data = PH4full
# dv = ACC
# wid = Subject 
# between = Group

ANOVAModel <- ezANOVA(data=PH4f0, wid=Subject, dv=ACC, between= Group, type =3, detailed=TRUE)
f0 = tracedEzOut(ANOVAModel, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")
#this analysis return a warning about unequal groups, since there are 39 musicians but 38 non-musicians and 89 amateurs

#########################################################
#expected output 
#--- FORMATTED RESULTS ------------------------------------`
#       Effect                                       Text
#1 (Intercept) F(1, 163) = 9374.384, p < .001, np2 = .983
#2       Group F(2, 163) =    2.108, p = .125, np2 = .025
##########################################################

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA <- PH4f0 %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., ACC ~ Group, paired=FALSE)
d_MA <- t_to_d(MA$statistic, MA$parameter, paired = FALSE)
MC <- PH4f0 %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., ACC ~ Group, paired=FALSE)
d_MC <- t_to_d(MC$statistic, MC$parameter, paired = FALSE)
AC <- PH4f0 %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., ACC ~ Group, paired=FALSE)
d_AC <- t_to_d(AC$statistic, AC$parameter, paired = FALSE)

##############################################
# expected output (reduced)
#[1] "Musicians vs Amateurs"
# t = -0.78328, df = 115.65, p-value = 0.4351
# [1] "Musicians vs NonMusicians"
# t = -2.3784, df = 67.521, p-value = 0.02022
# [1] "Amateurs vs NonMusicians"
# t = 1.637, df = 83.718, p-value = 0.1054
###############################################

#add descriptions for output:
info <- c("ANOVA on Group for f0-MorphType only","Professionals vs Amateurs",
          "Professionals vs NonMusicians","Amateurs vs NonMusicians")

# save output to textfile
capture.output(info[1], f0, info[2], MA, d_MA, info[3],MC, d_MC, info[4], AC, d_AC, 
               file="output/emotion_classification/allgroups_A_PH4_Int_aMType_aMGroup_f0.txt")

#remove objects to keep environment tidy
rm(info,f0, MA, d_MA,MC,d_MC,AC,d_AC, PH4f0, ANOVAModel)

############## MType "Timbre" only: ######################################

PH4tbr <- PH4 %>% filter(MType == "tbr")

#####################
###### ANOVA ########
# data = PH4full
# dv = ACC
# wid = Subject 
# between = Group

ANOVAModel <- ezANOVA(data=PH4tbr, wid=Subject, dv=ACC, between= Group, type =3, detailed=TRUE)
tbr = tracedEzOut(ANOVAModel, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")
#this analysis return a warning about unequal groups, since there are 39 musicians but 38 non-musicians and 89 amateurs

#########################################################
#expected output 
#--- FORMATTED RESULTS ------------------------------------`
#       Effect                                       Text
#1(Intercept) F(1, 163) = 5785.403, p < .001, np2 = .973
#2       Group F(2, 163) =    0.024, p = .977, np2 < .010
##########################################################

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA <- PH4tbr %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., ACC ~ Group, paired=FALSE)
d_MA <- t_to_d(MA$statistic, MA$parameter, paired = FALSE)
MC <- PH4tbr %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., ACC ~ Group, paired=FALSE)
d_MC <- t_to_d(MC$statistic, MC$parameter, paired = FALSE)
AC <- PH4tbr %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., ACC ~ Group, paired=FALSE)
d_AC <- t_to_d(AC$statistic, AC$parameter, paired = FALSE)

##############################################
# expected output (reduced)
# [1] "Musicians vs Amateurs"
# t = 0.20222, df = 88.998, p-value = 0.8402
# [1] "Musicians vs NonMusicians"
#t = 0.21335, df = 75.996, p-value = 0.8316
# [1] "Amateurs vs NonMusicians"
# t = -0.031078, df = 86.736, p-value = 0.9753
###############################################

#add descriptions for output:
info <- c("ANOVA on Group for timbre-MorphType only","Professionals vs Amateurs",
          "Professionals vs NonMusicians","Amateurs vs NonMusicians")

# save output to textfile
capture.output(info[1], tbr, info[2], MA, d_MA, info[3],MC, d_MC, info[4], AC, d_AC, 
               file="output/emotion_classification/allgroups_A_PH4_Int_aMType_aMGroup_tbr.txt")

#remove objects to keep environment tidy
rm(info,tbr, MA, d_MA,MC,d_MC,AC,d_AC, PH4tbr)

rm(PH4, PH4_descriptive,ANOVAModel)


###############  PH5:  Interaction of Emotion and Morph Type  ###############

#-> effect of Morph Type is tested for each emotion separately

#reaggreate dataset
PH5 <- D %>% group_by(Subject, Emo, MType) %>% summarise(ACC=mean(ACC))

#extract descriptive data
PH5_descriptive <- mySummary(PH5, ACC, Emo, MType)
PH5_descriptive

############## descriptive data  ##################
#   Emo   MType   ACC    SD      N     SE      CI
#1  fea   f0    0.550 0.148    166 0.0115  0.0227
#2  fea   full  0.653 0.138    166 0.0107  0.0211
#3  fea   tbr   0.329 0.155    166 0.0120  0.0237
#4  hap   f0    0.800 0.136    166 0.0106  0.0209
#5  hap   full  0.937 0.0720   166 0.00559 0.0110
#6  hap   tbr   0.468 0.202    166 0.0156  0.0309
#7  ple   f0    0.469 0.138    166 0.0107  0.0211
#8  ple   full  0.666 0.147    166 0.0114  0.0225
#9  ple   tbr   0.371 0.149    166 0.0115  0.0228
#10 sad   f0    0.652 0.154    166 0.0119  0.0235
#11 sad   full  0.723 0.153    166 0.0119  0.0235
#12 sad   tbr   0.477 0.157    166 0.0122  0.0240
###################################################

capture.output(as.matrix(PH5_descriptive), 
               file = "output/emotion_classification/allgroups_A_PH5_Int_emo_MType_descriptive_data.txt")

############## Happy Stimuli only: ######################################

PH5hap <- PH5 %>% filter(Emo == "hap")

#####################
###### ANOVA ########
# data = PH5hap
# dv = ACC
# wid = Subject 
# within = MType
ANOVA_hap<-ezANOVA(data=PH5hap, wid=Subject, dv=ACC, within= MType, type =3, detailed=TRUE)
hap = tracedEzOut(ANOVA_hap, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

o2_hap <- F_to_omega2(unlist(c(ANOVA_hap$ANOVA[6])),  unlist(c(ANOVA_hap$ANOVA[2])),  unlist(c(ANOVA_hap$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2

#########################################################
#expected output 
#--- FORMATTED RESULTS ------------------------------------`
#       Effect                                       Text
#1 (Intercept) F(1, 165) = 8030.369, p < .001, np2 = .980
#2       MType F(2, 330) =  624.133, p < .001, np2 = .791
##########################################################

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
F0Tbr   <- PH5hap %>% filter(MType == "f0" | MType == "tbr") %>% t.test(data = ., ACC ~ MType, paired=TRUE)
d_F0Tbr <- t_to_d(F0Tbr$statistic, F0Tbr$parameter, paired = TRUE)
TbrFull <- PH5hap %>% filter(MType == "tbr" | MType == "full") %>% t.test(data = ., ACC ~ MType, paired=TRUE)
d_TbrFull <- t_to_d(TbrFull$statistic, TbrFull$parameter, paired = TRUE)
F0Full  <- PH5hap %>% filter(MType == "f0" | MType == "full") %>% t.test(data = ., ACC ~ MType, paired=TRUE)
d_F0Full <- t_to_d(F0Full$statistic, F0Full$parameter, paired = TRUE)


##############################################
# expected output (reduced)
# [1] "F0 vs Tbr"
# t = 21.05, df = 165, p-value < 0.001
# [1] "Tbr vs Full"
# t = 30.651, df = 165, p-value < 0.001
# [1] " F0 vs Full"
# t = -15.676, df = 165, p-value < 0.001
###############################################

#add descriptions for output:
info <- c("ANOVA on MType for happy stimuli only","F0 vs Tbr","Tbr vs Full"," F0 vs Full")

# save output to textfile
capture.output(info[1], hap, o2_hap, info[2], F0Tbr, d_F0Tbr, info[3],TbrFull, d_TbrFull, info[4], F0Full, d_F0Full, 
               file="output/emotion_classification/allgroups_A_PH5_Int_aEmo_aMType_hap.txt")

#remove objects to keep environment tidy
rm(info,hap,o2_hap, F0Tbr,TbrFull, F0Full, d_F0Tbr, d_TbrFull, d_F0Full, PH5hap)


# ############## Pleasure Stimuli only: ######################################

PH5ple <- PH5 %>% filter(Emo == "ple")

#####################
###### ANOVA ########
# data = PH5ple
# dv = ACC
# wid = Subject
# within = MType
ANOVA_ple<-ezANOVA(data=PH5ple, wid=Subject, dv=ACC, within= MType, type=3, detailed=TRUE)
ple = tracedEzOut(ANOVA_ple, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

o2_ple <- F_to_omega2(unlist(c(ANOVA_ple$ANOVA[6])),  unlist(c(ANOVA_ple$ANOVA[2])),  unlist(c(ANOVA_ple$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2

###########################################################
#expected output
# --- FORMATTED RESULTS ------------------------------------`
#       Effect                                       Text
#1 (Intercept) F(1, 165) = 3045.664, p < .001, np2 = .949
#2       MType F(2, 330) =  348.494, p < .001, np2 = .679
############################################################

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
F0Tbr   <- PH5ple %>% filter(MType == "f0" | MType == "tbr") %>% t.test(data = ., ACC ~ MType, paired=TRUE)
d_F0Tbr <- t_to_d(F0Tbr$statistic, F0Tbr$parameter, paired = TRUE)
TbrFull <- PH5ple %>% filter(MType == "tbr" | MType == "full") %>% t.test(data = ., ACC ~ MType, paired=TRUE)
d_TbrFull <- t_to_d(TbrFull$statistic, TbrFull$parameter, paired = TRUE)
F0Full  <- PH5ple %>% filter(MType == "f0" | MType == "full") %>% t.test(data = ., ACC ~ MType, paired=TRUE)
d_F0Full <- t_to_d(F0Full$statistic, F0Full$parameter, paired = TRUE)

# ##############################################
# expected output
# [1] "F0 vs Tbr"
# t = 8.4334, df = 165, p-value = 0.001
# [1] "Tbr vs Full"
# t = 24.586, df = 165, p-value < 0.001
# [1] " F0 vs Full"
# t = -18.842, df = 165, p-value < 0.001
# ###############################################


#add descriptions for output:
info <- c("ANOVA on MType for pleasure stimuli only","F0 vs Tbr","Tbr vs Full"," F0 vs Full")

# save output to textfile
capture.output(info[1], ple,o2_ple, info[2], F0Tbr, d_F0Tbr, info[3],TbrFull, d_TbrFull, info[4], F0Full, d_F0Full, 
               file="output/emotion_classification/allgroups_A_PH5_Int_aEmo_aMType_ple.txt")

#remove objects to keep environment tidy
rm(info,ple,o2_ple, F0Tbr,TbrFull, F0Full, d_F0Tbr, d_TbrFull, d_F0Full, PH5ple)


# ############## Fear Stimuli only: ######################################

PH5fea <- PH5 %>% filter(Emo == "fea")

#####################
###### ANOVA ########
# data = PH5fea
# dv = ACC
# wid = Subject
# within = MType
ANOVA_fea<-ezANOVA(data=PH5fea, wid=Subject, dv=ACC, within= MType, type=3, detailed=TRUE)
fea = tracedEzOut(ANOVA_fea, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

o2_fea <- F_to_omega2(unlist(c(ANOVA_fea$ANOVA[6])),  unlist(c(ANOVA_fea$ANOVA[2])),  unlist(c(ANOVA_fea$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2

############################################################
#expected output
# --- FORMATTED RESULTS ------------------------------------`
#       Effect                                       Text
#1 (Intercept) F(1, 165) = 3007.797, p < .001, np2 = .948
#2       MType F(2, 330) =  420.655, p < .001, np2 = .718
############################################################

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
F0Tbr   <- PH5fea %>% filter(MType == "f0" | MType == "tbr") %>% t.test(data = ., ACC ~ MType, paired=TRUE)
d_F0Tbr <- t_to_d(F0Tbr$statistic, F0Tbr$parameter, paired = TRUE)
TbrFull <- PH5fea %>% filter(MType == "tbr" | MType == "full") %>% t.test(data = ., ACC ~ MType, paired=TRUE)
d_TbrFull <- t_to_d(TbrFull$statistic, TbrFull$parameter, paired = TRUE)
F0Full  <- PH5fea %>% filter(MType == "f0" | MType == "full") %>% t.test(data = ., ACC ~ MType, paired=TRUE)
d_F0Full <- t_to_d(F0Full$statistic, F0Full$parameter, paired = TRUE)

# ##############################################
# expected output
# [1] "F0 vs Tbr"
# t = 18.404, df = 165, p-value < 0.001
# [1] "F0 vs Tbr"
# t = 27.137, df = 165, p-value < 0.001
# [1] " F0 vs Full"
# t = -10.084, df = 165, p-value < 0.001
# ###############################################


#add descriptions for output:
info <- c("ANOVA on MType for fea stimuli only","F0 vs Tbr","Tbr vs Full"," F0 vs Full")

# save output to textfile
capture.output(info[1], fea, o2_fea, info[2], F0Tbr, d_F0Tbr, info[3],TbrFull, d_TbrFull, info[4], F0Full, d_F0Full, 
               file="output/emotion_classification/allgroups_A_PH5_Int_aEmo_aMType_fea.txt")

#remove objects to keep environment tidy
rm(info,fea, o2_fea,F0Tbr,TbrFull, F0Full, d_F0Tbr, d_TbrFull, d_F0Full, PH5fea)

############## Sad Stimuli only: ######################################

PH5sad <- PH5 %>% filter(Emo == "sad")

#####################
###### ANOVA ########
# data = PH5sad
# dv = ACC
# wid = Subject
# within = MType
ANOVA_sad<-ezANOVA(data=PH5sad, wid=Subject, dv=ACC, within= MType, type=3, detailed=TRUE)
sad = tracedEzOut(ANOVA_sad, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

o2_sad <- F_to_omega2(unlist(c(ANOVA_sad$ANOVA[6])),  unlist(c(ANOVA_sad$ANOVA[2])),  unlist(c(ANOVA_sad$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2

#########################################################
#expected output
# $`--- FORMATTED RESULTS ------------------------------`
#       Effect                                       Text
#1 (Intercept) F(1, 165) = 4089.135, p < .001, np2 = .961
#2       MType F(2, 330) =  210.634, p < .001, np2 = .561
#########################################################


# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
F0Tbr   <- PH5sad %>% filter(MType == "f0" | MType == "tbr") %>% t.test(data = ., ACC ~ MType, paired=TRUE)
d_F0Tbr <- t_to_d(F0Tbr$statistic, F0Tbr$parameter, paired = TRUE)
TbrFull <- PH5sad %>% filter(MType == "tbr" | MType == "full") %>% t.test(data = ., ACC ~ MType, paired=TRUE)
d_TbrFull <- t_to_d(TbrFull$statistic, TbrFull$parameter, paired = TRUE)
F0Full  <- PH5sad %>% filter(MType == "f0" | MType == "full") %>% t.test(data = ., ACC ~ MType, paired=TRUE)
d_F0Full <- t_to_d(F0Full$statistic, F0Full$parameter, paired = TRUE)

# ##############################################
# expected output
# [1] "F0 vs Tbr"
# t = 12.568, df = 165, p-value < 0.001
# [1] "Tbr vs Full"
# t = 18.551, df = 165, p-value < 0.00
# [1] " F0 vs Full"
# t = -7.6334, df = 165, p-value = 0.001
# ###############################################

#add descriptions for output:
info <- c("ANOVA on MType for sad stimuli only","F0 vs Tbr","Tbr vs Full"," F0 vs Full")

# save output to textfile
capture.output(info[1], sad,o2_sad, info[2], F0Tbr, d_F0Tbr, info[3],TbrFull, d_TbrFull, info[4], F0Full, d_F0Full, 
               file="output/emotion_classification/allgroups_A_PH5_Int_aEmo_aMType_sad.txt")

#remove objects to keep environment tidy
rm(info,sad,o2_sad, F0Tbr,TbrFull, F0Full, d_F0Tbr, d_TbrFull, d_F0Full, PH5sad)



########################   Specifically test the F0-vs Timbre-Difference in the four emotions ################

##convert PH5 partly in wide format

PH5 <- PH5 %>% pivot_wider(names_from = c("MType"), values_from = c("ACC"))

PH5$Diff <- PH5$f0 - PH5$tbr

#extract descriptive data
PH5_descriptive <- mySummary(PH5, Diff, Emo)

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/6 = .008
HapPle <- PH5 %>% filter(Emo == "hap" | Emo == "ple") %>% t.test(data = ., Diff ~ Emo, paired=TRUE)
d_HapPle <- t_to_d(HapPle$statistic, HapPle$parameter, paired = TRUE)
HapFea <- PH5 %>% filter(Emo == "hap" | Emo == "fea") %>% t.test(data = ., Diff ~ Emo, paired=TRUE)
d_HapFea <- t_to_d(HapFea$statistic, HapFea$parameter, paired = TRUE)
HapSad <- PH5 %>% filter(Emo == "hap" | Emo == "sad") %>% t.test(data = ., Diff ~ Emo, paired=TRUE)
d_HapSad <- t_to_d(HapSad$statistic, HapSad$parameter, paired = TRUE)
PleFea <- PH5 %>% filter(Emo == "ple" | Emo == "fea") %>% t.test(data = ., Diff ~ Emo, paired=TRUE)
d_PleFea <- t_to_d(PleFea$statistic, PleFea$parameter, paired = TRUE)
PleSad <- PH5 %>% filter(Emo == "ple" | Emo == "sad") %>% t.test(data = ., Diff ~ Emo, paired=TRUE)
d_PleSad <- t_to_d(PleSad$statistic, PleSad$parameter, paired = TRUE)
FeaSad <- PH5 %>% filter(Emo == "fea" | Emo == "sad") %>% t.test(data = ., Diff ~ Emo, paired=TRUE)
d_FeaSad <- t_to_d(FeaSad$statistic, FeaSad$parameter, paired = TRUE)

#add descriptions for output:
info <- c("Hap vs Ple", "Hap vs Fea", "Hap vs Sad", "Ple vs Fea", "Ple vs Sad", "Fea vs Sad",  "Descriptive Data")

# save output to textfile
capture.output(info[1], HapPle, d_HapPle, info[2],HapFea, d_HapFea, info[3], HapSad, d_HapSad, info[4], PleFea, d_PleFea, info[5], PleSad, d_PleSad, info[6], FeaSad, d_FeaSad, info[7], as.matrix(PH5_descriptive), 
               file="output/emotion_classification/allgroups_A_PH5_Diff_F0-Tbr.txt")

#remove objects to keep environment tidy
rm(info, HapPle, HapFea, HapSad, PleFea, PleSad, FeaSad,PH5_descriptive, PH5,
   d_HapPle, d_HapFea, d_HapSad, d_PleFea, d_PleSad, d_FeaSad)


###################################################################
#expected output (reduced)
# [1] "Hap vs Ple"
# t = 11.415, df = 165, p-value < 0.001
# [1] "Hap vs Fea"
# t = -5.5693, df = 165, p-value < 0.001
# [1] "Hap vs Sad"
# t = 8.0523, df = 165, p-value < 0.001
# [1] "Ple vs Fea"
# t = 7.4093, df = 165, p-value < 0.001
# [1] "Ple vs Sad"
# t = -3.9892, df = 165, p-value < 0.001
# [1] "Fea vs Sad"
# t = 2.5364, df = 165, p-value = 0.01213
# [1] "Descriptive Data"
#      Emo   Diff           SD          N    SE           CI          
#[1,] "fea" "0.22092664" "0.1546634" "166" "0.01200420" "0.02370165"
#[2,] "hap" "0.33204774" "0.2032325" "166" "0.01577390" "0.03114471"
#[3,] "ple" "0.09801335" "0.1497388" "166" "0.01162198" "0.02294697"
#[4,] "sad" "0.17480486" "0.1792016" "166" "0.01390874" "0.02746205"
####################################################################


rm(ANOVA_hap, ANOVA_ple, ANOVA_fea, ANOVA_sad, D)


### End of Script
