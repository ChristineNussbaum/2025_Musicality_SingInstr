##########################################################################
## File: 02a_emotion_classification_data_analysis_amateurs_partI.R
## This script analysis the emotion classification performance of musicians and non-musicians
# authors: Christine Nussbaum (christine.nussbaum@uni-jena.de), Jessica Dethloff
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


#---------------------------------------------------------------------------------------------#
#          Extract descriptive Information about emotion perception experiment                #
#---------------------------------------------------------------------------------------------#


#extract distribution of counterbalancing conditions
CB <- D[, c(2,3,10)]
CB <- unique(CB)

# capture output:
capture.output(table(CB$CB, CB$Group), file="output/sample_description/counterbalancing_amateurs.txt")
rm(CB)


#missing analysis
missings <- D %>% filter(RT == 5000)
missings <- missings %>% group_by(Code, Group) %>% summarise(N = length(RT))
missings$Percent <- round((missings$N/312 *100), 1)
capture.output(as.matrix(missings), file="output/emotion_classification/amateurs_missings.txt")
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

#88 * 4 *3 = 1056

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

a<-ezANOVA(data=D, dv=.(ACC), wid=.(Subject), within = .(Emo, MType), between = .(Group), type=3, detailed = TRUE)
b = tracedEzOut(a, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")
#this analysis return a warning about unequal groups, since there are 45 singers but 44 instrumentalists

####################################################################
#expected output (reduced):
#Effect                                       Text
#1     (Intercept)  F(1, 86) = 6737.023, p < .001, np2 = .987
#2           Group  F(1, 86) =    0.375, p = .542, np2 < .014
#3             Emo F(3, 258) =   72.430, p < .001, np2 = .457
#4           MType F(2, 172) =  768.932, p < .001, np2 = .899
#5       Group:Emo F(3, 258) =    2.143, p = .095, np2 = .024
#6     Group:MType F(2, 172) =    0.359, p = .635, np2 < .014
#7       Emo:MType F(6, 516) =   22.781, p < .001, np2 = .209
#8 Group:Emo:MType F(6, 516) =    1.334, p = .249, np2 = .015
####################################################################

### calculate omega effect sizes: 
o2_emo <- F_to_omega2(unlist(c(a$ANOVA[6])),  unlist(c(a$ANOVA[2])),  unlist(c(a$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2

# save output to textfile
capture.output(b, o2_emo, file = "output/emotion_classification/amateurs_A_Behavior_ANOVA_I.txt")

rm(a,b, o2_emo)

##############################################################################
########                   Post-Hoc Analysis                     #############
##############################################################################


################
###Main effects



###############  PH1: Main effect of emotion  ###############

#reaggreate dataset
PH1 <- D %>% group_by(Subject,Emo) %>% summarise(ACC=mean(ACC))

#extract descriptive data
PH1_descriptive <- mySummary(PH1, ACC, Emo)

#convert to wide format
PH1 <- PH1 %>% pivot_wider(names_from= Emo, values_from=ACC)

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/6 = .008
HapPle <- t.test(PH1$hap, PH1$ple, paired = TRUE) 
d_HapPle <- t_to_d(HapPle$statistic, HapPle$parameter, paired = TRUE)
HapFea <- t.test(PH1$hap, PH1$fea, paired = TRUE) 
d_HapFea <- t_to_d(HapFea$statistic, HapFea$parameter, paired = TRUE)
HapSad <- t.test(PH1$hap, PH1$sad, paired = TRUE) 
d_HapSad <- t_to_d(HapSad$statistic, HapSad$parameter, paired = TRUE)
PleFea <- t.test(PH1$ple, PH1$fea, paired = TRUE) 
d_PleFea <- t_to_d(PleFea$statistic, PleFea$parameter, paired = TRUE)
PleSad <- t.test(PH1$ple, PH1$sad, paired = TRUE) 
d_PleSad <- t_to_d(PleSad$statistic, PleSad$parameter, paired = TRUE)
FeaSad <- t.test(PH1$fea, PH1$sad, paired = TRUE) 
d_FeaSad <- t_to_d(FeaSad$statistic, FeaSad$parameter, paired = TRUE)

#add descriptions for output:
info <- c("Hap vs Ple", "Hap vs Fea", "Hap vs Sad", "Ple vs Fea", "Ple vs Sad", "Fea vs Sad",  "Descriptive Data")

# save output to textfile
capture.output(info[1], HapPle, d_HapPle, info[2],HapFea, d_HapFea, info[3], HapSad, d_HapSad, info[4], PleFea, d_PleFea, info[5], PleSad, d_PleSad, info[6], FeaSad, d_FeaSad, info[7], as.matrix(PH1_descriptive), 
               file="output/emotion_classification/amateurs_A_PH1_ME_Emo.txt")

#remove objects to keep environment tidy
rm(info, HapPle, HapFea, HapSad, PleFea, PleSad, FeaSad,PH1_descriptive, PH1,
   d_HapPle, d_HapFea, d_HapSad, d_PleFea, d_PleSad, d_FeaSad)


#########################################################################
#expected output (reduced)
# [1] "Hap vs Ple"
# t = 13.29, df = 87, p-value < 0.001
# [1] "Hap vs Fea"
# t = 11.721, df = 87, p-value p-value < 0.001
# [1] "Hap vs Sad"
# t = 5.9617, df = 87, p-value < 0.001
# [1] "Ple vs Fea"
# t = -1.1388, df = 87, p-value = 0.2579
# [1] "Ple vs Sad"
# t = -6.7928, df = 87, p-value
# [1] "Fea vs Sad"
# t = -6.1512, df = 87, p-value < 0.001
# [1] "Descriptive Data"
# Emo   ACC         SD          N    SE           CI          
# [1,] "fea" "0.5193454" "0.1260721" "88" "0.01343933" "0.02671212"
# [2,] "hap" "0.7296598" "0.1102260" "88" "0.01175013" "0.02335466"
# [3,] "ple" "0.4991427" "0.1289717" "88" "0.01374843" "0.02732649"
# [4,] "sad" "0.6269864" "0.1236003" "88" "0.01317584" "0.02618840"

######################################################################


###############  PH3: Main effect of Morph-Type  ###############

#reaggreate dataset
PH2 <- D %>% group_by(Subject, MType) %>% summarise(ACC=mean(ACC))

#extract descriptive data
PH2_descriptive <- mySummary(PH2, ACC, MType)

#convert to wide format
PH2 <- PH2 %>% pivot_wider(names_from= MType, values_from=ACC)


# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
F0Tbr   <- t.test(PH2$f0, PH2$tbr, paired = TRUE) 
d_F0Tbr <- t_to_d(F0Tbr$statistic, F0Tbr$parameter, paired = TRUE)
TbrFull <- t.test(PH2$tbr, PH2$full, paired = TRUE)
d_TbrFull <- t_to_d(TbrFull$statistic, TbrFull$parameter, paired = TRUE)
F0Full  <- t.test(PH2$f0, PH2$full, paired = TRUE)
d_F0Full <- t_to_d(F0Full$statistic, F0Full$parameter, paired = TRUE)

#add descriptions for output:
info <- c("F0 vs Tbr","Tbr vs Full"," F0 vs Full","Descriptive Data")

# save output to textfile
capture.output(info[1], F0Tbr, d_F0Tbr, info[2],TbrFull, d_TbrFull, info[3], F0Full, d_F0Full, info[4], as.matrix(PH2_descriptive), 
               file="output/emotion_classification/amateurs_A_PH2_ME_MType.txt")

#remove objects to keep environment tidy
rm(info, F0Tbr,TbrFull, F0Full, PH2_descriptive, PH2, d_F0Tbr, d_TbrFull, d_F0Full)


##########################################################################
#expected output (reduced)
# [1] "F0 vs Tbr"
# t = 21.443, df = 87, p-value < 0.001
# [1] "Tbr vs Full"
# t = -33.94, df = 87, p-value < 0.001
# [1] " F0 vs Full"
# t = -23.276, df = 87, p-value < 0.001
#[1] "Descriptive Data"
#     MType  ACC          SD           N    SE            CI          
# [1,] "f0"   "0.6210909" "0.08640563" "88" "0.009210871" "0.01830760"
# [2,] "full" "0.7483468" "0.08855097" "88" "0.009439565" "0.01876216"
# [3,] "tbr"  "0.4119130" "0.07034627" "88" "0.007498937" "0.01490495"
#########################################################################



################
###Interactions


###############  PH3:  Interaction of Emotion and Morph Type  ###############

#-> effect of Morph Type is tested for each emotion separately


#reaggreate dataset
PH3 <- D %>% group_by(Subject, Emo, MType) %>% summarise(ACC=mean(ACC))

#extract descriptive data
PH3_descriptive <- mySummary(PH3, ACC, Emo, MType)

############## descriptive data  ##################
#   Emo   MType ACC   SD     N      SE     CI
# 1 fea   f0    0.558 0.147     88 0.0157  0.0312
# 2 fea   full  0.656 0.145     88 0.0155  0.0308
# 3 fea   tbr   0.344 0.164     88 0.0175  0.0348
# 4 hap   f0    0.794 0.135     88 0.0144  0.0287
# 5 hap   full  0.941 0.0667    88 0.00711 0.0141
# 6 hap   tbr   0.455 0.222     88 0.0237  0.0470
# 7 ple   f0    0.472 0.158     88 0.0168  0.0335
# 8 ple   full  0.657 0.160     88 0.0170  0.0338
# 9 ple   tbr   0.368 0.156     88 0.0166  0.0330
# 10 sad   f0    0.661 0.160     88 0.0171  0.0339
# 11 sad   full  0.740 0.155     88 0.0165  0.0329
# 12 sad   tbr   0.480 0.163     88 0.0173  0.0345
###################################################

capture.output(as.matrix(PH3_descriptive), 
               file = "output/emotion_classification/amateurs_A_PH3_Int_emo_MType_descriptive_data.txt")

############## Happy Stimuli only: ######################################

PH3hap <- PH3 %>% filter(Emo == "hap")

#####################
###### ANOVA ########
# data = PH3hap
# dv = ACC
# wid = Subject 
# within = MType
ANOVAModel<-ezANOVA(data=PH3hap, wid=Subject, dv=ACC, within= MType, type =3, detailed=TRUE)
hap = tracedEzOut(ANOVAModel, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

#########################################################
#expected output 
#--- FORMATTED RESULTS ------------------------------------`
#       Effect                                       Text
# 1 (Intercept)  F(1, 87) = 3856.163, p < .001, np2 = .978
# 2       MType F(2, 174) =  306.793, p < .001, np2 = .779
##########################################################

#convert to wide format
PH3hap <- PH3hap %>% pivot_wider(names_from= MType, values_from=ACC)

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
F0Tbr   <- t.test(PH3hap$f0, PH3hap$tbr, paired = TRUE) 
d_F0Tbr <- t_to_d(F0Tbr$statistic, F0Tbr$parameter, paired = TRUE)
TbrFull <- t.test(PH3hap$tbr, PH3hap$full, paired = TRUE)
d_TbrFull <- t_to_d(TbrFull$statistic, TbrFull$parameter, paired = TRUE)
F0Full  <- t.test(PH3hap$f0, PH3hap$full, paired = TRUE)
d_F0Full <- t_to_d(F0Full$statistic, F0Full$parameter, paired = TRUE)


##############################################
# expected output (reduced)
#[1] "F0 vs Tbr"
# t = 14.485, df = 87, p-value < 0.001
# [1] "Tbr vs Full"
# t = -21.196, df = 87, p-value < 0.001
# [1] " F0 vs Full"
# t = -12.36, df = 87, p-value < 0.001
###############################################

#add descriptions for output:
info <- c("ANOVA on MType for happy stimuli only","F0 vs Tbr","Tbr vs Full"," F0 vs Full")

# save output to textfile
capture.output(info[1], hap, info[2], F0Tbr, d_F0Tbr, info[3],TbrFull, d_TbrFull, info[4], F0Full, d_F0Full, 
               file="output/emotion_classification/amateurs_A_PH3_Int_aEmo_aMType_hap.txt")

#remove objects to keep environment tidy
rm(info,hap, F0Tbr,TbrFull, F0Full, d_F0Tbr, d_TbrFull, d_F0Full, PH3hap)


# ############## Pleasure Stimuli only: ######################################

PH3ple <- PH3 %>% filter(Emo == "ple")

#####################
###### ANOVA ########
# data = PH3ple
# dv = ACC
# wid = Subject
# within = MType
ANOVAModel<-ezANOVA(data=PH3ple, wid=Subject, dv=ACC, within= MType, type=3, detailed=TRUE)
ple = tracedEzOut(ANOVAModel, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

###########################################################
#expected output
# --- FORMATTED RESULTS ------------------------------------`
#       Effect                                       Text
#1 (Intercept)  F(1, 88) = 1340.922, p < .001, np2 = .938
#2       MType F(2, 176) =  149.516, p < .001, np2 = .629
############################################################


#convert to wide format
PH3ple <- PH3ple %>% pivot_wider(names_from= MType, values_from=ACC)

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
F0Tbr   <- t.test(PH3ple$f0, PH3ple$tbr, paired = TRUE) 
d_F0Tbr <- t_to_d(F0Tbr$statistic, F0Tbr$parameter, paired = TRUE)
TbrFull <- t.test(PH3ple$tbr, PH3ple$full, paired = TRUE)
d_TbrFull <- t_to_d(TbrFull$statistic, TbrFull$parameter, paired = TRUE)
F0Full  <- t.test(PH3ple$f0, PH3ple$full, paired = TRUE)
d_F0Full <- t_to_d(F0Full$statistic, F0Full$parameter, paired = TRUE)


# ##############################################
# expected output
#[1] "F0 vs Tbr"
# t = 6.1163, df = 87, p-value < 0.001
# [1] "Tbr vs Full"
# t = -16.249, df = 87, p-value < 0.001
# [1] " F0 vs Full"
# t = -11.938, df = 87, p-value < 0.001
# ###############################################


#add descriptions for output:
info <- c("ANOVA on MType for pleasure stimuli only","F0 vs Tbr","Tbr vs Full"," F0 vs Full")

# save output to textfile
capture.output(info[1], ple, info[2], F0Tbr, d_F0Tbr, info[3],TbrFull, d_TbrFull, info[4], F0Full, d_F0Full, 
               file="output/emotion_classification/amateurs_A_PH3_Int_aEmo_aMType_ple.txt")

#remove objects to keep environment tidy
rm(info,ple, F0Tbr,TbrFull, F0Full, d_F0Tbr, d_TbrFull, d_F0Full, PH3ple)


# ############## Fear Stimuli only: ######################################

PH3fea <- PH3 %>% filter(Emo == "fea")

#####################
###### ANOVA ########
# data = PH3fea
# dv = ACC
# wid = Subject
# within = MType
ANOVAModel<-ezANOVA(data=PH3fea, wid=Subject, dv=ACC, within= MType, type=3, detailed=TRUE)
fea = tracedEzOut(ANOVAModel, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

############################################################
#expected output
# --- FORMATTED RESULTS ------------------------------------`
#       Effect                                       Text
# 1 (Intercept)  F(1, 87) = 1493.336, p < .001, np2 = .945
# 2       MType F(2, 174) =  202.375, p < .001, np2 = .699
############################################################


#convert to wide format
PH3fea <- PH3fea %>% pivot_wider(names_from= MType, values_from=ACC)

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
F0Tbr   <- t.test(PH3fea$f0, PH3fea$tbr, paired = TRUE) 
d_F0Tbr <- t_to_d(F0Tbr$statistic, F0Tbr$parameter, paired = TRUE)
TbrFull <- t.test(PH3fea$tbr, PH3fea$full, paired = TRUE)
d_TbrFull <- t_to_d(TbrFull$statistic, TbrFull$parameter, paired = TRUE)
F0Full  <- t.test(PH3fea$f0, PH3fea$full, paired = TRUE)
d_F0Full <- t_to_d(F0Full$statistic, F0Full$parameter, paired = TRUE)

# ##############################################
# expected output
#[1] "F0 vs Tbr"
# t = 12.737, df = 87,  p-value < 0.001
# [1] "Tbr vs Full"
# t = -18.364, df = 87, p-value < 0.001
# [1] " F0 vs Full"
# t = -7.2571, df = 87, p-value < 0.001
# ###############################################


#add descriptions for output:
info <- c("ANOVA on MType for fea stimuli only","F0 vs Tbr","Tbr vs Full"," F0 vs Full")

# save output to textfile
capture.output(info[1], fea, info[2], F0Tbr, d_F0Tbr, info[3],TbrFull, d_TbrFull, info[4], F0Full, d_F0Full, 
               file="output/emotion_classification/amateurs_A_PH3_Int_aEmo_aMType_fea.txt")

#remove objects to keep environment tidy
rm(info,fea, F0Tbr,TbrFull, F0Full, d_F0Tbr, d_TbrFull, d_F0Full, PH3fea)

############## Sad Stimuli only: ######################################

PH3sad <- PH3 %>% filter(Emo == "sad")

#####################
###### ANOVA ########
# data = PH3sad
# dv = ACC
# wid = Subject
# within = MType
ANOVAModel<-ezANOVA(data=PH3sad, wid=Subject, dv=ACC, within= MType, type=3, detailed=TRUE)
sad = tracedEzOut(ANOVAModel, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

#########################################################
#expected output
# $`--- FORMATTED RESULTS ------------------------------`
#       Effect                                       Text
# 1 (Intercept)  F(1, 87) = 2264.435, p < .001, np2 = .963
# 2       MType F(2, 174) =  102.435, p < .001, np2 = .541
#########################################################


#convert to wide format
PH3sad <- PH3sad %>% pivot_wider(names_from= MType, values_from=ACC)

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
F0Tbr   <- t.test(PH3sad$f0, PH3sad$tbr, paired = TRUE) 
d_F0Tbr <- t_to_d(F0Tbr$statistic, F0Tbr$parameter, paired = TRUE)
TbrFull <- t.test(PH3sad$tbr, PH3sad$full, paired = TRUE)
d_TbrFull <- t_to_d(TbrFull$statistic, TbrFull$parameter, paired = TRUE)
F0Full  <- t.test(PH3sad$f0, PH3sad$full, paired = TRUE)
d_F0Full <- t_to_d(F0Full$statistic, F0Full$parameter, paired = TRUE)

# ##############################################
# expected output
#[1] "F0 vs Tbr"
# t = 8.2367, df = 87, p-value < 0.001
# [1] "Tbr vs Full"
# t = -13.256, df = 87, p-value < 0.001
# [1] " F0 vs Full"
# t = -6.0035, df = 87, p-value < 0.001
# ###############################################

#add descriptions for output:
info <- c("ANOVA on MType for sad stimuli only","F0 vs Tbr","Tbr vs Full"," F0 vs Full")

# save output to textfile
capture.output(info[1], sad, info[2], F0Tbr, d_F0Tbr, info[3],TbrFull, d_TbrFull, info[4], F0Full, d_F0Full, 
               file="output/emotion_classification/amateurs_A_PH3_Int_aEmo_aMType_sad.txt")

#remove objects to keep environment tidy
rm(info,sad, F0Tbr,TbrFull, F0Full, d_F0Tbr, d_TbrFull, d_F0Full, PH3sad)




########################   Specifically test the F0-vs Timbre-Difference in the four emotions ################

##convert PH3 partly in wide format

PH3 <- PH3 %>% pivot_wider(names_from = c("MType"), values_from = c("ACC"))

PH3$Diff <- PH3$f0 - PH3$tbr

#extract descriptive data
PH3_descriptive <- mySummary(PH3, Diff, Emo)

#convert to wide format
PH3 <- PH3[,c(1,2,6)] %>% pivot_wider(names_from= Emo, values_from=Diff)

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/6 = .008
HapPle <- t.test(PH3$hap, PH3$ple, paired = TRUE) 
d_HapPle <- t_to_d(HapPle$statistic, HapPle$parameter, paired = TRUE)
HapFea <- t.test(PH3$hap, PH3$fea, paired = TRUE) 
d_HapFea <- t_to_d(HapFea$statistic, HapFea$parameter, paired = TRUE)
HapSad <- t.test(PH3$hap, PH3$sad, paired = TRUE) 
d_HapSad <- t_to_d(HapSad$statistic, HapSad$parameter, paired = TRUE)
PleFea <- t.test(PH3$ple, PH3$fea, paired = TRUE) 
d_PleFea <- t_to_d(PleFea$statistic, PleFea$parameter, paired = TRUE)
PleSad <- t.test(PH3$ple, PH3$sad, paired = TRUE) 
d_PleSad <- t_to_d(PleSad$statistic, PleSad$parameter, paired = TRUE)
FeaSad <- t.test(PH3$fea, PH3$sad, paired = TRUE) 
d_FeaSad <- t_to_d(FeaSad$statistic, FeaSad$parameter, paired = TRUE)

#add descriptions for output:
info <- c("Hap vs Ple", "Hap vs Fea", "Hap vs Sad", "Ple vs Fea", "Ple vs Sad", "Fea vs Sad",  "Descriptive Data")

# save output to textfile
capture.output(info[1], HapPle, d_HapPle, info[2],HapFea, d_HapFea, info[3], HapSad, d_HapSad, info[4], PleFea, d_PleFea, info[5], PleSad, d_PleSad, info[6], FeaSad, d_FeaSad, info[7], as.matrix(PH3_descriptive), 
               file="output/emotion_classification/amateurs_A_PH3_Diff_F0-Tbr.txt")

#remove objects to keep environment tidy
rm(info, HapPle, HapFea, HapSad, PleFea, PleSad, FeaSad,PH3_descriptive, PH3,
   d_HapPle, d_HapFea, d_HapSad, d_PleFea, d_PleSad, d_FeaSad)


#############################################
#expected output (reduced)
# [1] "Hap vs Ple"
# t = 8.017, df = 87, p-value < 0.001
# [1] "Hap vs Fea"
# t = 4.371, df = 87, p-value < 0.001
# [1] "Hap vs Sad"
# t = 5.0811, df = 87, p-value < 0.001
# [1] "Ple vs Fea"
# t = -4.8207, df = 87 p-value < 0.001
# [1] "Ple vs Sad"
# t = -2.5671, df = 87, p-value = 0.01196
# [1] "Fea vs Sad"
# t = 1.1316, df = 87, p-value = 0.2609
# [1] "Descriptive Data"
#      Emo   Diff        SD          N    SE           CI          
# [1,] "fea" "0.2130720" "0.1569306" "88" "0.01672885" "0.03325041"
# [2,] "hap" "0.3387928" "0.2194086" "88" "0.02338903" "0.04648823"
# [3,] "ple" "0.1044068" "0.1601335" "88" "0.01707029" "0.03392905"
# [4,] "sad" "0.1804400" "0.2055034" "88" "0.02190673" "0.04354201"

#############################################


rm(ANOVAModel, D)


### End of Script