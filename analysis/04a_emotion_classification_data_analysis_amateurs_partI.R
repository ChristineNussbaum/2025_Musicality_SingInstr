##########################################################################
## File: 04a_emotion_classification_data_analysis_amateurs.R
## This script analysis the emotion classification performance of musicians and non-musicians
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


###############  PH1: Main effect of Group  ###############

#reaggreate dataset
PH1 <- D %>% group_by(Subject,Group) %>% summarise(ACC=mean(ACC))

#extract descriptive data
PH1_descriptive <- mySummary(PH1, ACC, Group)

# perform pairwise t-tests
Group <- t.test(data = PH1, ACC ~ Group, paired=FALSE)
d_Group <- t_to_d(Group$statistic, Group$parameter, paired = FALSE)

#add descriptions for output:
info <- c("Singers vs Instrumentalists")

# save output to textfile
capture.output(info, Group, d_Group,  as.matrix(PH1_descriptive), 
               file="output/emotion_classification/amateurs_A_PH1_ME_group.txt")

#remove objects to keep environment tidy
rm(info, Group, d_Group, PH1_descriptive, PH1)

#############################################
#expected output (reduced)
#t = 0.61582, df = 82.953, p-value = 0.5397
#############################################

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
               file="output/emotion_classification/amateurs_A_PH2_ME_Emo.txt")

#remove objects to keep environment tidy
rm(info, HapPle, HapFea, HapSad, PleFea, PleSad, FeaSad,PH2_descriptive, PH2,
   d_HapPle, d_HapFea, d_HapSad, d_PleFea, d_PleSad, d_FeaSad)


#########################################################################
#expected output (reduced)
# [1] "Hap vs Ple"
# t = 13.478, df = 88, p-value < 0.001
# [1] "Hap vs Fea"
# t = -11.889, df = 88, p-value < 0.001
# [1] "Hap vs Sad"
# t = 6.1105, df = 88, p-value < 0.001
# [1] "Ple vs Fea"
# t = 1.189, df = 88, p-value = 0.2377
# [1] "Ple vs Sad"
# t = -6.8139, df = 88, p-value < 0.001
# [1] "Fea vs Sad"
# t = -6.1057, df = 88, p-value < 0.001
# [1] "Descriptive Data"
#     Emo   ACC          SD          N    SE           CI          
#[1,] "fea" "0.5193042" "0.1253543" "89" "0.01328753" "0.02640618"
#[2,] "hap" "0.7304583" "0.1098565" "89" "0.01164476" "0.02314152"
#[3,] "ple" "0.4984345" "0.1284108" "89" "0.01361151" "0.02705002"
#[4,] "sad" "0.6253755" "0.1238321" "89" "0.01312617" "0.02608551"

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
               file="output/emotion_classification/amateurs_A_PH3_ME_MType.txt")

#remove objects to keep environment tidy
rm(info, F0Tbr,TbrFull, F0Full, PH3_descriptive, PH3, d_F0Tbr, d_TbrFull, d_F0Full)


##########################################################################
#expected output (reduced)
# [1] "F0 vs Tbr"
# t = 21.685, df = 88, p-value < 0.001
# [1] "Tbr vs Full"
# t = 33.004, df = 88, p-value < 0.001
# [1] " F0 vs Full"
# t = -20.977, df = 88, p-value < 0.001
#[1] "Descriptive Data"
#     MType  ACC          SD           N    SE            CI          
#[1,] "f0"   "0.6215179" "0.08600766" "89" "0.009116793" "0.01811771"
#[2,] "full" "0.7462913" "0.09015655" "89" "0.009556575" "0.01899168"
#[3,] "tbr"  "0.4123702" "0.07007829" "89" "0.007428284" "0.01476215"
#########################################################################



################
###Interactions

#->the effect of Group is tested for each Morph-Type

###############  PH4: Interaction of Morph-Type and Group  ###############

#reaggreate dataset
PH4 <- D %>% group_by(Subject, MType, Group) %>% summarise(ACC=mean(ACC))

#extract descriptive data
PH4_descriptive <- mySummary(PH4, ACC, MType, Group)

# perform pairwise t-tests
Tbr   <- PH4 %>% filter(MType == "tbr") %>% t.test(data = ., ACC ~ Group, paired=FALSE)
d_Tbr <- t_to_d(Tbr$statistic, Tbr$parameter, paired = FALSE)
F0 <- PH4 %>% filter(MType == "f0") %>% t.test(data = ., ACC ~ Group, paired=FALSE)
d_F0 <- t_to_d(F0$statistic, F0$parameter, paired = FALSE)
Full  <- PH4 %>% filter(MType == "full") %>% t.test(data = ., ACC ~ Group, paired=FALSE)
d_Full <- t_to_d(Full$statistic, Full$parameter, paired = FALSE)

#add descriptions for output:
info <- c("Tbr","F0","Full","Descriptive Data")

# save output to textfile
capture.output(info[1],Tbr, d_Tbr, info[2],F0, d_F0, info[3], Full, d_Full, info[4], as.matrix(PH4_descriptive), 
               file="output/emotion_classification/amateurs_A_PH4_Int_MType_Group.txt")

#remove objects to keep environment tidy
rm(info, Tbr,F0, Full, PH4_descriptive, d_F0, d_Full, d_Tbr)

################################################################################
# #expected output (reduced)
# [1] "Full"
# t = 0.86093, df = 82.303, p-value = 0.3918
# [1] "F0"
# t = 0.081278, df = 84.375, p-value = 0.9354
# [1] "Tbr"
# tt = 0.59334, df = 83.98, p-value = 0.5546
#[1] "Descriptive Data"
#     MType  Group  ACC         SD           N    SE               CI          
#[1,] "f0"   "1"   "0.6218238" "0.09436095" "45" "0.014066500"
#[2,] "f0"   "2"   "0.6203239" "0.07833249" "43" "0.011945593"
#[3,] "full" "1"   "0.7562564" "0.09911747" "45" "0.014775560"
#[4,] "full" "2"   "0.7400693" "0.07624543" "43" "0.011627319"
#[5,] "tbr"  "1"   "0.4162587" "0.07717783" "45" "0.011504991"
#[6,] "tbr"  "2"   "0.4073652" "0.06300160" "43" "0.009607653"
################################################################################

rm(PH4)

###############  PH5:  Interaction of Emotion and Morph Type  ###############

#-> effect of Morph Type is tested for each emotion separately

#reaggreate dataset
PH5 <- D %>% group_by(Subject, Emo, MType) %>% summarise(ACC=mean(ACC))

#extract descriptive data
PH5_descriptive <- mySummary(PH5, ACC, Emo, MType)
PH5_descriptive

############## descriptive data  ##################
#   Emo   MType ACC   SD     N      SE     CI
#1  fea   f0    0.559 0.147  89  0.0156  0.0310
#2  fea   full  0.655 0.145  89  0.0153  0.0305
#3  fea   tbr   0.344 0.163  89  0.0173  0.0344
#4  hap   f0    0.795 0.135  89  0.0143  0.0285
#5  hap   full  0.939 0.0673 89  0.00713 0.0142
#6  hap   tbr   0.457 0.222  89  0.0235  0.0467
#7  ple   f0    0.472 0.157  89  0.0166  0.0331
#8  ple   full  0.655 0.160  89  0.0170  0.0338
#9  ple   tbr   0.368 0.155  89  0.0164  0.0327
#10 sad   f0    0.660 0.159  89  0.0169  0.0336
#11 sad   full  0.736 0.158  89  0.0167  0.0332
#12 sad   tbr   0.480 0.162  89  0.0171  0.0341
###################################################

capture.output(as.matrix(PH5_descriptive), 
               file = "output/emotion_classification/amateurs_A_PH5_Int_emo_MType_descriptive_data.txt")

############## Happy Stimuli only: ######################################

PH5hap <- PH5 %>% filter(Emo == "hap")

#####################
###### ANOVA ########
# data = PH5hap
# dv = ACC
# wid = Subject 
# within = MType
ANOVAModel<-ezANOVA(data=PH5hap, wid=Subject, dv=ACC, within= MType, type =3, detailed=TRUE)
hap = tracedEzOut(ANOVAModel, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

#########################################################
#expected output 
#--- FORMATTED RESULTS ------------------------------------`
#       Effect                                       Text
#1 (Intercept)  F(1, 88) = 3934.861, p < .001, np2 = .978
#2       MType F(2, 176) =  305.031, p < .001, np2 = .776
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
#[1] "F0 vs Tbr"
# t = 14.605, df = 88, p-value < 0.001
# [1] "Tbr vs Full"
# t = 21.048, df = 88, p-value < 0.001
# [1] " F0 vs Full"
# t = -11.994, df = 88, p-value < 0.001
###############################################

#add descriptions for output:
info <- c("ANOVA on MType for happy stimuli only","F0 vs Tbr","Tbr vs Full"," F0 vs Full")

# save output to textfile
capture.output(info[1], hap, info[2], F0Tbr, d_F0Tbr, info[3],TbrFull, d_TbrFull, info[4], F0Full, d_F0Full, 
               file="output/emotion_classification/amateurs_A_PH5_Int_aEmo_aMType_hap.txt")

#remove objects to keep environment tidy
rm(info,hap, F0Tbr,TbrFull, F0Full, d_F0Tbr, d_TbrFull, d_F0Full, PH5hap)


# ############## Pleasure Stimuli only: ######################################

PH5ple <- PH5 %>% filter(Emo == "ple")

#####################
###### ANOVA ########
# data = PH5ple
# dv = ACC
# wid = Subject
# within = MType
ANOVAModel<-ezANOVA(data=PH5ple, wid=Subject, dv=ACC, within= MType, type=3, detailed=TRUE)
ple = tracedEzOut(ANOVAModel, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

###########################################################
#expected output
# --- FORMATTED RESULTS ------------------------------------`
#       Effect                                       Text
#1 (Intercept)  F(1, 88) = 1340.922, p < .001, np2 = .938
#2       MType F(2, 176) =  149.516, p < .001, np2 = .629
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
#[1] "F0 vs Tbr"
# t = 6.1383, df = 88, p-value < 0.001
# [1] "Tbr vs Full"
# t = 16.042, df = 88, p-value < 0.001
# [1] " F0 vs Full"
# t = -11.802, df = 88, p-value < 0.001
# ###############################################


#add descriptions for output:
info <- c("ANOVA on MType for pleasure stimuli only","F0 vs Tbr","Tbr vs Full"," F0 vs Full")

# save output to textfile
capture.output(info[1], ple, info[2], F0Tbr, d_F0Tbr, info[3],TbrFull, d_TbrFull, info[4], F0Full, d_F0Full, 
               file="output/emotion_classification/amateurs_A_PH5_Int_aEmo_aMType_ple.txt")

#remove objects to keep environment tidy
rm(info,ple, F0Tbr,TbrFull, F0Full, d_F0Tbr, d_TbrFull, d_F0Full, PH5ple)


# ############## Fear Stimuli only: ######################################

PH5fea <- PH5 %>% filter(Emo == "fea")

#####################
###### ANOVA ########
# data = PH5fea
# dv = ACC
# wid = Subject
# within = MType
ANOVAModel<-ezANOVA(data=PH5fea, wid=Subject, dv=ACC, within= MType, type=3, detailed=TRUE)
fea = tracedEzOut(ANOVAModel, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

############################################################
#expected output
# --- FORMATTED RESULTS ------------------------------------`
#       Effect                                       Text
#1 (Intercept)  F(1, 88) = 1527.408, p < .001, np2 = .946
#2       MType F(2, 176) =  204.351, p < .001, np2 = .699
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
#[1] "F0 vs Tbr"
# t = 12.912, df = 88, p-value < 0.001
# [1] "F0 vs Tbr"
# t = 18.534, df = 88, p-value < 0.001
# [1] " F0 vs Full"
# t = -7.0466, df = 88, p-value < 0.001
# ###############################################


#add descriptions for output:
info <- c("ANOVA on MType for fea stimuli only","F0 vs Tbr","Tbr vs Full"," F0 vs Full")

# save output to textfile
capture.output(info[1], fea, info[2], F0Tbr, d_F0Tbr, info[3],TbrFull, d_TbrFull, info[4], F0Full, d_F0Full, 
               file="output/emotion_classification/amateurs_A_PH5_Int_aEmo_aMType_fea.txt")

#remove objects to keep environment tidy
rm(info,fea, F0Tbr,TbrFull, F0Full, d_F0Tbr, d_TbrFull, d_F0Full, PH5fea)

############## Sad Stimuli only: ######################################

PH5sad <- PH5 %>% filter(Emo == "sad")

#####################
###### ANOVA ########
# data = PH5sad
# dv = ACC
# wid = Subject
# within = MType
ANOVAModel<-ezANOVA(data=PH5sad, wid=Subject, dv=ACC, within= MType, type=3, detailed=TRUE)
sad = tracedEzOut(ANOVAModel, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

#########################################################
#expected output
# $`--- FORMATTED RESULTS ------------------------------`
#       Effect                                       Text
#1 (Intercept)  F(1, 88) = 2269.894, p < .001, np2 = .963
#2       MType F(2, 176) =  100.873, p < .001, np2 = .534
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
#[1] "F0 vs Tbr"
# t = 8.3074, df = 88, p-value < 0.001
# [1] "Tbr vs Full"
# t = 13.072, df = 88, p-value < 0.001
# [1] " F0 vs Full"
# t = -5.743, df = 88, p-value < 0.001
# ###############################################

#add descriptions for output:
info <- c("ANOVA on MType for sad stimuli only","F0 vs Tbr","Tbr vs Full"," F0 vs Full")

# save output to textfile
capture.output(info[1], sad, info[2], F0Tbr, d_F0Tbr, info[3],TbrFull, d_TbrFull, info[4], F0Full, d_F0Full, 
               file="output/emotion_classification/amateurs_A_PH5_Int_aEmo_aMType_sad.txt")

#remove objects to keep environment tidy
rm(info,sad, F0Tbr,TbrFull, F0Full, d_F0Tbr, d_TbrFull, d_F0Full, PH5sad)



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
               file="output/emotion_classification/amateurs_A_PH5_Diff_F0-Tbr.txt")

#remove objects to keep environment tidy
rm(info, HapPle, HapFea, HapSad, PleFea, PleSad, FeaSad,PH5_descriptive, PH5,
   d_HapPle, d_HapFea, d_HapSad, d_PleFea, d_PleSad, d_FeaSad)


#############################################
#expected output (reduced)
# [1] "Hap vs Ple"
# t = 8.1044, df = 88, p-value < 0.001
# [1] "Hap vs Fea"
# t = -4.3051, df = 88, p-value < 0.001
# [1] "Hap vs Sad"
# t = 5.1269, df = 88, p-value < 0.001
# [1] "Ple vs Fea"
# t = 4.9591, df = 88, p-value < 0.001
# [1] "Ple vs Sad"
# t = -2.6048, df = 88, p-value = 0.01079
# [1] "Fea vs Sad"
# t = 1.2232, df = 88, p-value = 0.2245
# [1] "Descriptive Data"
#      Emo   Diff        SD          N    SE           CI          
#[1,] "fea" "0.2149680" "0.1570582" "89" "0.01664814" "0.03308468"
#[2,] "hap" "0.3379580" "0.2183005" "89" "0.02313981" "0.04598550"
#[3,] "ple" "0.1036916" "0.1593640" "89" "0.01689255" "0.03357038"
#[4,] "sad" "0.1799731" "0.2043799" "89" "0.02166422" "0.04305309"

#############################################


rm(ANOVAModel, D)


### End of Script
