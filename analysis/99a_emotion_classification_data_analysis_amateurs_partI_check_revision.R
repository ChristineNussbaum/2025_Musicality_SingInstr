##########################################################################
## File: 02a_emotion_classification_data_analysis_amateurs_partI.R
## This script analysis the emotion classification performance of musicians and non-musicians
# authors: Christine Nussbaum (christine.nussbaum@uni-jena.de), Jessica Dethloff
# date 10/2022, 02/2024

#This is a script to check whether the first and last 25% of the trials differ in accuracy
# to check for fatigue effects

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

#add Trialnr

D$No <- rep(c(1:312), 88)

#check if it worked: 
table(table(D$Subject, D$No))

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
#                   Analysis of first and last 25 %                              #
#---------------------------------------------------------------------------------#

#extract emotion perception accuracy

#remove average trials for this analysis
D <- D %>% filter(Emo != "avg")

#Filter first 78 trial and last  78 (after 234) 

D <- D %>% filter(No <79 | No > 234)


#add variable "quartile"

D$quart <- ifelse(D$No < 79, "first", "last")

#check
table(D$quart, D$No)

##############################################################################
########                  Statistical analysis                   #############
##############################################################################

### aggregate data for ANOVA
D <- D %>% group_by(Subject, Group, quart) %>% summarise(ACC = mean(ACC),
                                                              N = length(Subject)) # how many trials entered each average

#88 *2 = 176

### Define variables as appropriate data class and drop levels of factors 
D$Subject <- as.factor(as.character(D$Subject))
D$Group <- as.factor(as.character(D$Group))
D <-  droplevels(D)

#####################
###### ANOVA ########
# data = D
# dv = ACC
# wid = Subject 
# within = quart
# between = Group

a<-ezANOVA(data=D, dv=.(ACC), wid=.(Subject), within = .(quart), between = .(Group), type=3, detailed = TRUE)
b = tracedEzOut(a, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")
#this analysis return a warning about unequal groups, since there are 45 singers but 44 instrumentalists


check <- mySummary(D, ACC, quart)
check2 <- mySummary(D, ACC, Group)

### End of Script