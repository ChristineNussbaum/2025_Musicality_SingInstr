##########################################################################
## File: 10a_emotion_recognition_AQ_OCEAN_correlational_analysis_amateurs.R
## This script runs some correlational analyses on the emotion recognition data and the AQ and OCEAN data
# author: Christine Nussbaum (christine.nussbaum@uni-jena.de)
# date 10/2022

# clear directory
rm(list=ls())

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# load required packages
library(tidyverse) #version 1.3.1
library(ez) # version 4.4-0
library(effectsize) # version 0.4.5
library(Hmisc) # version 4.5-0

# load relevant functions
source("functions/mySummary.R") 

# set some relevant setting 
options(scipen = 999)

#---------------------------------------------------------------------------------
#get the preprocessed data: 

#Experiment data
load(file="input/amateurs_Exp_processed.RData")

# remove participant with degree in music science
D2 <- subset(D2, Code!="AEB56L")

#survey data including the Gold-MSI
load(file="input/amateurs_survey_processed.RData")

# remove participant with degree in music science
survey2 <- subset(survey2, Code!="AEB56L")

# rename data set to make script easier to read
D <- D2
survey <- survey2
rm(D2,survey2)

#---------------------------------------------------------------------------------#
#                 Prepare Datasets for Correlation Analyses                       #
#---------------------------------------------------------------------------------#

## AQ and OCEAN
# reduce to relevant Variables only: 

AQOCEAN <- survey[, c(1, 21:25, 14:20)]


## Emotion recognition data

# remove avg trials
B <- D %>% filter(Emo != "avg")

# average data

B <- B %>% group_by(Code, Group, MType) %>% summarise(ACC = mean(ACC), 
                                                      RT = mean(RT),
                                                      N = length(Emo))

B$N <- NULL # that was just included to check if averaging worked properly

# convert to wide format

B <- B %>% pivot_wider(names_from = c(MType), values_from = c(ACC, RT))

B$ACC_all <- (B$ACC_f0 + B$ACC_full + B$ACC_tbr)/3
B$RT_all <- (B$RT_f0 + B$RT_full + B$RT_tbr)/3

# merge the datasets
Corrdata <- merge(B, AQOCEAN)

#save for later analysis: 
save(Corrdata, file="input/amateurs_Corrdata_OCEAN_AQ.RData")

#---------------------------------------------------------------------------------#
#                           Correlational Analyses OCEAN                          #
#---------------------------------------------------------------------------------#

Correlations_ACC <- rcorr(as.matrix(Corrdata[,c(9, 3:5, 11:15)]), type="spearman") 


# prepare them to be saved and plotted

Cor_r <- data.frame(Correlations_ACC$r[1:4, 5:9])
Cor_p <- data.frame(Correlations_ACC$P[1:4, 5:9])
Cor_n <- Correlations_ACC$n[1:1]

Cor_r$MType <- rownames(Cor_r)
Cor_p$MType <- rownames(Cor_p)

#convert to long format

Cor_r = Cor_r %>% pivot_longer(cols = O:N, names_to = "Test", values_to = "r")
Cor_p = Cor_p %>% pivot_longer(cols = O:N, names_to = "Test", values_to = "p")

Cor <-merge(Cor_r, Cor_p)
Cor$N <- Cor_n

rm(Cor_r, Cor_p, Cor_n)

capture.output(as.matrix(Cor), file="output/emotion_classification/amateurs_C_Correlations_ACC_OCEAN.txt")

#---------------------------------------------------------------------------------#
#                           Correlational Analyses AQ                          #
#---------------------------------------------------------------------------------#

Correlations_ACC <- rcorr(as.matrix(Corrdata[,c(9, 3:5, 16:22)]), type="spearman")


# prepare them to be saved and plotted

Cor_r <- data.frame(Correlations_ACC$r[1:4, 5:11])
Cor_p <- data.frame(Correlations_ACC$P[1:4, 5:11])
Cor_n <- Correlations_ACC$n[1:1]

Cor_r$MType <- rownames(Cor_r)
Cor_p$MType <- rownames(Cor_p)

#convert to long format

Cor_r = Cor_r %>% pivot_longer(cols = AQ_SocialSkills:AQ_Total, names_to = "Test", values_to = "r")
Cor_p = Cor_p %>% pivot_longer(cols = AQ_SocialSkills:AQ_Total, names_to = "Test", values_to = "p")

Cor <-merge(Cor_r, Cor_p)
Cor$N <- Cor_n

rm(Cor_r, Cor_p, Cor_n)

#perform the multiple comparison correction: 
#https://www.youtube.com/watch?v=rZKa4tW2NKs
FDR = 0.05  # set false discovery rate to 5%
Cor <- Cor %>% arrange(p) #order the dataset by p-value
Cor$i <- c(1:28) # assign rank of p-value
m <- 28  # adding number of tests 
Cor$CritValue <- (Cor$i / m) * FDR  #(i/m)*Q  # correct the critical value
Cor$p_corrected <- Cor$p * (m / Cor$i)        # correct the p-value itself

#perform adjustement of the corrected value: 
Cor$p_corrected2 <- Cor$p_corrected 

for (j in (m-1):1){
  Cor$p_corrected2[j] <- ifelse(Cor$p_corrected[j] > Cor$p_corrected2[j+1], Cor$p_corrected2[j+1], Cor$p_corrected[j] )
}


capture.output(as.matrix(Cor), file="output/emotion_classification/amateurs_C_Correlations_ACC_AQ.txt")

### End of Script