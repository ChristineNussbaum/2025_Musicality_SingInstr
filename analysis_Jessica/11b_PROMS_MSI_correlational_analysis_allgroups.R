##########################################################################
## File: 11b_PROMS_MSI_correlational_analysis_allgroups.R
## This script runs some correlational analyses on PROMS data with the Gold-MSI data
# author: Christine Nussbaum (christine.nussbaum@uni-jena.de)
# date 07/2023

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


#---------------------------------------------------------------------------------#
#                 Prepare Datasets for Correlation Analyses                       #
#---------------------------------------------------------------------------------#
#load all data
load(file="input/allgroups_Corrdata_PROMS.RData")
PROMS <- Corrdata
load(file="input/allgroups_Corrdata_MSI.RData")
MSI <- Corrdata
load(file="input/allgroups_Corrdata_OCEAN_AQ.RData")
OCEAN <- Corrdata

#merge the data

Corrdata <- merge(PROMS, MSI)
Corrdata <- merge(Corrdata, OCEAN)

rm(MSI, PROMS, OCEAN)

#remove the Variables we are not interested in: 

Corrdata <- Corrdata[,-c(6:8, 10)]


#---------------------------------------------------------------------------------#
#              Correlational Analyses - PROMS and MSI                             #
#---------------------------------------------------------------------------------#

Correlations <- rcorr(as.matrix(Corrdata[,c(11, 7:10, 12:17)]), type="spearman") # correlate with ACC 



# prepare them to be saved and plotted

Cor_r <- data.frame(Correlations$r[1:5, 6:11])
Cor_p <- data.frame(Correlations$P[1:5, 6:11])
Cor_n <- Correlations$n[1:1]

Cor_r$PROMS <- rownames(Cor_r)
Cor_p$PROMS <- rownames(Cor_p)

#convert to long format

Cor_r = Cor_r %>% pivot_longer(cols = ME_mean:Mean_singing, names_to = "MSI", values_to = "r")
Cor_p = Cor_p %>% pivot_longer(cols = ME_mean:Mean_singing, names_to = "MSI", values_to = "p")

Cor <-merge(Cor_r, Cor_p)
Cor$N <- Cor_n
rm(Cor_r, Cor_p, Cor_n)



#perform the multiple comparison correction: 
#https://www.youtube.com/watch?v=rZKa4tW2NKs
FDR = 0.05  # set false discovery rate to 5%
Cor <- Cor %>% arrange(p) #order the dataset by p-value
Cor$i <- c(1:30) # assign rank of p-value
m <- 30  # assing number of tests 
Cor$CritValue <- (Cor$i / m) * FDR  #(i/m)*Q  # correct the critical value
Cor$p_corrected <- Cor$p * (m / Cor$i)        # correct the p-value itself

#perform adjustement of the corrected value: 
Cor$p_corrected2 <- Cor$p_corrected 

for (j in (m-1):1){
  Cor$p_corrected2[j] <- ifelse(Cor$p_corrected[j] > Cor$p_corrected[j+1], Cor$p_corrected2[j+1], Cor$p_corrected[j] )
}



# tidy dataset
Cor$PROMS <- recode(Cor$PROMS, PROMS_mean = "PROMS_Averaged", pitch = "Pitch", melody = "Melody", timbre = "Timbre", rhythm = "Rhythm")
Cor$PROMS <- factor(Cor$PROMS, levels = c("PROMS_Averaged", "Pitch", "Melody", "Timbre", "Rhythm"))

Cor$MSI <- recode(Cor$MSI, ME_mean = "General ME", Mean_active = "Active", Mean_perception = "Perception", Mean_singing = "Singing", Mean_emotion = "Emotion", Mean_education = "Education")
Cor$MSI <- factor(Cor$MSI, levels = c("General ME", "Active", "Perception", "Singing", "Emotion", "Education"))

# save the dataset
capture.output(as.matrix(Cor), file="output/emotion_classification/allgroups_C_Correlations_PROMS_MSI.txt")


### End of Script