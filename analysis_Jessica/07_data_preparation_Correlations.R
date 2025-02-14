##########################################################################
## File: 07_data_preparation_Correlations.R.R
## Data Preparation for the Correlation Data
# authors: Christine Nussbaum, Jessica senftleben
# date 07/2021, 02/2024

#NOT PUBLISHED

# clear directory
rm(list=ls())

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# load required packages
library(tidyverse)
library(ez) # version 4.4-0
library(effectsize) # version 0.4.5
library(Hmisc) # version 4.5-0
library(ppcor) # version 1.1

# load relevant functions
source("functions/loadPTKExperimentData.R")

# set some relevant setting 
options(scipen = 999)

#---------------------------------------------------------------------------------#
#                 Correlation Data PROMS - AMATEURS                    #
#---------------------------------------------------------------------------------#

#get the preprocessed/prepared data: 
#Experiment data
load(file="input/amateurs_Exp_processed.RData")

# remove participant with degree in music science
D2 <- subset(D2,Code!="AEB56L") # instrumentalists

#PROMS Data
load(file="input/amateurs_PROMS_prepared.RData")

# rename data sets to make script easier to read
D <- D2
PROMS <- PROMS2
rm(D2, PROMS2)

## PROMS
PROMS <- PROMS[, c(1:3, 12)]  # Use Confidence Rating
#PROMS <- PROMS[, c(1:3, 16)]  # use for D-Prime Rating

# convert to wide format
PROMS <- PROMS %>% pivot_wider(names_from = c(Test), values_from = c(Confidence))

#PROMS <- PROMS %>% pivot_wider(names_from = c(Test), values_from = c(dprime))   # use for D-Prime Rating

PROMS$PROMS_mean <- (PROMS$melody + PROMS$pitch + PROMS$rhythm + PROMS$timbre ) /4

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
Corrdata <- merge(B, PROMS)

#save for later analysis: 
save(Corrdata, file="input/amateurs_Corrdata_PROMS.RData")

rm(B,D,Corrdata,PROMS)

#---------------------------------------------------------------------------------#
#                 Correlation Data PROMS - ALLGROUPS                    #
#---------------------------------------------------------------------------------#

#Experiment data
load(file="input/allgroups_Exp_processed.RData")
#PROMS Data
load(file="input/allgroups_PROMS_prepared.RData")

## PROMS
PROMS <- PROMS[, c(1:3, 12)]  # Use Confidence Rating
#PROMS <- PROMS[, c(1:3, 16)]  # use for D-Prime Rating

# convert to wide format
PROMS <- PROMS %>% pivot_wider(names_from = c(Test), values_from = c(Confidence))

#PROMS <- PROMS %>% pivot_wider(names_from = c(Test), values_from = c(dprime))   # use for D-Prime Rating

PROMS$PROMS_mean <- (PROMS$melody + PROMS$pitch + PROMS$rhythm + PROMS$timbre ) /4

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
Corrdata <- merge(B, PROMS)

#save for later analysis: 
save(Corrdata, file="input/allgroups_Corrdata_PROMS.RData")

rm(B,D,Corrdata,PROMS)

#---------------------------------------------------------------------------------#
#                 Correlation Data MSI - AMATEURS                    #
#---------------------------------------------------------------------------------#

#Experiment data
load(file="input/amateurs_Exp_processed.RData")

# remove participant with degree in music science
D2 <- subset(D2, Code!="AEB56L") # instrumentalist

#survey data including the Gold-MSI
load(file="input/amateurs_survey_processed.RData")

# remove participant with degree in music science
survey2 <- subset(survey2, Code!="AEB56L") # instrumentalist

# rename data set to make script easier to read
D <- D2
survey <- survey2
rm(D2,survey2)

## Gold-MSI
MSI <- survey[, c(1, 31, 26:30 )]
rm(survey)

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
Corrdata <- merge(B, MSI)

#save for later analysis: 
save(Corrdata, file="input/amateurs_Corrdata_MSI.RData")

rm (D,B,Corrdata,MSI)

#---------------------------------------------------------------------------------#
#                 Correlation Data MSI - ALLGROUPS                   #
#---------------------------------------------------------------------------------#

#Experiment data
load(file="input/allgroups_Exp_processed.RData")
#survey data including the Gold-MSI
load(file="input/allgroups_survey_processed.RData")


## Gold-MSI
MSI <- survey[, c(1, 31, 26:30 )]
rm(survey)

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
Corrdata <- merge(B, MSI)

#save for later analysis: 
save(Corrdata, file="input/allgroups_Corrdata_MSI.RData")

rm(B,B,Corrdata,MSI)

#---------------------------------------------------------------------------------#
#                 Correlation Data AQ & OCEAN - AMATEURS                   #
#---------------------------------------------------------------------------------#


#Experiment data
load(file="input/amateurs_Exp_processed.RData")

# remove participant with degree in music science
D2 <- subset(D2, Code!="AEB56L") # instrumentalist

#survey data including the Gold-MSI
load(file="input/amateurs_survey_processed.RData")

# remove participant with degree in music science
survey2 <- subset(survey2, Code!="AEB56L") # instrumentalist


# rename data set to make script easier to read
D <- D2
survey <- survey2
rm(D2,survey2)

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

rm(B,D,Corrdata,AQOCEAN,survey)

#---------------------------------------------------------------------------------#
#                 Correlation Data AQ & OCEAN - ALLGROUPS                   #
#---------------------------------------------------------------------------------#

#Experiment data
load(file="input/allgroups_Exp_processed.RData")
#survey data including the Gold-MSI
load(file="input/allgroups_survey_processed.RData")


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
save(Corrdata, file="input/allgroups_Corrdata_OCEAN_AQ.RData")

rm(B,D,Corrdata,AQOCEAN,survey)


### End of Script
