##########################################################################
## File: 08b_emotion_recognition_PROMS_correlational_analysis_allgroups.R
## This script runs some correlational analyses on the emotion recognition data with the PROMS
# authors: Christine Nussbaum (christine.nussbaum@uni-jena.de), Jessica Senftleben
# date 07/2023, 02/2024

# clear directory
rm(list=ls())

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# load required packages
library(tidyverse) #version 1.3.1
library(ez) # version 4.4-0
library(effectsize) # version 0.4.5
library(Hmisc) # version 4.5-0
library(ppcor) # version 1.1

# load relevant functions
source("functions/mySummary.R") 

# set some relevant setting 
options(scipen = 999)

#---------------------------------------------------------------------------------
#get the preprocessed/prepared data: 


#Experiment data
load(file="input/allgroups_Exp_processed.RData")

#PROMS Data
load(file="input/allgroups_PROMS_prepared.RData")

#---------------------------------------------------------------------------------#
#                 Prepare Datasets for Correlation Analyses                       #
#---------------------------------------------------------------------------------#

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

#---------------------------------------------------------------------------------#
#              1. Correlational Analyses - uncontrolled                           #
#---------------------------------------------------------------------------------#


Correlations_ACC <- rcorr(as.matrix(Corrdata[,c(9, 3:5, 11:15)]), type="spearman") # calculate correlations for confidence ratings

Correlations_RT <- rcorr(as.matrix(Corrdata[,c(10, 6:8, 11:15)]), type="spearman") # calculate correlations for reaction time


# prepare correlations_ACC to be saved and plotted

Cor_r <- data.frame(Correlations_ACC$r[1:4, 5:9])
Cor_p <- data.frame(Correlations_ACC$P[1:4, 5:9])
Cor_n <- Correlations_ACC$n[1:1]

Cor_r$MType <- rownames(Cor_r)
Cor_p$MType <- rownames(Cor_p)

#convert to long format
Cor_r = Cor_r %>% pivot_longer(cols = melody:PROMS_mean, names_to = "Test", values_to = "r")
Cor_p = Cor_p %>% pivot_longer(cols = melody:PROMS_mean, names_to = "Test", values_to = "p")

Cor <-merge(Cor_r, Cor_p)
Cor$N <- Cor_n

rm(Cor_r, Cor_p, Cor_n)


# tidy dataset
Cor$MType <- recode(Cor$MType, ACC_all = "Averaged", ACC_full = "Full", ACC_f0 = "F0", ACC_tbr = "Timbre")
Cor$MType <- factor(Cor$MType, levels = c("Averaged", "Full", "F0", "Timbre"))

Cor$Test <- recode(Cor$Test, PROMS_mean = "PROMS_Averaged", pitch = "Pitch", melody = "Melody", timbre = "Timbre", rhythm = "Rhythm")
Cor$Test <- factor(Cor$Test, levels = c("PROMS_Averaged", "Pitch", "Melody", "Timbre", "Rhythm"))

#perform the multiple comparison correction (Benjamini-Hochberg Correction): 
#https://www.youtube.com/watch?v=rZKa4tW2NKs
FDR = 0.05  # set false discovery rate to 5%
Cor <- Cor %>% arrange(p) #order the dataset by p-value
Cor$i <- c(1:20) # assign rank of p-value
m <- 20  # assing number of tests 
Cor$CritValue <- (Cor$i / m) * FDR  #(i/m)*Q  # correct the critical value
Cor$p_corrected <- Cor$p * (m / Cor$i)        # correct the p-value itself

#perform adjustment of the corrected value: 
Cor$p_corrected2 <- Cor$p_corrected 

for (j in (m-1):1){
  Cor$p_corrected2[j] <- ifelse(Cor$p_corrected[j] > Cor$p_corrected2[j+1], Cor$p_corrected2[j+1], Cor$p_corrected[j] )
}

# save the output
capture.output(as.matrix(Cor), file="output/emotion_classification/allgroups_C_Correlations_ACC_PROMS.txt")

### prepare RT data as well: 

Cor_r <- data.frame(Correlations_RT$r[1:4, 5:9])
Cor_p <- data.frame(Correlations_RT$P[1:4, 5:9])
Cor_n <- Correlations_RT$n[1:1]

Cor_r$MType <- rownames(Cor_r)
Cor_p$MType <- rownames(Cor_p)

#convert to long format

Cor_r = Cor_r %>% pivot_longer(cols = melody:PROMS_mean, names_to = "Test", values_to = "r")
Cor_p = Cor_p %>% pivot_longer(cols = melody:PROMS_mean, names_to = "Test", values_to = "p")

Cor_RT <-merge(Cor_r, Cor_p)
Cor_RT$N <- Cor_n

rm(Cor_r, Cor_p, Cor_n)


# tidy dataset
Cor_RT$MType <- recode(Cor_RT$MType, RT_all = "Averaged", RT_full = "Full", RT_f0 = "F0", RT_tbr = "Timbre")
Cor_RT$MType <- factor(Cor_RT$MType, levels = c("Averaged", "Full", "F0", "Timbre"))

Cor_RT$Test <- recode(Cor_RT$Test, PROMS_mean = "PROMS_Averaged", pitch = "Pitch", melody = "Melody", timbre = "Timbre", rhythm = "Rhythm")
Cor_RT$Test <- factor(Cor_RT$Test, levels = c("PROMS_Averaged", "Pitch", "Melody", "Timbre", "Rhythm"))

# save the output
capture.output(as.matrix(Cor_RT), file="output/emotion_classification/allgroups_C_Correlations_RT_PROMS.txt")


#save correlations within PROMS and within Vocal Emotion Perception Performance

#VER within
Cor_r <- data.frame(Correlations_ACC$r[2:4, 2:4])
Cor_p <- data.frame(Correlations_ACC$P[2:4, 2:4])
Cor_n <- Correlations_ACC$n[1:1]

# save the dataset
capture.output(as.matrix(Cor_r), as.matrix(Cor_p), Cor_n, file="output/emotion_classification/allgroups_C_Correlations_ACC_VER_within.txt")


#PROMS within
Cor_r <- data.frame(Correlations_ACC$r[5:8, 5:8])
Cor_p <- data.frame(Correlations_ACC$P[5:8, 5:8])
Cor_n <- Correlations_ACC$n[1:1]

# save the dataset
capture.output(as.matrix(Cor_r), as.matrix(Cor_p), Cor_n, file="output/emotion_classification/allgroups_C_Correlations_ACC_PROMS_within.txt")


rm(Cor_RT,Correlations_ACC, Correlations_RT)

#---------------------------------------------------------------------------------#
#                            Data Visualization                                   #
#---------------------------------------------------------------------------------#

# plot a 4 x 5 matrix with all the correlations and Scatterplots

# prepare dataset
Corrdata <- Corrdata[, -c(6,7,8,10)]

Emo <- Corrdata[, c(1:6)] %>% pivot_longer(cols = ACC_f0:ACC_all, names_to = "MType", values_to = "ACC")
PROMS <- Corrdata[, c(1,2,7:11)] %>% pivot_longer(cols = melody:PROMS_mean, names_to = "Test", values_to = "Confidence")
#PROMS <- Corrdata[, c(1,2,7:11)] %>% pivot_longer(cols = melody:PROMS_mean, names_to = "Test", values_to = "dprime")   # use for D-Prime Rating

Data <- merge(Emo, PROMS)

rm(Emo, PROMS)

# recode factors
Data$MType <- recode(Data$MType, ACC_all = "Averaged", ACC_full = "Full", ACC_f0 = "F0", ACC_tbr = "Timbre")
Data$MType <- factor(Data$MType, levels = c("Averaged", "Full", "F0", "Timbre"))

Data$Test <- recode(Data$Test, PROMS_mean = "PROMS_Averaged", pitch = "Pitch", melody = "Melody", timbre = "Timbre", rhythm = "Rhythm")
Data$Test <- factor(Data$Test, levels = c("PROMS_Averaged", "Pitch", "Melody", "Timbre", "Rhythm"))

Data$Group <- factor(Data$Group, levels = c("M","A","C"))


# format Cor
Cor$p_corrected2 <- round(Cor$p_corrected2, 3)
Cor$pplot <- paste0("= ", Cor$p_corrected2)
Cor$pplot <- ifelse(Cor$pplot == "= 0", "< 0.001", Cor$pplot)

Cor$pplot
Cor$sig <- ifelse(Cor$p_corrected2 < 0.05, "bold.italic", "plain")


############################################

title = paste0("Correlation between Emotion Classification Performance and Music Perception Abilities (N = 166)")

filename = paste0("plots/allgroups_04_correlations_ACC_PROMS_Accuracy.png")

p<-(ggplot(data= Data, aes(x = Confidence, y=ACC)) +
      geom_point(aes(color = Group), shape= 18, size = 3) +
      labs(x = "Music Perception Abilities (Confidence)" , y = "Emotion Classification Performance" , title = title, color = "") +       #, title = title
      facet_grid(rows = vars(MType), cols = vars(Test), switch = "y") + #scales = "free"
      scale_color_manual(labels = c("Professionals", "Amateurs","Non-Musicians" ), values = c("red3", "green3","blue2")) +
      geom_smooth(method='lm', formula= y~x, color = "black") + 
      geom_text(data= Cor, 
                aes(label= paste0("r = ", round(r, 2), "\n p ", pplot), x = 0.1, y = 0.85, fontface = sig), color = "black") +
      scale_y_continuous(position = "right") + 
      scale_x_continuous(limits=c(0, 0.6), breaks = c(0.2, 0.4, 0.6)) + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            legend.position="bottom", 
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14),
            strip.text.y = element_text(size = 14),
            legend.text = element_text(size=14)))
ggsave(filename, width = 15, height = 12, dpi =300)


#-----------------------------------------------------------------------------------------------------------------#
#                            Correlations split for Professionals, NonMusicians and Amateurs                                 #
#-----------------------------------------------------------------------------------------------------------------#

#########################
# Professionals (M) only 
#########################

M <- Corrdata %>% filter(Group == "M")

Correlations_ACC <- rcorr(as.matrix(M[,c(6, 3:5, 11, 7:10)]), type="spearman")


# prepare them to be saved and plotted

Cor_r <- data.frame(Correlations_ACC$r[1:4, 5:9])
Cor_p <- data.frame(Correlations_ACC$P[1:4, 5:9])
Cor_n <- Correlations_ACC$n[1:1]

Cor_r$MType <- rownames(Cor_r)
Cor_p$MType <- rownames(Cor_p)

#convert to long format

Cor_r = Cor_r %>% pivot_longer(cols = PROMS_mean:timbre, names_to = "Test", values_to = "r")
Cor_p = Cor_p %>% pivot_longer(cols = PROMS_mean:timbre, names_to = "Test", values_to = "p")


Cor <-merge(Cor_r, Cor_p)
Cor$N <- Cor_n
rm(Cor_r, Cor_p, Cor_n)


#nothing is significant anyway -> no correction necessary

# tidy dataset
Cor$MType <- recode(Cor$MType, ACC_all = "Averaged", ACC_full = "Full", ACC_f0 = "F0", ACC_tbr = "Timbre")
Cor$MType <- factor(Cor$MType, levels = c("Averaged", "Full", "F0", "Timbre"))

Cor$Test <- recode(Cor$Test, PROMS_mean = "PROMS_Averaged", pitch = "Pitch", melody = "Melody", timbre = "Timbre", rhythm = "Rhythm")
Cor$Test <- factor(Cor$Test, levels = c("PROMS_Averaged", "Pitch", "Melody", "Timbre", "Rhythm"))

# save the dataset
capture.output(as.matrix(Cor), file="output/emotion_classification/allgroups_C_Correlations_ACC_PROMS_Mus_only.txt")



# plot a 4 x 5 matrix with all the correlations and Scatterplots

# prepare dataset

Emo <- M[, c(1:6)] %>% pivot_longer(cols = ACC_f0:ACC_all, names_to = "MType", values_to = "ACC")
PROMS <- M[, c(1,2,7:11)] %>% pivot_longer(cols = melody:PROMS_mean, names_to = "Test", values_to = "confidence")

Data <- merge(Emo, PROMS)

rm(Emo, PROMS)

# recode factors
Data$MType <- recode(Data$MType, ACC_all = "Averaged", ACC_full = "Full", ACC_f0 = "F0", ACC_tbr = "Timbre")
Data$MType <- factor(Data$MType, levels = c("Averaged", "Full", "F0", "Timbre"))

Data$Test <- recode(Data$Test, PROMS_mean = "PROMS_Averaged", pitch = "Pitch", melody = "Melody", timbre = "Timbre", rhythm = "Rhythm")
Data$Test <- factor(Data$Test, levels = c("PROMS_Averaged", "Pitch", "Melody", "Timbre", "Rhythm"))

Data$Group <- factor(Data$Group, levels = c("M","A","C"))


# format Cor

Cor$p <- round(Cor$p, 3)
Cor$pplot <- paste0("= ", Cor$p)
Cor$pplot <- ifelse(Cor$pplot == "= 0", "< 0.001", Cor$pplot)

Cor$sig <- ifelse(Cor$p < 0.05, "bold.italic", "plain")


############################################

title = paste0("Correlation between Emotion Classification Performance and Music Perception Abilities, Professionals only (N = 40)")

filename = paste0("plots/allgroups_04b_correlations_ACC_PROMS_Pro_only.png")

p<-(ggplot(data= Data, aes(x = confidence, y=ACC)) +
      geom_point(shape= 18, size = 3, color = "darkred") +
      labs(x = "Music Perception Abilities (Confidence)" , y = "Emotion Classification Performance" , title = title, color = "") +       #, title = title
      facet_grid(rows = vars(MType), cols = vars(Test), switch = "y") + #scales = "free"
      geom_smooth(method='lm', formula= y~x, color = "black") + 
      geom_text(data= Cor, 
                aes(label= paste0("r = ", round(r, 2), "\n p ", pplot), x = 0.1, y = 0.85, fontface = sig), color = "black") +
      scale_y_continuous(position = "right") + 
      scale_x_continuous(limits=c(0, 0.6), breaks = c(0.2, 0.4, 0.6)) + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            legend.position="bottom", 
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14),
            strip.text.y = element_text(size = 14),
            legend.text = element_text(size=14)))
ggsave(filename, width = 15, height = 12, dpi =300)


rm(M, Data, p, Correlations_ACC, Cor)

#################
# NonMusicians (C) only
#################

C <- Corrdata %>% filter(Group == "C")

Correlations_ACC <- rcorr(as.matrix(C[,c(6, 3:5, 11, 7:10)]), type="spearman")


# prepare them to be saved and plotted

Cor_r <- data.frame(Correlations_ACC$r[1:4, 5:9])
Cor_p <- data.frame(Correlations_ACC$P[1:4, 5:9])
Cor_n <- Correlations_ACC$n[1:1]

Cor_r$MType <- rownames(Cor_r)
Cor_p$MType <- rownames(Cor_p)

#convert to long format

Cor_r = Cor_r %>% pivot_longer(cols = PROMS_mean:timbre, names_to = "Test", values_to = "r")
Cor_p = Cor_p %>% pivot_longer(cols = PROMS_mean:timbre, names_to = "Test", values_to = "p")

Cor <-merge(Cor_r, Cor_p)
Cor$N <- Cor_n
rm(Cor_r, Cor_p, Cor_n)

#perform the multiple comparison correction: 
#https://www.youtube.com/watch?v=rZKa4tW2NKs
FDR = 0.05  # set false discovery rate to 5%
Cor <- Cor %>% arrange(p) #order the dataset by p-value
Cor$i <- c(1:20) # assign rank of p-value
m <- 20  # assing number of tests 
Cor$CritValue <- (Cor$i / m) * FDR  #(i/m)*Q  # correct the critical value
Cor$p_corrected <- Cor$p * (m / Cor$i)        # correct the p-value itself

#perform adjustement of the corrected value: 
Cor$p_corrected2 <- Cor$p_corrected 

for (j in (m-1):1){
  Cor$p_corrected2[j] <- ifelse(Cor$p_corrected[j] > Cor$p_corrected2[j+1], Cor$p_corrected2[j+1], Cor$p_corrected[j] )
}

# tidy dataset
Cor$MType <- recode(Cor$MType, ACC_all = "Averaged", ACC_full = "Full", ACC_f0 = "F0", ACC_tbr = "Timbre")
Cor$MType <- factor(Cor$MType, levels = c("Averaged", "Full", "F0", "Timbre"))

Cor$Test <- recode(Cor$Test, PROMS_mean = "PROMS_Averaged", pitch = "Pitch", melody = "Melody", timbre = "Timbre", rhythm = "Rhythm")
Cor$Test <- factor(Cor$Test, levels = c("PROMS_Averaged", "Pitch", "Melody", "Timbre", "Rhythm"))

# save the dataset
capture.output(as.matrix(Cor), file="output/emotion_classification/allgroups_C_Correlations_ACC_PROMS_Con_only.txt")



# plot a 4 x 5 matrix with all the correlations and Scatterplots

# prepare dataset
Emo <- C[, c(1:6)] %>% pivot_longer(cols = ACC_f0:ACC_all, names_to = "MType", values_to = "ACC")
PROMS <- C[, c(1,2,7:11)] %>% pivot_longer(cols = melody:PROMS_mean, names_to = "Test", values_to = "confidence")

Data <- merge(Emo, PROMS)

rm(Emo, PROMS)

# recode factors
Data$MType <- recode(Data$MType, ACC_all = "Averaged", ACC_full = "Full", ACC_f0 = "F0", ACC_tbr = "Timbre")
Data$MType <- factor(Data$MType, levels = c("Averaged", "Full", "F0", "Timbre"))

Data$Test <- recode(Data$Test, PROMS_mean = "PROMS_Averaged", pitch = "Pitch", melody = "Melody", timbre = "Timbre", rhythm = "Rhythm")
Data$Test <- factor(Data$Test, levels = c("PROMS_Averaged", "Pitch", "Melody", "Timbre", "Rhythm"))

Data$Group <- factor(Data$Group, levels = c("M","A","C"))


# format Cor
Cor$p_corrected2 <- round(Cor$p_corrected2, 3)
Cor$pplot <- paste0("= ", Cor$p_corrected2)
Cor$pplot <- ifelse(Cor$pplot == "= 0", "< 0.001", Cor$pplot)

Cor$sig <- ifelse(Cor$p_corrected2 < 0.05, "bold.italic", "plain")


############################################

title = paste0("Correlation between Emotion Classification Performance and Music Perception Abilities, Non-Musicians only (N = 38)")

filename = paste0("plots/allgroups_04c_correlations_ACC_PROMS_Con_only.png")

p<-(ggplot(data= Data, aes(x = confidence, y=ACC)) +
      geom_point(shape= 18, size = 3, color = "darkblue") +
      labs(x = "Music Perception Abilities (Confidence)" , y = "Emotion Classification Performance" , title = title, color = "") +       #, title = title
      facet_grid(rows = vars(MType), cols = vars(Test), switch = "y") + #scales = "free"
      geom_smooth(method='lm', formula= y~x, color = "black") + 
      geom_text(data= Cor, 
                aes(label= paste0("r = ", round(r, 2), "\n p ", pplot), x = 0.1, y = 0.85, fontface = sig), color = "black") +
      scale_y_continuous(position = "right") +
      scale_x_continuous(limits=c(0, 0.6), breaks = c(0.2, 0.4, 0.6)) + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            legend.position="bottom", 
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14),
            strip.text.y = element_text(size = 14),
            legend.text = element_text(size=14)))
ggsave(filename, width = 15, height = 12, dpi =300)

#tidy environment completely
rm(C, Data, p, Correlations_ACC, Cor)

#################
# Amateurs (A) only 
#################

A <- Corrdata %>% filter(Group == "A")

Correlations_ACC <- rcorr(as.matrix(A[,c(6, 3:5, 11, 7:10)]), type="spearman")


# prepare them to be saved and plotted

Cor_r <- data.frame(Correlations_ACC$r[1:4, 5:9])
Cor_p <- data.frame(Correlations_ACC$P[1:4, 5:9])
Cor_n <- Correlations_ACC$n[1:1]

Cor_r$MType <- rownames(Cor_r)
Cor_p$MType <- rownames(Cor_p)

#convert to long format

Cor_r = Cor_r %>% pivot_longer(cols = PROMS_mean:timbre, names_to = "Test", values_to = "r")
Cor_p = Cor_p %>% pivot_longer(cols = PROMS_mean:timbre, names_to = "Test", values_to = "p")

Cor <-merge(Cor_r, Cor_p)
Cor$N <- Cor_n
rm(Cor_r, Cor_p, Cor_n)

#perform the multiple comparison correction: 
#https://www.youtube.com/watch?v=rZKa4tW2NKs
FDR = 0.05  # set false discovery rate to 5%
Cor <- Cor %>% arrange(p) #order the dataset by p-value
Cor$i <- c(1:20) # assign rank of p-value
m <- 20  # assing number of tests 
Cor$CritValue <- (Cor$i / m) * FDR  #(i/m)*Q  # correct the critical value
Cor$p_corrected <- Cor$p * (m / Cor$i)        # correct the p-value itself

#perform adjustement of the corrected value: 
Cor$p_corrected2 <- Cor$p_corrected 

for (j in (m-1):1){
  Cor$p_corrected2[j] <- ifelse(Cor$p_corrected[j] > Cor$p_corrected[j+1], Cor$p_corrected2[j+1], Cor$p_corrected[j] )
}

# tidy dataset
Cor$MType <- recode(Cor$MType, ACC_all = "Averaged", ACC_full = "Full", ACC_f0 = "F0", ACC_tbr = "Timbre")
Cor$MType <- factor(Cor$MType, levels = c("Averaged", "Full", "F0", "Timbre"))

Cor$Test <- recode(Cor$Test, PROMS_mean = "PROMS_Averaged", pitch = "Pitch", melody = "Melody", timbre = "Timbre", rhythm = "Rhythm")
Cor$Test <- factor(Cor$Test, levels = c("PROMS_Averaged", "Pitch", "Melody", "Timbre", "Rhythm"))

# save the dataset
capture.output(as.matrix(Cor), file="output/emotion_classification/allgroups_C_Correlations_ACC_PROMS_Ama_only.txt")



# plot a 4 x 5 matrix with all the correlations and Scatterplots

# prepare dataset
Emo <- A[, c(1:6)] %>% pivot_longer(cols = ACC_f0:ACC_all, names_to = "MType", values_to = "ACC")
PROMS <- A[, c(1,2,7:11)] %>% pivot_longer(cols = melody:PROMS_mean, names_to = "Test", values_to = "confidence")

Data <- merge(Emo, PROMS)

rm(Emo, PROMS)

# recode factors
Data$MType <- recode(Data$MType, ACC_all = "Averaged", ACC_full = "Full", ACC_f0 = "F0", ACC_tbr = "Timbre")
Data$MType <- factor(Data$MType, levels = c("Averaged", "Full", "F0", "Timbre"))

Data$Test <- recode(Data$Test, PROMS_mean = "PROMS_Averaged", pitch = "Pitch", melody = "Melody", timbre = "Timbre", rhythm = "Rhythm")
Data$Test <- factor(Data$Test, levels = c("PROMS_Averaged", "Pitch", "Melody", "Timbre", "Rhythm"))

Data$Group <- factor(Data$Group, levels = c("M","A","C"))


# format Cor
Cor$p_corrected2 <- round(Cor$p_corrected2, 3)
Cor$pplot <- paste0("= ", Cor$p_corrected2)
Cor$pplot <- ifelse(Cor$pplot == "= 0", "< 0.001", Cor$pplot)

Cor$sig <- ifelse(Cor$p_corrected2 < 0.05, "bold.italic", "plain")


############################################

title = paste0("Correlation between Emotion Classification Performance and Music Perception Abilities, Amateurs only (N = 88)")

filename = paste0("plots/allgroups_04c_correlations_ACC_PROMS_Ama_only.png")

p<-(ggplot(data= Data, aes(x = confidence, y=ACC)) +
      geom_point(shape= 18, size = 3, color = "darkgreen") +
      labs(x = "Music Perception Abilities (Confidence)" , y = "Emotion Classification Performance" , title = title, color = "") +       #, title = title
      facet_grid(rows = vars(MType), cols = vars(Test), switch = "y") + #scales = "free"
      geom_smooth(method='lm', formula= y~x, color = "black") + 
      geom_text(data= Cor, 
                aes(label= paste0("r = ", round(r, 2), "\n p ", pplot), x = 0.1, y = 0.85, fontface = sig), color = "black") +
      scale_y_continuous(position = "right") +
      scale_x_continuous(limits=c(0, 0.6), breaks = c(0.2, 0.4, 0.6)) + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            legend.position="bottom", 
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14),
            strip.text.y = element_text(size = 14),
            legend.text = element_text(size=14)))
ggsave(filename, width = 15, height = 12, dpi =300)

#tidy environment completely
rm(list=ls())


#---------------------------------------------------------------------------------#
#       2. Correlational Analyses - controlled for formal musical education       #
#---------------------------------------------------------------------------------#

#reload all data
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

## Partial correlations: 

Comb <- expand.grid(c("ACC_all", "ACC_full", "ACC_f0", "ACC_tbr"),c("PROMS_mean", "pitch", "melody", "timbre", "rhythm"))

cor_results <- Comb # create "empty" results data frame
cor_results$cor <- NA
cor_results$p <- NA
cor_results$N <- 166

# (1) PROMS Correlations controlled for Music Education

for(i in 1:length(Comb$Var1)) {
  #i = 2
  cordat <- Corrdata %>% dplyr::select(Comb$Var1[i], Comb$Var2[i], Mean_education)
  cor <- pcor(cordat, method = "spearman")  # this Correlation holds
  cor_results$cor[i] <-  cor$estimate[1,2]
  cor_results$p[i] <- cor$p.value[1,2]
}
Cor <- cor_results
names(Cor) <- c("MType", "Test", "Cor", "p", "N")
rm(cor, Comb, cordat, cor_results)



# tidy dataset
Cor$MType <- recode(Cor$MType, ACC_all = "Averaged", ACC_full = "Full", ACC_f0 = "F0", ACC_tbr = "Timbre")
Cor$MType <- factor(Cor$MType, levels = c("Averaged", "Full", "F0", "Timbre"))

Cor$Test <- recode(Cor$Test, PROMS_mean = "PROMS_Averaged", pitch = "Pitch", melody = "Melody", timbre = "Timbre", rhythm = "Rhythm")
Cor$Test <- factor(Cor$Test, levels = c("PROMS_Averaged", "Pitch", "Melody", "Timbre", "Rhythm"))

#perform the multiple comparison correction: 
#https://www.youtube.com/watch?v=rZKa4tW2NKs
FDR = 0.05  # set false discovery rate to 5%
Cor <- Cor %>% arrange(p) #order the dataset by p-value
Cor$i <- c(1:20) # assign rank of p-value
m <- 20  # assing number of tests 
Cor$CritValue <- (Cor$i / m) * FDR  #(i/m)*Q  # correct the critical value
Cor$p_corrected <- Cor$p * (m / Cor$i)        # correct the p-value itself

#perform adjustement of the corrected value: 
Cor$p_corrected2 <- Cor$p_corrected 

for (j in (m-1):1){
  Cor$p_corrected2[j] <- ifelse(Cor$p_corrected[j] > Cor$p_corrected2[j+1], Cor$p_corrected2[j+1], Cor$p_corrected[j] )
}

# save the dataset
capture.output(as.matrix(Cor), file="output/emotion_classification/allgroups_C_Correlations_ACC_PROMS_c-Education.txt")

rm(Cor)

#---------------------------------------------------------------------------------#
#             3. Logistic regression - uncontrolled                               #
#---------------------------------------------------------------------------------#
#I used these resources: https://www.r-bloggers.com/2021/02/how-to-run-logistic-regression-on-aggregate-data-in-r/

Corrdata$n <- 24

Comb <- expand.grid(c("ACC_all", "ACC_full", "ACC_f0", "ACC_tbr"),c("PROMS_mean", "pitch", "melody", "timbre", "rhythm"))

log_results <- Comb # create "empty" results data frame
log_results$coeff <- NA
log_results$p <- NA
log_results$N <- 166

#run logistic regression: 
for(i in 1:length(Comb$Var1)) {
  mod <- as.formula(paste(Comb$Var1[i], "~", Comb$Var2[i]))
  m<-glm(formula = mod, data=Corrdata, weights = n, family = quasibinomial("logit"))
  sum_m <- summary(m)
  log_results$coeff[i] <-  m$coefficients[2]
  log_results$p[i] <- sum_m$coefficients[2,4]
}


Log <- log_results
names(Log) <- c("MType", "Test", "Log", "p", "N")
rm(Comb,log_results, m, sum_m)

# tidy dataset
Log$MType <- recode(Log$MType, ACC_all = "Averaged", ACC_full = "Full", ACC_f0 = "F0", ACC_tbr = "Timbre")
Log$MType <- factor(Log$MType, levels = c("Averaged", "Full", "F0", "Timbre"))

Log$Test <- recode(Log$Test, PROMS_mean = "PROMS_Averaged", pitch = "Pitch", melody = "Melody", timbre = "Timbre", rhythm = "Rhythm")
Log$Test <- factor(Log$Test, levels = c("PROMS_Averaged", "Pitch", "Melody", "Timbre", "Rhythm"))



#perform the multiple comparison correction: 
#https://www.youtube.com/watch?v=rZKa4tW2NKs
FDR = 0.05  # set false discovery rate to 5%
Log <- Log %>% arrange(p) #order the dataset by p-value
Log$i <- c(1:20) # assign rank of p-value
m <- 20  # assing number of tests 
Log$CritValue <- (Log$i / m) * FDR  #(i/m)*Q  # correct the critical value
Log$p_corrected <- Log$p * (m / Log$i)        # correct the p-value itself

#perform adjustement of the corrected value: 
Log$p_corrected2 <- Log$p_corrected 

for (j in (m-1):1){
  Log$p_corrected2[j] <- ifelse(Log$p_corrected[j] > Log$p_corrected2[j+1], Log$p_corrected2[j+1], Log$p_corrected[j] )
}

# save the dataset
capture.output(as.matrix(Log), file="output/emotion_classification/allgroups_C_LogisticReg_ACC_PROMS.txt")

rm(Log)

#---------------------------------------------------------------------------------#
#       4. Logistic regression - controlled for formal musical education          #
#---------------------------------------------------------------------------------#
Corrdata$n <- 24

Comb <- expand.grid(c("ACC_all", "ACC_full", "ACC_f0", "ACC_tbr"),c("PROMS_mean", "pitch", "melody", "timbre", "rhythm"))

log_results <- Comb # create "empty" results data frame
log_results$coeff <- NA
log_results$p <- NA
log_results$N <- 166

# logistic regression with an effect estimated for musical education

for(i in 1:length(Comb$Var1)) {
  mod <- as.formula(paste(Comb$Var1[i], "~", Comb$Var2[i], "+ Mean_education"))
  m<-glm(formula = mod, data=Corrdata, weights = n, family = quasibinomial("logit"))
  sum_m <- summary(m)
  log_results$coeff[i] <-  m$coefficients[2]
  log_results$p[i] <- sum_m$coefficients[2,4]
}

Log <- log_results
names(Log) <- c("MType", "Test", "Log", "p", "N")
rm(Comb, log_results, m, sum_m)



# tidy dataset
Log$MType <- recode(Log$MType, ACC_all = "Averaged", ACC_full = "Full", ACC_f0 = "F0", ACC_tbr = "Timbre")
Log$MType <- factor(Log$MType, levels = c("Averaged", "Full", "F0", "Timbre"))

Log$Test <- recode(Log$Test, PROMS_mean = "PROMS_Averaged", pitch = "Pitch", melody = "Melody", timbre = "Timbre", rhythm = "Rhythm")
Log$Test <- factor(Log$Test, levels = c("PROMS_Averaged", "Pitch", "Melody", "Timbre", "Rhythm"))

#perform the multiple comparison correction: 
#https://www.youtube.com/watch?v=rZKa4tW2NKs
FDR = 0.05  # set false discovery rate to 5%
Log <- Log %>% arrange(p) #order the dataset by p-value
Log$i <- c(1:20) # assign rank of p-value
m <- 20  # assing number of tests 
Log$CritValue <- (Log$i / m) * FDR  #(i/m)*Q  # correct the critical value
Log$p_corrected <- Log$p * (m / Log$i)        # correct the p-value itself

#perform adjustement of the corrected value: 
Log$p_corrected2 <- Log$p_corrected 

for (j in (m-1):1){
  Log$p_corrected2[j] <- ifelse(Log$p_corrected[j] > Log$p_corrected2[j+1], Log$p_corrected2[j+1], Log$p_corrected[j] )
}

# save the dataset
capture.output(as.matrix(Log), file="output/emotion_classification/allgroups_C_LogisticReg_ACC_PROMS_c-Education.txt")


### End of Script