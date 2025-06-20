##########################################################################
## File: 03a_correlational_analysis_amateurs_PROMS_partII.R
## This script runs some correlational analyses on the emotion recognition data with the PROMS
# authors: Christine Nussbaum (christine.nussbaum@uni-jena.de), Jessica Senftleben
# date 07/2023, 02/2024, 05/2025

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

load(file="input/amateurs_Corrdata_PROMS.RData")


## Meaning of Variables

# Subject                       individual participant code assigned by PsyToolkit
# Code                          self-generated Code by the participant
# Group                         group - 1 = singers; 2 = instrumentalists 
# ACC_f0:ACC_tbr                Accuracy in the F0, Full and Timbre condition
# RT_f0:RT_tbr                  Reaction time in the F0, Full and Timbre condition
# ACC_all                       Accuracy averaged across all emotions
# RT_all                        Reaction time averaged across all emotions
# melody:timbre                 Performance (confidence) in the PROMS subtests
# PROMS_mean                    Performance (confidence) averaged across all PROMS subtests


#---------------------------------------------------------------------------------#
#              1. PROMS Correlational Analyses - uncontrolled                     #
#---------------------------------------------------------------------------------#


Correlations_ACC <- rcorr(as.matrix(Corrdata[,c(9, 3:5, 11:15)]), type="spearman") # calculate correlations for confidence ratings

# prepare correlations_ACC to be saved and plotted

Cor_r <- data.frame(Correlations_ACC$r[1:4, 5:9])
Cor_p <- data.frame(Correlations_ACC$P[1:4, 5:9])
Cor_n <- Correlations_ACC$n[1:1]

Cor_r$MType <- rownames(Cor_r)
Cor_p$MType <- rownames(Cor_p)

#convert to long format
Cor_r = Cor_r %>% pivot_longer(cols = melody:PROMS_mean, names_to = "Test", values_to = "r")
Cor_p = Cor_p %>% pivot_longer(cols = melody:PROMS_mean, names_to = "Test", values_to = "p")

Cor <- merge(Cor_r, Cor_p)
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
capture.output(as.matrix(Cor), file="output/emotion_classification/amateurs_C_Correlations_ACC_PROMS.txt") 

#---------------------------------------------------------------------------------#
#                            Data Visualization                                   #
#---------------------------------------------------------------------------------#

# plot a 4 x 5 matrix with all the correlations and Scatterplots

# prepare dataset
Corrdata <- Corrdata[, -c(6,7,8,10)]

Emo <- Corrdata[, c(1:6)] %>% pivot_longer(cols = ACC_f0:ACC_all, names_to = "MType", values_to = "ACC")
PROMS <- Corrdata[, c(1,2,7:11)] %>% pivot_longer(cols = melody:PROMS_mean, names_to = "Test", values_to = "Confidence")

Data <- merge(Emo, PROMS)

rm(Emo, PROMS)

# recode factors
Data$MType <- recode(Data$MType, ACC_all = "Averaged", ACC_full = "Full", ACC_f0 = "F0", ACC_tbr = "Timbre")
Data$MType <- factor(Data$MType, levels = c("Averaged", "Full", "F0", "Timbre"))

Data$Test <- recode(Data$Test, PROMS_mean = "PROMS_Averaged", pitch = "Pitch", melody = "Melody", timbre = "Timbre", rhythm = "Rhythm")
Data$Test <- factor(Data$Test, levels = c("PROMS_Averaged", "Pitch", "Melody", "Timbre", "Rhythm"))

Data$Group <- factor(Data$Group, levels = c("1","2"))


# format Cor
Cor$p_corrected2 <- round(Cor$p_corrected2, 3)
Cor$pplot <- paste0("= ", Cor$p_corrected2)
Cor$pplot <- ifelse(Cor$pplot == "= 0", "< 0.001", Cor$pplot)

Cor$pplot
Cor$sig <- ifelse(Cor$p_corrected2 < 0.05, "bold.italic", "plain")


############################################

title = paste0("Correlation between Emotion Classification Performance and Music Perception Abilities (N = 88)")

filename = paste0("plots/amateurs_05_correlations_ACC_PROMS_Accuracy.png")

p<-(ggplot(data= Data, aes(x = Confidence, y=ACC)) +
      geom_point(aes(color = Group), shape= 18, size = 3) +
      labs(x = "Music Perception Abilities (Confidence)" , y = "Emotion Classification Performance" , title = title, color = "") +       #, title = title
      facet_grid(rows = vars(MType), cols = vars(Test), switch = "y") + #scales = "free"
      scale_color_manual(labels = c("Singers", "Instrumentalists" ), values = c("green3", "darkgreen")) +
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


#clean complete environment
rm(list=ls())

#---------------------------------------------------------------------------------#
#    2. PROMS Correlational Analyses - controlled for formal musical education    #
#---------------------------------------------------------------------------------#

#reload all data
load(file="input/amateurs_Corrdata_PROMS.RData")
PROMS <- Corrdata
load(file="input/amateurs_Corrdata_MSI.RData")
MSI <- Corrdata


#merge the data

Corrdata <- merge(PROMS, MSI)


rm(MSI, PROMS)

#remove the Variables we are not interested in (RT): 
Corrdata <- Corrdata[,-c(6:8, 10)]

## Partial correlations: 
Comb <- expand.grid(c("ACC_all", "ACC_full", "ACC_f0", "ACC_tbr"),c("PROMS_mean", "pitch", "melody", "timbre", "rhythm"))

cor_results <- Comb # create "empty" results data frame
cor_results$cor <- NA
cor_results$p <- NA
cor_results$N <- 88

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
  Cor$p_corrected2[j] <- ifelse(Cor$p_corrected[j] > Cor$p_corrected[j+1], Cor$p_corrected2[j+1], Cor$p_corrected[j] )
}

# save the dataset
capture.output(as.matrix(Cor), file="output/emotion_classification/amateurs_C_Correlations_ACC_PROMS_c-Education.txt")

rm(Cor)

#clean complete environment
rm(list=ls())


#---------------------------------------------------------------------------------#
#              3. Gold-MSI Correlational Analyses - uncontrolled                  #
#---------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------
#get the preprocessed/prepared data: 

load(file="input/amateurs_Corrdata_MSI.RData")


## Meaning of Variables

# Subject                       individual participant code assigned by PsyToolkit
# Code                          self-generated Code by the participant
# Group                         group - 1 = singers; 2 = instrumentalists 
# ACC_f0:ACC_tbr                Accuracy in the F0, Full and Timbre condition
# RT_f0:RT_tbr                  Reaction time in the F0, Full and Timbre condition
# ACC_all                       Accuracy averaged across all emotions
# RT_all                        Reaction time averaged across all emotions
# ME_mean                       General Musical Sophistication index
# Mean_active:Mean_singing      Scores in the Gold-MSI subfactors


Correlations_ACC <- rcorr(as.matrix(Corrdata[,c(9, 3:5, 11:16)]), type="spearman") # correlate with ACC 


# prepare them to be saved and plotted

Cor_r <- data.frame(Correlations_ACC$r[1:4, 5:10])
Cor_p <- data.frame(Correlations_ACC$P[1:4, 5:10])
Cor_n <- Correlations_ACC$n[1:1]

Cor_r$MType <- rownames(Cor_r)
Cor_p$MType <- rownames(Cor_p)

#convert to long format

Cor_r = Cor_r %>% pivot_longer(cols = ME_mean:Mean_singing, names_to = "Test", values_to = "r")
Cor_p = Cor_p %>% pivot_longer(cols = ME_mean:Mean_singing, names_to = "Test", values_to = "p")

Cor <-merge(Cor_r, Cor_p)
Cor$N <- Cor_n
rm(Cor_r, Cor_p, Cor_n)

#none of the p-values survive correction


# tidy dataset
Cor$MType <- recode(Cor$MType, ACC_all = "Averaged", ACC_full = "Full", ACC_f0 = "F0", ACC_tbr = "Timbre")
Cor$MType <- factor(Cor$MType, levels = c("Averaged", "Full", "F0", "Timbre"))

Cor$Test <- recode(Cor$Test, ME_mean = "General ME", Mean_active = "Active", Mean_perception = "Perception", Mean_singing = "Singing", Mean_emotion = "Emotion", Mean_education = "Education")
Cor$Test <- factor(Cor$Test, levels = c("General ME", "Active", "Perception", "Singing", "Emotion", "Education"))

# save the dataset
capture.output(as.matrix(Cor), file="output/emotion_classification/amateurs_C_Correlations_ACC_MSI.txt")



#---------------------------------------------------------------------------------#
#                            Data Visualization                                   #
#---------------------------------------------------------------------------------#

# plot a 4 x 6 matrix with all the correlations and Scatterplots

# prepare dataset
Corrdata <- Corrdata[, -c(6,7,8,10)]

Emo <- Corrdata[, c(1:6)] %>% pivot_longer(cols = ACC_f0:ACC_all, names_to = "MType", values_to = "ACC")
MSI <- Corrdata[, c(1,2,7:12)] %>% pivot_longer(cols = ME_mean:Mean_singing, names_to = "Test", values_to = "Score")

Data <- merge(Emo, MSI)

rm(Emo, MSI)

# recode factors
Data$MType <- recode(Data$MType, ACC_all = "Averaged", ACC_full = "Full", ACC_f0 = "F0", ACC_tbr = "Timbre")
Data$MType <- factor(Data$MType, levels = c("Averaged", "Full", "F0", "Timbre"))

Data$Test <- recode(Data$Test, ME_mean = "General ME", Mean_active = "Active", Mean_perception = "Perception", Mean_singing = "Singing", Mean_emotion = "Emotion", Mean_education = "Education")
Data$Test <- factor(Data$Test, levels = c("General ME", "Active", "Perception", "Singing", "Emotion", "Education"))

Data$Group <- factor(Data$Group, levels = c("1", "2"))

# format Cor

Cor$p <- round(Cor$p, 3)
Cor$pplot <- paste0("= ", Cor$p)
Cor$pplot <- ifelse(Cor$pplot == "= 0", "< 0.001", Cor$pplot)

Cor$pplot
Cor$sig <- ifelse(Cor$p < 0.05, "bold.italic", "plain")


############################################

title = paste0("Correlation between Emotion Classification Performance and Gold-MSI (N = 88)")

filename = paste0("plots/amateurs_05b_correlations_ACC_MSI.png")

p<-(ggplot(data= Data, aes(x = Score, y=ACC)) +
      geom_point(aes(color = Group), shape= 18, size = 3) +
      labs(y = "Emotion Classification Performance" , x = "Gold-MSI" , title = title, color = "") +       #, title = title
      facet_grid(rows = vars(MType), cols = vars(Test), switch = "y", ) + #scales = "free_x"
      scale_color_manual(labels = c("Singers", "Instrumentalists"), values = c("green3", "darkgreen")) +
      geom_smooth(method='lm', formula= y~x, color = "black") + 
      geom_text(data= Cor, 
                aes(label= paste0("r = ", round(r, 2), "\n p ", pplot), x = 2, y = 0.85, fontface = sig), color = "black") +
      scale_y_continuous(position = "right") + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            legend.position="bottom", 
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14),
            strip.text.y = element_text(size = 14),
            legend.text = element_text(size=14)))
ggsave(filename, width = 17, height = 12, dpi =300)

#remove(yTitleStr, xTitleStr, title, filename, p)

rm(Cor, Data, p)


#---------------------------------------------------------------------------------#
#       4. Gold-MSI Correlational Analyses - controlled for formal musical education       #
#---------------------------------------------------------------------------------#


## Partial correlations: 
Comb <- expand.grid(c("ACC_all", "ACC_full", "ACC_f0", "ACC_tbr"),c("ME_mean", "Mean_active", 
                                                                    "Mean_emotion", "Mean_perception", "Mean_singing"))

cor_results <- Comb # create "empty" results data frame
cor_results$cor <- NA
cor_results$p <- NA
cor_results$N <- 88

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

Cor$Test <- recode(Cor$Test, ME_mean = "General ME", Mean_active = "Active", Mean_perception = "Perception", Mean_singing = "Singing", Mean_emotion = "Emotion")
Cor$Test <- factor(Cor$Test, levels = c("General ME", "Active", "Perception", "Singing", "Emotion"))

# save the dataset
capture.output(as.matrix(Cor), file="output/emotion_classification/amateurs_C_Correlations_ACC_MSI_c-Education.txt")

#none of the p-values are significant

rm(Cor)

### End of Script