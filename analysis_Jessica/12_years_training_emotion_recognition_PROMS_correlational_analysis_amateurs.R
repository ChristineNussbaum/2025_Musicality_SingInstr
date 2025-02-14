##########################################################################
## File: 12_years_training_emotion_recognition_PROMS_correlational_analysis_amateurs.R
## This script runs correlational analyses on years participant practiced (trained) 
#      and took music lessons with the emotion recognition data and PROMS data
# author: Jessica Senftleben
# date 02/2024

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
## Meaning of Variables

# YearsTraining:  Gold-MSI, Question 32: 
#                 "Ich habe regelmäßig und täglich ein Instrument 
#                 (einschließlich Gesang) für ...  Jahre geübt."
# YearsLessons:   Gold-MSI, Question 36: 
#                 "Ich habe ... Jahre Musikunterricht auf einem Instrument 
#                 (einschließlich Gesang) in meinem bisherigen Leben gehabt."


#---------------------------------------------------------------------------------
#       Prepare data sets
#---------------------------------------------------------------------------------
#get the preprocessed data: 
load(file="input/amateurs_Corrdata_MSI.RData")
MSI <- Corrdata

# load relevant variable from Raw MSI data
load(file="input/amateurs_Corrdata_MSI_YearsTraining.RData")

# correct data set to match Corrdata: remove participants 
MLessons <- subset(MLessons, participant!="c7e7e936") # singer EHM92A
MLessons <- subset(MLessons, participant!="ecd4a2cb") # singer SCS58E
MLessons <- subset(MLessons, participant!="08eba336") # instrumentalist AIG27E
MLessons <- subset(MLessons, participant!="27e3b37a") # singer IHP42U
MLessons <- subset(MLessons, participant!="a67e3eb6") # NIR46E
MLessons <- subset(MLessons, participant!="1dd171b3") # AEB56L instrumentalist

# correct to numeric
MLessons <- MLessons %>%  mutate(YearsTraining = as.numeric(YearsTraining))
MLessons <- MLessons %>%  mutate(YearsLessons = as.numeric(YearsLessons))

# merge MSI and MLessons
MSI <- merge(MLessons,MSI)

# remove participant column (don't need it anymore)
MSI <- MSI[,-c(2)]

# load other data sets
load(file="input/amateurs_Corrdata_PROMS.RData")
PROMS <- Corrdata
load(file="input/amateurs_Corrdata_OCEAN_AQ.RData")
OCEAN <- Corrdata

#merge the data
Corrdata <- merge(PROMS, MSI)
Corrdata <- merge(Corrdata, OCEAN)

rm(MSI, PROMS, OCEAN, MLessons)

#remove the Variables we are not interested in: 
Corrdata <- Corrdata[,-c(6:8, 10)]

#---------------------------------------------------------------------------------
# Correlation with emotion recognition
#---------------------------------------------------------------------------------

Correlations_ACC <- rcorr(as.matrix(Corrdata[,c(6, 3:5, 12:13)]), type="spearman") # correlate with ACC 

# prepare them to be saved and plotted
Cor_r <- data.frame(Correlations_ACC$r[1:4, 5:6])
Cor_p <- data.frame(Correlations_ACC$P[1:4, 5:6])
Cor_n <- Correlations_ACC$n[1:1]

Cor_r$MType <- rownames(Cor_r)
Cor_p$MType <- rownames(Cor_p)

#convert to long format

Cor_r = Cor_r %>% pivot_longer(cols = YearsTraining:YearsLessons, names_to = "Question", values_to = "r")
Cor_p = Cor_p %>% pivot_longer(cols = YearsTraining:YearsLessons, names_to = "Question", values_to = "p")

Cor <-merge(Cor_r, Cor_p)
Cor$N <- Cor_n
rm(Cor_r, Cor_p, Cor_n)


#perform the multiple comparison correction: 
#https://www.youtube.com/watch?v=rZKa4tW2NKs
FDR = 0.05  # set false discovery rate to 5%
Cor <- Cor %>% arrange(p) #order the dataset by p-value
Cor$i <- c(1:8) # assign rank of p-value
m <- 8  # assing number of tests 
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
Cor$Question <- recode(Cor$Question, YearsTraining = "Music Training", YearsLessons = "Music Lessons")
Cor$Question <- factor(Cor$Question, levels = c("Music Training", "Music Lessons"))


# save the dataset
capture.output(as.matrix(Cor), file="output/emotion_classification/amateurs_C_Correlations_YearsTraining_ACC.txt")

rm(Correlations_ACC)

#---------------------------------------------------------------------------------#
#              Data Visualization of VER x Music Training/Lessons                                   #
#---------------------------------------------------------------------------------#

# plot a 4 x 2 matrix with all the correlations and Scatterplots

Emo <- Corrdata[, c(1:6)] %>% pivot_longer(cols = ACC_f0:ACC_all, names_to = "MType", values_to = "ACC")
Years <- Corrdata[, c(1,2,12:13)] %>% pivot_longer(cols = YearsTraining:YearsLessons, names_to = "Question", values_to = "Years")

Data <- merge(Emo, Years)

rm(Emo, Years)

# recode factors
Data$MType <- recode(Data$MType, ACC_all = "Averaged", ACC_full = "Full", ACC_f0 = "F0", ACC_tbr = "Timbre")
Data$MType <- factor(Data$MType, levels = c("Averaged", "Full", "F0", "Timbre"))

Data$Question <- recode(Data$Question, YearsTraining = "Music Training", YearsLessons = "Music Lessons")
Data$Question <- factor(Data$Question, levels = c("Music Training", "Music Lessons"))

Data$Group <- factor(Data$Group, levels = c("1", "2"))

# format Cor
Cor$p_corrected2 <- round(Cor$p_corrected2, 3)
Cor$pplot <- paste0("= ", Cor$p_corrected2)
Cor$pplot <- ifelse(Cor$pplot == "= 0", "< 0.001", Cor$pplot)

Cor$pplot
Cor$sig <- ifelse(Cor$p_corrected2 < 0.05, "bold.italic", "plain")


############################################

title = paste0("Correlation between Emotion Classification and Musical Training and Lessons (N = 88)")

filename = paste0("plots/amateurs_05d_correlations_ACC_YearsTraining.png")

p<-(ggplot(data= Data, aes(x = Years, y=ACC)) +
      geom_point(aes(color = Group), shape= 18, size = 3) +
      labs(y = "Emotion Classification Performance" , x = "Years" , title = title, color = "") +       #, title = title
      facet_grid(rows = vars(MType), cols = vars(Question), switch = "y", ) + #scales = "free"
      scale_color_manual(labels = c("Singers", "Instrumentalists"), values = c("green3", "darkgreen")) +
      geom_smooth(method='lm', formula= y~x, color = "black") + 
      geom_text(data= Cor, 
                aes(label= paste0("r = ", round(r, 2), "\n p ", pplot), x = 4, y = 0.8, fontface = sig), color = "black") +
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
ggsave(filename, width = 8, height = 12, dpi =300)

rm(Cor,Data,p)

#---------------------------------------------------------------------------------
# Correlation with PROMS
#---------------------------------------------------------------------------------

Correlations_PROMS <- rcorr(as.matrix(Corrdata[,c(11, 7:10, 12:13)]), type="spearman") # correlate with PROMS 

# prepare them to be saved and plotted
Cor_r <- data.frame(Correlations_PROMS$r[1:5, 6:7])
Cor_p <- data.frame(Correlations_PROMS$P[1:5, 6:7])
Cor_n <- Correlations_PROMS$n[1:1]

Cor_r$Test <- rownames(Cor_r)
Cor_p$Test<- rownames(Cor_p)

#convert to long format
Cor_r = Cor_r %>% pivot_longer(cols = YearsTraining:YearsLessons, names_to = "Question", values_to = "r")
Cor_p = Cor_p %>% pivot_longer(cols = YearsTraining:YearsLessons, names_to = "Question", values_to = "p")

Cor <-merge(Cor_r, Cor_p)
Cor$N <- Cor_n
rm(Cor_r, Cor_p, Cor_n)


#perform the multiple comparison correction: 
#https://www.youtube.com/watch?v=rZKa4tW2NKs
FDR = 0.05  # set false discovery rate to 5%
Cor <- Cor %>% arrange(p) #order the dataset by p-value
Cor$i <- c(1:10) # assign rank of p-value
m <- 10  # assing number of tests 
Cor$CritValue <- (Cor$i / m) * FDR  #(i/m)*Q  # correct the critical value
Cor$p_corrected <- Cor$p * (m / Cor$i)        # correct the p-value itself

#perform adjustement of the corrected value: 
Cor$p_corrected2 <- Cor$p_corrected 

for (j in (m-1):1){
  Cor$p_corrected2[j] <- ifelse(Cor$p_corrected[j] > Cor$p_corrected2[j+1], Cor$p_corrected2[j+1], Cor$p_corrected[j] )
}

# tidy dataset
Cor$Test <- recode(Cor$Test, PROMS_mean = "PROMS_Averaged", pitch = "Pitch", melody = "Melody", timbre = "Timbre", rhythm = "Rhythm")
Cor$Test <- factor(Cor$Test, levels = c("PROMS_Averaged", "Pitch", "Melody", "Timbre", "Rhythm"))
Cor$Question <- recode(Cor$Question, YearsTraining = "Music Training", YearsLessons = "Music Lessons")
Cor$Question <- factor(Cor$Question, levels = c("Music Training", "Music Lessons"))

# save the dataset
capture.output(as.matrix(Cor), file="output/emotion_classification/amateurs_C_Correlations_YearsTraining_PROMS.txt")

rm(Correlations_PROMS)

#---------------------------------------------------------------------------------#
#                   Data Visualization of PROMS x Music Training/Lessons                                 #
#---------------------------------------------------------------------------------#

# plot a 4 x 2 matrix with all the correlations and Scatterplots

PROMS <- Corrdata[, c(1,2,7:11)] %>% pivot_longer(cols = melody:PROMS_mean, names_to = "Test", values_to = "Confidence")
Years <- Corrdata[, c(1,2,12:13)] %>% pivot_longer(cols = YearsTraining:YearsLessons, names_to = "Question", values_to = "Years")

Data <- merge(Years,PROMS)

rm(Years, PROMS)

# recode factors
Data$Test <- recode(Data$Test, PROMS_mean = "PROMS_Averaged", pitch = "Pitch", melody = "Melody", timbre = "Timbre", rhythm = "Rhythm")
Data$Test <- factor(Data$Test, levels = c("PROMS_Averaged", "Pitch", "Melody", "Timbre", "Rhythm"))

Data$Question <- recode(Data$Question, YearsTraining = "Music Training", YearsLessons = "Music Lessons")
Data$Question <- factor(Data$Question, levels = c("Music Training", "Music Lessons"))

Data$Group <- factor(Data$Group, levels = c("1", "2"))

# format Cor
Cor$p_corrected2 <- round(Cor$p_corrected2, 3)
Cor$pplot <- paste0("= ", Cor$p_corrected2)
Cor$pplot <- ifelse(Cor$pplot == "= 0", "< 0.001", Cor$pplot)

Cor$pplot
Cor$sig <- ifelse(Cor$p_corrected2 < 0.05, "bold.italic", "plain")


############################################

title = paste0("Correlation between PROMS and Music Training and Lessons (N = 88)")

filename = paste0("plots/amateurs_05e_correlations_PROMS_YearsTraining.png")

p<-(ggplot(data= Data, aes(x = Years, y=Confidence)) +
      geom_point(aes(color = Group), shape= 18, size = 3) +
      labs(y = "Emotion Classification Performance" , x = "Years" , title = title, color = "") +       #, title = title
      facet_grid(rows = vars(Test), cols = vars(Question), switch = "y", ) + #scales = "free_x"
      scale_color_manual(labels = c("Singers", "Instrumentalists"), values = c("green3", "darkgreen")) +
      geom_smooth(method='lm', formula= y~x, color = "black") + 
      geom_text(data= Cor, 
                aes(label= paste0("r = ", round(r, 2), "\n p ", pplot), x = 4, y = 0.6, fontface = sig), color = "black") +
      scale_y_continuous(position = "right", limits=c(0, 0.7), breaks=c(0.2, 0.4, 0.6)) + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            legend.position="bottom", 
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14),
            strip.text.y = element_text(size = 14),
            legend.text = element_text(size=14)))
ggsave(filename, width = 8, height = 12, dpi =300)


# End of Script
