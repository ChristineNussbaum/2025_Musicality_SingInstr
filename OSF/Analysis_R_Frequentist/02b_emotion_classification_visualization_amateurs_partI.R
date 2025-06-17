##########################################################################
## File: 02b_emotion_classification_visualization_amateurs_partI.R
## This script analysis the emotion recognition performance of singers and instrumentalists
# authors: Christine Nussbaum (christine.nussbaum@uni-jena.de), Jessica Senftleben
# date 7/2023, 05/2025


# clear directory
rm(list=ls())

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# load required packages
library(tidyverse) # version 1.3.1

# load relevant functions
source("functions/mySummary.R") 

# set some relevant setting 
options(scipen = 999)


#---------------------------------------------------------------------------------
#get the preprocessed data: 
#Experiment data (after some preprocessing, e.g. removing participants with too many omissions)
load(file="input/amateurs_Exp_processed.RData")

# remove participant with degree in music science
D2 <- subset(D, Code!="AEB56L")

#rename D2 in D to make script easier to read
D <- D2
rm(D2)

#-> for meaning of variables, refer to "02a_emotion_classification_data_analysis_amateurs_partI.R"

##################################################################################################################
# aggregate dataset, for ACC only 

#remove the averaged stimuli
D <- D %>% filter(Emo != "avg")

B <- mySummary(D, ACC, Subject, Emo, MType, Group) # 88 participants * 4 emotions * 3 MTypes
A <- mySummary(B, ACC, Emo, MType, Group) # 4 emotions * 3 MTypes * 2 Groups


#-----------------------------------------------------------------------------------#
#                 Plot 1: Proportion of correct classifications                      #
#-----------------------------------------------------------------------------------#

yTitleStr = "Proportion of Correct Classifications"
xTitleStr = "Morph Type"

title = paste0("Mean ", yTitleStr, " per ", xTitleStr, " (N =", length(unique(D$Subject))," )")

filename = paste0("plots/amateurs_01_means_prop_correct_Emo_MType.png")

A$MType <- recode(A$MType, full = "Full", f0 = "F0", tbr = "Timbre")
B$MType <- recode(B$MType, full = "Full", f0 = "F0", tbr = "Timbre")
A$Emo <- recode(A$Emo, hap = "Happiness", ple = "Pleasure", fea = "Fear", sad = "Sadness")
B$Emo <- recode(B$Emo, hap = "Happiness", ple = "Pleasure", fea = "Fear", sad = "Sadness")
A$Group <- recode(A$Group, "1" = "Singers", "2" = "Instrumentalists")
B$Group <- recode(B$Group, "1" = "Singers", "2" = "Instrumentalists")

# bring factors in order
A$Emo <- factor(A$Emo, levels = c("Happiness", "Pleasure", "Fear", "Sadness"))
B$Emo <- factor(B$Emo, levels = c("Happiness", "Pleasure", "Fear", "Sadness"))

A$MType <- factor(A$MType, levels= c("Full", "F0", "Timbre"))
B$MType <- factor(B$MType, levels= c("Full", "F0", "Timbre"))


#[1]
p<-(ggplot(data= A, aes(x = MType, y=ACC, group = Group, color = Group)) +
      geom_point(shape= 18, size = 3) +
      geom_errorbar(aes(ymin = (ACC-CI), ymax = (ACC+CI)), width = 0.1 ) + 
      labs(x = xTitleStr , y = yTitleStr) +       #, title = title
      facet_wrap(~ Emo, ncol = 4) +
      geom_hline(yintercept = 0.25, linetype = 4) + theme_bw()+
      scale_color_manual(values = c("darkgreen", "green3")) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14)) + 
      scale_y_continuous(limits=c(0,1.0), breaks = c( 0.0, 0.2, 0.4, 0.6, 0.8, 1.0)))
ggsave(filename, width = 12, height = 4, dpi =300)



#-----------------------------------------------------------------------------------#
#                 Plot 2: Proportion of correct classifications for MType only      #
#-----------------------------------------------------------------------------------#


B <- mySummary(D, ACC, Subject,MType, Group) # 89 participants * 4 emotions * 3 MTypes
A <- mySummary(B, ACC, MType, Group) # 2 Groups * 3 MTypes


yTitleStr = "Proportion of Correct Classifications"
xTitleStr = "Morph Type"

title = paste0("Mean ", yTitleStr, " per ", xTitleStr, " (N =", length(unique(D$Subject))," )")

filename = paste0("plots/amateurs_02_means_prop_correctMType.png")

A$MType <- recode(A$MType, full = "Full", f0 = "F0", tbr = "Timbre")
B$MType <- recode(B$MType, full = "Full", f0 = "F0", tbr = "Timbre")
A$Group <- recode(A$Group, "1" = "Singers", "2" = "Instrumentalists")
B$Group <- recode(B$Group, "1" = "Singers", "2" = "Instrumentalists")

# bring factors in order
A$MType <- factor(A$MType, levels= c("Full", "F0", "Timbre"))
B$MType <- factor(B$MType, levels= c("Full", "F0", "Timbre"))

#[1]
p<-(ggplot(data= A, aes(x = MType, y=ACC, color = Group)) +
      geom_violin(data= B, aes(x = MType, y=ACC, fill = Group),color = NA,  position = position_dodge(width = 0.75), alpha = 0.2) +
      geom_point(shape= 18, size = 3, position = position_dodge(width = 0.75)) +
      geom_errorbar(aes(ymin = (ACC-CI), ymax = (ACC+CI)), width = 0.3 , position = position_dodge(width = 0.75)) + 
      labs(x = xTitleStr , y = yTitleStr) +       #, title = title
      geom_hline(yintercept = 0.25, linetype = 4) + theme_bw()+
      scale_color_manual(values = c("darkgreen", "green3")) +
      scale_fill_manual(values = c("darkgreen", "green3")) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14)) + 
      scale_y_continuous(limits=c(0,1.0), breaks = c( 0.0, 0.2, 0.4, 0.6, 0.8, 1.0)))
ggsave(filename, width = 6, height = 4, dpi =300)




#-----------------------------------------------------------------------------------#
#                 Plot 3: Proportion of correct classifications, without Groups     #
#-----------------------------------------------------------------------------------#


B <- mySummary(D, ACC, Subject, Emo, MType, Group) # 88 participants * 4 emotions * 3 MTypes
A <- mySummary(B, ACC, Emo, MType) # 4 emotions * 3 MTypes



yTitleStr = "Proportion of Correct Classifications"
xTitleStr = "Morph Type"

title = paste0("Mean ", yTitleStr, " per ", xTitleStr, " (N =", length(unique(D$Subject))," )")

filename = paste0("plots/amateurs_3_means_prop_Emo_MType.png")

A$MType <- recode(A$MType, full = "Full", f0 = "F0", tbr = "Timbre")
B$MType <- recode(B$MType, full = "Full", f0 = "F0", tbr = "Timbre")
A$Emo <- recode(A$Emo, hap = "Happiness", ple = "Pleasure", fea = "Fear", sad = "Sadness")
B$Emo <- recode(B$Emo, hap = "Happiness", ple = "Pleasure", fea = "Fear", sad = "Sadness")

# bring factors in order
A$MType <- factor(A$MType, levels= c("Full", "F0", "Timbre"))
B$MType <- factor(B$MType, levels= c("Full", "F0", "Timbre"))
A$Emo <- factor(A$Emo, levels = c("Happiness", "Pleasure", "Fear", "Sadness"))
B$Emo <- factor(B$Emo, levels = c("Happiness", "Pleasure", "Fear", "Sadness"))


p<-(ggplot(data= A, aes(x = MType, y=ACC)) +
      geom_jitter(data= B, aes(x = MType, y=ACC),  size = 1, shape = 16, position = position_jitter(0.1), color="grey")+ #color = Group
      geom_point(color = "black", shape= 18, size = 3) +
      geom_errorbar(aes(ymin = (ACC-CI), ymax = (ACC+CI)), width = 0.1 ) + 
      labs(x = xTitleStr , y = yTitleStr) +       #, title = title
      facet_wrap(~ Emo, ncol = 4) +
      geom_hline(yintercept = 0.25, linetype = 4) + theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text=element_text(size=14),
            axis.title=element_text(size=14), 
            axis.text.x = element_text(color = "black", size = 14), # angle = 45, hjust = 1.2, vjust =  1.2
            axis.text.y = element_text(color = "black", size = 14), 
            strip.text.x = element_text(size = 14)) + 
      scale_y_continuous(limits=c(0,1.0), breaks = c( 0.0, 0.2, 0.4, 0.6, 0.8, 1.0)))
ggsave(filename, width = 12, height = 4, dpi =300)

remove(yTitleStr, xTitleStr,title, filename, p)


### End of Script