##########################################################################
## File: 02c_emotion_classification_averages_analysis_and_confusion_data_partI.R
## This script analysis the emotion recognition performance of musicians and non-musicians - Analysis of AVG trials and Confusion matrices
# author: Christine Nussbaum (christine.nussbaum@uni-jena.de)
# date 10/2022 and 05/2025

#Done for revision, to check if individual stimuli were consistently misslassified

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
options(scipen = 999)

#---------------------------------------------------------------------------------
#get the preprocessed data: 
#Experiment data (after some preprocessing, e.g. removing participants with too many omissions)
load(file="input/amateurs_Exp_processed.RData")

#remove participant with a Degree in Music Science
D <- subset (D, Code!="AEB56L") # instrumentalist

#-> for meaning of variables, refer to "02a_emotion_classification_data_analysis_amateurs_partI.R"

#---------------------------------------------------------------------------------#
#                              Code the given Response                            #
#---------------------------------------------------------------------------------#

#information comes from Psytoolkit-Coding 
CB1 <- D %>% filter(CB == "CB1")
CB1$Emo_Resp <- recode_factor(CB1$Resp, `1` = "hap", `2` = "ple", `3` = "sad", `4` = "fea")

CB2 <- D %>% filter(CB == "CB2")
CB2$Emo_Resp <- recode_factor(CB2$Resp, `1` = "sad", `2` = "fea", `3` = "hap", `4` = "ple")

CB3 <- D %>% filter(CB == "CB3")
CB3$Emo_Resp <- recode_factor(CB3$Resp, `1` = "ple", `2` = "hap", `3` = "fea", `4` = "sad")

CB4 <- D %>% filter(CB == "CB4")
CB4$Emo_Resp <- recode_factor(CB4$Resp, `1` = "fea", `2` = "sad", `3` = "ple", `4` = "hap")


D <- rbind(CB1, CB2, CB3, CB4)

rm(CB1, CB2, CB3, CB4)


#save this dataset
save(D, file="input/Exp_processed_with_Resp.RData")

#---------------------------------------------------------------------------------#
#                        Analysis of Averaged Emotions                            #
#---------------------------------------------------------------------------------#
all <- D # copy of the data

# rmoved here

#--------------------------------------------------------------------------------------#
#                          Preprocessing of Confusion Data                             #
#--------------------------------------------------------------------------------------#

# get the whole dataset back: 
D <- all

#Revove avg trials

D <- D %>% filter(Emo != "avg")

#calculate accuracy for all stimuli
stimuli <- D %>% group_by(filename, MType, Emo) %>% summarise(ACC = mean(ACC))


#get range for each Emotion x Morph Type 

range <- mySummary(stimuli, ACC, MType, Emo)


#below 25% 

low <- stimuli %>% filter(ACC < 0.25)


#do the same for singer and instrumentalists only 

#calculate accuracy for all stimuli
stimuli2 <- D %>% group_by(filename, Group,  MType, Emo) %>% summarise(ACC = mean(ACC))

28/288

2/96

AVG <- C %>% filter(Emo == "avg") # treat averaged Stimuli differently
C <- C %>% filter(Emo != "avg")

## complete missing combinations
C <- as_tibble(C)
C <- C %>% complete(Subject,MType, Emo, Emo_Resp)

AVG <- as_tibble(AVG)
AVG <- AVG %>% complete(Subject, Emo_Resp)
AVG$MType <- "full"
AVG$Emo <- "avg"
AVG <- AVG[c(1,4,5,2,3,6)]

C <- rbind(C, AVG) # this is the correct number of rows -> 4576
rm(AVG)

C <- as.data.frame(C)
C$Group <- NULL
#every participant has 52 rows = 4576 in total

##fix the Group Information again
C <- merge(C, Codes)
rm(Codes)

C$N <- ifelse(is.na(C$N), 0, C$N)

#calculate how often each Emotion was presented (not equal due to error in randomization in experiment)
total <- C %>% group_by(Subject, Emo, MType) %>% summarize(total = sum(N))

C <- merge(C, total)
C$Freq <- C$N/C$total

rm(total)

# label all combinations which are correct
C$Correct <- ifelse(C$Emo == C$Emo_Resp, "Yes", "No")

C_all <- C # save unaveraged Dataset


# Average proportions across Participants
C <- C %>% group_by(MType, Emo, Emo_Resp) %>% summarise(Freq = mean(Freq), 
                                                         N = length(Emo))



############################
###  Data Visualization  ###
############################

yTitleStr = "Classification Proportion in %"
xTitleStr = "Emotion"
facetStr =  " splitted per mType"

title = paste0("Confusion matrix: Proportion of ", yTitleStr, " per ", xTitleStr, facetStr, " (N =", length(unique(D$Subject))," )")
filename = paste0("plots/amateurs_04_confusion_matrix_emotion_withavg.png")

C$MType <- recode(C$MType, full = "Full", f0 = "F0", tbr = "Timbre")
C$MType <- factor(C$MType, levels = c("Full", "F0", "Timbre"))
C$Emo <- recode(C$Emo, hap = "Hap", ple = "Ple", fea = "Fea", sad = "Sad", avg = "Avg")
C$Emo <- factor(C$Emo, levels = c("Hap", "Ple", "Fea", "Sad", "Avg"))
C$Emo_Resp<- recode(C$Emo_Resp, hap = "Hap", ple = "Ple", fea = "Fea", sad = "Sad")
C$Emo_Resp <- factor(C$Emo_Resp, levels = c("Hap", "Ple", "Fea", "Sad"))
C$Freq <- round(C$Freq, 2)

# as heatmap
p <- (ggplot(data = C, aes(x=Emo, y=Emo_Resp, fill=Freq)) + 
        geom_tile(show.legend = FALSE) +
        labs( x = xTitleStr , y = yTitleStr
        ) +  
        facet_wrap(~ MType, ncol = 3, scales = "free") +
        scale_fill_gradient(low = "white", high = "darkgrey") +
        theme(panel.background = element_rect(fill = 'white'),
              axis.text=element_text(size=16),
              axis.title=element_text(size=16), 
              axis.text.x = element_text(color = "black", size = 16), # angle = 45, hjust = 1.2, vjust =  1.2
              axis.text.y = element_text(color = "black", size = 16), 
              strip.text.x = element_text(size = 16)) +
        geom_text(aes(label=(Freq*100)), color = "black", size = 5))
ggsave(filename, width = 12, height = 4, dpi =300)

remove(yTitleStr, xTitleStr, facetStr, title, filename, p)

########################
##### Singers Only ###
########################
# Average proportions across Participants
C <- C_all %>% filter(Group == "1") %>% group_by(MType, Emo, Emo_Resp) %>% summarise(Freq = mean(Freq), 
                                                        N = length(Emo))


yTitleStr = "Classification Proportion in %"
xTitleStr = "Emotion"
facetStr =  " splitted per mType"

#title = paste0("Confusion matrix: Proportion of ", yTitleStr, " per ", xTitleStr, facetStr, " (Musicians only)")
filename = paste0("plots/amateurs_04b_singers_confusion_matrix_emotion_withavg.png")

C$MType <- recode(C$MType, full = "Full", f0 = "F0", tbr = "Timbre")
C$MType <- factor(C$MType, levels = c("Full", "F0", "Timbre"))
C$Emo <- recode(C$Emo, hap = "Hap", ple = "Ple", fea = "Fea", sad = "Sad", avg = "Avg")
C$Emo <- factor(C$Emo, levels = c("Hap", "Ple", "Fea", "Sad", "Avg"))
C$Emo_Resp<- recode(C$Emo_Resp, hap = "Hap", ple = "Ple", fea = "Fea", sad = "Sad")
C$Emo_Resp <- factor(C$Emo_Resp, levels = c("Hap", "Ple", "Fea", "Sad"))
C$Freq <- round(C$Freq, 2)

# as heatmap
p <- (ggplot(data = C, aes(x=Emo, y=Emo_Resp, fill=Freq)) + 
        geom_tile(show.legend = FALSE) +
        labs( x = xTitleStr , y = yTitleStr , #title = title
        ) +  
        facet_wrap(~ MType, ncol = 3, scales = "free") +
        scale_fill_gradient(low = "white", high = "darkgrey") +
        theme(panel.background = element_rect(fill = 'white'),
              axis.text=element_text(size=16),
              axis.title=element_text(size=16), 
              axis.text.x = element_text(color = "black", size = 16), # angle = 45, hjust = 1.2, vjust =  1.2
              axis.text.y = element_text(color = "black", size = 16), 
              strip.text.x = element_text(size = 16)) +
        geom_text(aes(label=(Freq*100)), color = "black", size = 5))
ggsave(filename, width = 12, height = 4, dpi =300)

###############################
##### Instrumentalists Only ###
###############################
# Average proportions across Participants
C <- C_all %>% filter(Group == "2") %>% group_by(MType, Emo, Emo_Resp) %>% summarise(Freq = mean(Freq), 
                                                                                     N = length(Emo))


yTitleStr = "Classification Proportion in %"
xTitleStr = "Emotion"
facetStr =  " splitted per mType"

#title = paste0("Confusion matrix: Proportion of ", yTitleStr, " per ", xTitleStr, facetStr, " (Nonmusicians only)")
filename = paste0("plots/amateurs_04c_instrumentalists_confusion_matrix_emotion_withavg.png")

C$MType <- recode(C$MType, full = "Full", f0 = "F0", tbr = "Timbre")
C$MType <- factor(C$MType, levels = c("Full", "F0", "Timbre"))
C$Emo <- recode(C$Emo, hap = "Hap", ple = "Ple", fea = "Fea", sad = "Sad", avg = "Avg")
C$Emo <- factor(C$Emo, levels = c("Hap", "Ple", "Fea", "Sad", "Avg"))
C$Emo_Resp<- recode(C$Emo_Resp, hap = "Hap", ple = "Ple", fea = "Fea", sad = "Sad")
C$Emo_Resp <- factor(C$Emo_Resp, levels = c("Hap", "Ple", "Fea", "Sad"))
C$Freq <- round(C$Freq, 2)

# as heatmap
p <- (ggplot(data = C, aes(x=Emo, y=Emo_Resp, fill=Freq)) + 
        geom_tile(show.legend = FALSE) +
        labs( x = xTitleStr , y = yTitleStr , #title = title
        ) + 
        facet_wrap(~ MType, ncol = 3, scales = "free") +
        scale_fill_gradient(low = "white", high = "darkgrey") +
        theme(panel.background = element_rect(fill = 'white'),
              axis.text=element_text(size=16),
              axis.title=element_text(size=16), 
              axis.text.x = element_text(color = "black", size = 16), # angle = 45, hjust = 1.2, vjust =  1.2
              axis.text.y = element_text(color = "black", size = 16), 
              strip.text.x = element_text(size = 16)) +
        geom_text(aes(label=(Freq*100)), color = "black", size = 5))
ggsave(filename, width = 12, height = 4, dpi =300)



### End of Script