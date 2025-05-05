##########################################################################
## File: 00a_data_preparation_exp_proms.R.R
## Data Preparation for the Online Experiment Data and Proms Data
# authors: Christine Nussbaum, Jessica Senftleben
# date 05/2025

#NOT PUBLISHED

# clear directory
rm(list=ls())

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# load required packages
library("tidyverse")

# load relevant functions
source("functions/loadPTKExperimentData.R")
source("functions/mySummary.R") 

#---------------------------------------------------------------------------------
#                             emotion classification task
#---------------------------------------------------------------------------------

#get the raw data:
D <- loadPTKExperimentData(relDirPath = "input/emo_exp/")


names(D) <- c("SpId", "Emo","Word",  "MType", "SpSex", "filename", "CB", "ACC", "Resp", "RT", "Block", "TrialNo", "Experiment", "Date", "Subject")

# practise-Trials raus

D <- D %>% filter(Block != "practise")


# check if all stimuli are here
length(unique(D$filename))

table(D$Subject)


D$ACC <- ifelse(D$ACC == 1, 1,0)

# save prepared dataset: 
save(D, file="input/Exp_raw.RData")

#--------------------------------------------------------------------------------


### Processing for further analyses

## Analysis of missings 
missings <- D %>% filter(RT >= 5000) # omission, RT-cutoff was 5000ms

missings_n <- missings %>% group_by(Subject) %>% summarise(n = length(Experiment))

#-> Missing criterion is 5% (= 15.6 trials of 312), so two participants have to be removed (c7e7e936 and ecd4a2cb)
to_be_removed <- missings_n$Subject[missings_n$n > 15]

#remove these participants from the dataset: 
D <- D %>% filter(!(D$Subject  %in% to_be_removed))

missings_n <- missings_n %>% filter(!(Subject  %in% to_be_removed))

paste("There are currently", length(unique(D$Subject)), "datasets loaded.")


#save summary information about trials of omission: 
missings_n$percent <- missings_n$n /312
missings_info <- mySummary(missings_n, percent)
capture.output(missings_info, file="output/amateurs_omissions_summary.txt")
rm(missings_info, missings_n, missings)

#save information about the removed participants: 
save(to_be_removed, file ="input/amateurs_to_be_removed.RData")


## Removing data with faulty audio 
# remove participants with faulty audio-files
D <- subset(D, Subject!="08eba336") # instrumentalist AIG27E
D <- subset(D, Subject!="27e3b37a") # singer IHP42U
#ecd4a2cb (Subject SCS58E) also reported faulty audio-files, but is already removed because of omitting >5% of trials

# remove participant who is both in a choir and a band
D <- subset(D, Subject!="a67e3eb6") #NIR46E

paste("There are currently", length(unique(D$Subject)), "datasets loaded.")


## merge with relevant data from survey (Group, Code)
# set path to survey data
relPathDirFile = "input/survey/data.csv"
Raw <- read.csv(relPathDirFile, header= TRUE, sep = ",", row.names = NULL)

#fix  colnames
cols <- names(Raw)
names(Raw) <-cols[1:189]
Raw <- Raw[,1:189]

#fix single participant who closed the window too fast and has no time_end
Raw$TIME_end[Raw$TIME_start == "2024-01-24-18-23"] <- "2024-01-24-19-38"
Raw$TIME_total[Raw$TIME_start == "2024-01-24-18-23"] <- 75

#remove all incompleted datasets (they dont have time_end)
Raw <- Raw %>% filter(!is.na(Raw$TIME_total))

# remove test dataset (ENH59A)
Raw <- subset(Raw, intro_question1_1!="1062023")

N <- length(Raw$participant)
Raw$VPN <- paste0("VPN_",c(1:N))

#extract participant and Code
Raw$participant <- str_sub(Raw$participant , 31, 38)
Raw$Partcode <- paste0(Raw$LPartCode_1, Raw$LPartCode_2, Raw$LPartCode_3, Raw$LPartCode_4, Raw$LPartCode_5, Raw$LPartCode_6)

#remove irrelevant variables
Raw <- Raw[ , -c(180:185)]

nSubs <- length(unique(Raw$participant))
print(paste("loaded data from", nSubs, "participants", sep=" "))

#isolate relevant variables (Subject, Group, Code)
Sample <- Raw[, c(1,185,7)]      
names(Sample) <- c("Subject", "Code", "Group")              
Sample$Code <-toupper(Sample$Code)  # make code uppercase

## merge exp data with relevant survey data
D2 <- merge(D, Sample)

# correct Group assignment for one participant (AEM51L)
D2$Group[D2$Code == "AEM51L"] <- 1

#tidy up so it matches data set from professionals/nonmusicians
D <- D2[,c(1,16,17,2:11)]

save(D, file="input/amateurs_Exp_processed.RData")

rm(D2,Raw, N, cols, nSubs, relPathDirFile)

#---------------------------------------------------------------------------------
#                                   Modular PROMS Raw Data
#---------------------------------------------------------------------------------

source("functions/loadPTKPromsData.R")

P <- loadPTKPromsData(relDirPath = "input/proms/")


P <- P[, -c(1:8)]

names(P) <- c("Same", "Stimulus", "Response", "RT", "Test", "Trialno", "Experiment", "Date", "Subject")


#--------------------------------------------------------------------------------
  

# remove TrialNo1, because this is the Test-Trial
P <- P %>% filter(Trialno != 1)

P$Trialno <- P$Trialno -1

P$Response<- ifelse(P$Response > 5, P$Response -5, P$Response)


#code rough accuracy
P$ACC <- ifelse(P$Same == "DS" & P$Response < 3, 1, 0)
P$ACC <- ifelse(P$Same == "DD", 999, P$ACC)
P$ACC2 <- ifelse(P$Same == "DD" & P$Response > 3, 1, 0)
P$ACC2 <- ifelse(P$Same == "DS", 999, P$ACC2)

P$Resp <- ifelse(P$Same == "DS", P$ACC, P$ACC2)

save(P, file="input/PROMS_raw.RData")


#table(P$Test, P$Resp)


#------------------------------------------------------------------------------------------------------------------#

## Processing for further analyses

# remove participants who omitted >5% of trials in emotion class. exp
P <- subset(P, Subject!="c7e7e936") # singer EHM92A
P <- subset(P, Subject!="ecd4a2cb") # singer SCS58E

# remove participants with faulty audio-files
P <- subset(P, Subject!="08eba336") # instrumentalist AIG27E
P <- subset(P, Subject!="27e3b37a") # singer IHP42U
#ecd4a2cb (Subject SCS58E) also reported faulty audio-files, but is already removed because of omitting >5% of trials

# remove participant who is in both groups (NIR46E)
P <- subset(P, Subject!="a67e3eb6") #NIR46E

paste("There are currently", length(unique(P$Subject)), "datasets loaded.")

## merge PROMS with relevant data from survey (Group, Code)
P2 <- merge(P, Sample)

# correct Group assignment for one participant (AEM51L)
P2$Group[P2$Code == "AEM51L"] <- 1

#tidy up so it matches data set from musicians/nonmusicians
P <- P2[,c(1,13,14,2:7,12)]

save(P, file="input/amateurs_PROMS_preprocessed.RData")

rm(P,Sample)


### End of Script