##########################################################################
## File: 01c_PROMS_data_analysis_amateurs_partI.R
## This script analysis for the PROMS Data for singers vs. instrumentalists
# authors: Christine Nussbaum (christine.nussbaum@uni-jena.de), Jessica Senftleben
# date 10/2022, 02/2024

# clear directory
rm(list=ls())

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# load required packages
library(tidyverse) # version 1.3.1
library(ez) # version 4.4-0
library(effectsize) # 0.4.5

# load relevant functions
source("functions/mySummary.R") 

# set some relevant setting 
options(scipen = 999)


#-------------------------------------------------#
#         Data preparation of PROMS Data          #
#-------------------------------------------------#

load(file="input/amateurs_PROMS_preprocessed.RData")

# remove participant with degree in music science
P <- subset(P, Code!="AEB56L") # instrumentalist

## Meaning of Variables

# Subject                       individual participant code assigned by PsyToolkit
# Code                          self-generated Code by the participant
# Group                         group - 1 = singers; 2 = instrumentalists 
# Same                          is it a same or a different trial (DS = Same, DD = different)
# Stimulus                      name of the stimulus
# Response                      keypress of the participant (1 = definitely same, 2 = maybe same, 3 = dont know, 4 = maybe different, 5 = definitely different)
# RT                            reaction time
# test                          subtest (melody, pitch, timbre, rhythm)
# TrialNo                       Trial number (1 to 18 for each subtest)
# Resp                          coded accuracy

###--------------------------------------------------------------------------------------------------------------###
###                         Calculating Signal-Detection Parameters  and Confidence Measure                      ###
###--------------------------------------------------------------------------------------------------------------###


# Calculating Parameters of Signal Detection Theory

P$Hits <- ifelse(P$Same == "DS" & P$Resp== 1, 1, 0)
P$Hits <- ifelse(P$Same == "DD", NA, P$Hits)

P$CR <- ifelse(P$Same == "DD" & P$Resp == 1, 1, 0)
P$CR <- ifelse(P$Same == "DS", NA, P$CR)

P$Miss <- ifelse(P$Same == "DS" & P$Resp== 0, 1, 0)
P$Miss <- ifelse(P$Same == "DD", NA, P$Miss)

P$FA <- ifelse(P$Same == "DD" & P$Resp== 0, 1, 0)
P$FA <- ifelse(P$Same == "DS", NA, P$FA)

P$Sure <- ifelse(P$Response == 1 | P$Response == 5, 1,0)  # all responses where participants were "sure"
P$Unsure <- ifelse(P$Response == 2 | P$Response == 4, 1,0) # all responses where participants were "unsure"
P$DontKnow <- ifelse(P$Response == 3, 1,0) # all responses where participants responded with "don't know"


##incorporate the "unsure"-option into Hits and False Alarms - in correspondence with Dr. Hannah Strau? from Innsbruck (developers of the PROMS)

P$Hits_c <- ifelse(P$Hits == 1 & P$Unsure == 1, 0.5, P$Hits)

table(P$Hits_c, P$Hits)

P$FA_c <- ifelse(P$FA == 1 & P$Unsure == 1, 0.5, P$FA)

table(P$FA_c, P$FA)

### Calculate the confidence measure: 

# for same Trials: 1 = 1, 2 = 0.75, 3 = 0.5, 4 = 0.25, 5 = 0
# for different Trials: 1 = 0, 2 = 0.25, 3 = 0.5, 4 = 0.75, 5 = 1

#extract SameTrials and recode them
DS <- P %>% filter(Same == "DS")
DS$Confidence <-  recode(DS$Response, `1` = 1, `2` = 0.75, `3` = 0.5,  `4` = 0.25, `5` = 0)


#extract Different Trials and recode them
DD <- P %>% filter(Same == "DD")
DD$Confidence <-  recode(DD$Response, `1` = 0, `2` = 0.25, `3` = 0.5,  `4` = 0.75, `5` = 1)

P2 <- rbind(DD, DS)

#subtract 0.5 from it, to center it round 0

P2$Confidence <- P2$Confidence - 0.5

# merge them back to the main data frame:

P <- merge(P, P2)
rm(P2, DS, DD)


#### Aggregate the data

P_agg <- P %>% group_by(Code, Group, Test) %>% summarise(Resp = sum(Resp),
                                                         Hits = mean(Hits_c, na.rm= TRUE),
                                                         CR = mean(CR, na.rm=TRUE), 
                                                         Miss = mean(Miss, na.rm= TRUE), 
                                                         FA = mean(FA_c, na.rm=TRUE),
                                                         Sure = sum(Sure), 
                                                         Unsure = sum(Unsure),
                                                         DontKnow = sum(DontKnow),
                                                         Confidence = mean(Confidence), 
                                                         N = length(Trialno)) # should always be 18

####  calculate the z parameters

# correction for values of 0 and 1

P_agg$Hits <- ifelse(P_agg$Hits == 1, (90-0.5)/90, P_agg$Hits)
P_agg$FA <- ifelse(P_agg$FA == 0, 0.5/90, P_agg$FA)
P_agg$FA <- ifelse(P_agg$FA == 1, (90-0.5)/90, P_agg$FA)

# calculation of z-values
# help: dnorm, qnorm, pnorm, rnorm

P_agg$Hz <- qnorm(P_agg$Hits)
P_agg$FAz <- qnorm(P_agg$FA)


#calculate d-prime and criterion
#d-prime
P_agg$dprime <- P_agg$Hz - P_agg$FAz

#criterion
P_agg$crit <- (-0.5*(P_agg$Hz + P_agg$FAz))

B <- as.data.frame(P_agg)
rm(P_agg)
B$Group <- as.factor(B$Group)


###--------------------------------------------------------------------------------------------------------------###
###                                            Statistical Analysis                                              ###
###--------------------------------------------------------------------------------------------------------------###

# extract descriptive data
DPrime_summary <- mySummary(B, dprime, Group, Test)
Confidence_summary <- mySummary(B, Confidence, Group, Test)

capture.output(as.matrix(DPrime_summary), as.matrix(Confidence_summary), file="output/musicality/amateurs_PROMS_summary_dprime_and_confidence.txt")

rm(DPrime_summary, Confidence_summary )


# run a simple t-test on d-prime
melody <- t.test(data = B[B$Test == "melody",], dprime ~ Group)
d_melody <- t_to_d(melody$statistic, melody$parameter, paired = FALSE)
pitch <- t.test(data = B[B$Test == "pitch",], dprime ~ Group)
d_pitch <- t_to_d(pitch$statistic, pitch$parameter, paired = FALSE)
timbre <- t.test(data = B[B$Test == "timbre",], dprime ~ Group)
d_timbre <- t_to_d(timbre$statistic, timbre$parameter, paired = FALSE)
rhythm <- t.test(data = B[B$Test == "rhythm",], dprime ~ Group)
d_rhythm <- t_to_d(rhythm$statistic, rhythm$parameter, paired = FALSE)



# run a simple t-test on mean Resp Accuracy
melodyII <- t.test(data = B[B$Test == "melody",], Resp ~ Group)
pitchII <- t.test(data = B[B$Test == "pitch",], Resp ~ Group)
timbreII <- t.test(data = B[B$Test == "timbre",], Resp ~ Group)
rhythmII <- t.test(data = B[B$Test == "rhythm",], Resp ~ Group)


# run a simple t-test on the criterion
melodyIII <- t.test(data = B[B$Test == "melody",], crit ~ Group)
pitchIII <- t.test(data = B[B$Test == "pitch",], crit ~ Group)
timbreIII <- t.test(data = B[B$Test == "timbre",], crit ~ Group)
rhythmIII <- t.test(data = B[B$Test == "rhythm",], crit ~ Group)


# run a simple t-test on the number of "unsure"-responses
melodyIV <- t.test(data = B[B$Test == "melody",], Unsure ~ Group)
pitchIV <- t.test(data = B[B$Test == "pitch",], Unsure ~ Group)
timbreIV <- t.test(data = B[B$Test == "timbre",], Unsure ~ Group)
rhythmIV <- t.test(data = B[B$Test == "rhythm",], Unsure ~ Group)


# run a simple t-test on the number of "DontKnow"-responses
melodyV <- t.test(data = B[B$Test == "melody",], DontKnow ~ Group)
pitchV <- t.test(data = B[B$Test == "pitch",], DontKnow ~ Group)
timbreV <- t.test(data = B[B$Test == "timbre",], DontKnow ~ Group)
rhythmV <- t.test(data = B[B$Test == "rhythm",], DontKnow ~ Group)


# run a simple t-tests on confidence (this is the measure used for the main manuscript)
melodyVI <- t.test(data = B[B$Test == "melody",], Confidence ~ Group)
d_melodyVI <- t_to_d(melodyVI$statistic, melodyVI$parameter, paired = FALSE)
pitchVI <- t.test(data = B[B$Test == "pitch",], Confidence ~ Group)
d_pitchVI <- t_to_d(pitchVI$statistic, pitchVI$parameter,  paired = FALSE)
timbreVI <- t.test(data = B[B$Test == "timbre",], Confidence ~ Group)
d_timbreVI <- t_to_d(timbreVI$statistic, timbreVI$parameter, paired = FALSE)
rhythmVI <- t.test(data = B[B$Test == "rhythm",], Confidence ~ Group)
d_rhythmVI <- t_to_d(rhythmVI$statistic, rhythmVI$parameter, paired = FALSE)



# save all the t.test

capture.output(melody, d_melody, melodyII, melodyIII, melodyIV, melodyV, melodyVI, d_melodyVI, file="output/musicality/amateurs_PROMS_melody.txt")
capture.output(pitch, d_pitch, pitchII, pitchIII, pitchIV, pitchV, pitchVI, d_pitchVI, file="output/musicality/amateurs_PROMS_pitch.txt")
capture.output(timbre, d_timbre, timbreII, timbreIII, timbreIV, timbreV, timbreVI, d_timbreVI, file="output/musicality/amateurs_PROMS_timbre.txt")
capture.output(rhythm, d_rhythm, rhythmII, rhythmIII, rhythmIV, rhythmV, rhythmVI, d_rhythmVI, file="output/musicality/amateurs_PROMS_rhythm.txt")

rm(melody, melodyII, melodyIII, melodyIV, melodyV, melodyVI, d_melodyVI,
   pitch, pitchII, pitchIII, pitchIV, pitchV, pitchVI, d_pitchVI,
   timbre, timbreII, timbreIII, timbreIV, timbreV, timbreVI, d_timbreVI,
   rhythm, rhythmII, rhythmIII, rhythmIV, rhythmV, rhythmVI, d_rhythmVI,
   d_melody, d_pitch, d_timbre, d_rhythm)


# save the prepared dataset for further analyses
PROMS2 <- B
save(PROMS2, file="input/amateurs_PROMS_prepared.RData")

# End of Script