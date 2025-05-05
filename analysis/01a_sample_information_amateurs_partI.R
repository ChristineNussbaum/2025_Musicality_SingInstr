##########################################################################
## File: 01a_sample_information_amateurs_partI.R
## This script summarises the characteristics of our sample (singers vs.instrumentalists)
# authors: Christine Nussbaum (christine.nussbaum@uni-jena.de), Jessica Senftleben
# date 05/2025

# clear directory
rm(list=ls())

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# load required packages
library(tidyverse)  # version 1.3.1
library(effectsize) # version 0.4.5

# load helpful functions
source("functions/mySummary.R") 

# load the data of the survey and questionnaires that were collected before and after the classification experiment
load(file="input/amateurs_survey_processed.RData")

# remove participant with a Degree in Music Science
survey <- subset (survey, Code!="AEB56L") # instrumentalist

#-> this dataset entails the information of 45 singers and 43 instrumentalists

## Meaning of Variables

# participant                       individual participant code assigned by PsyToolkit
# Code                              self-generated Code by the participant
# Age                               participants' age
# LSex                              participants' sex, 1 = female, 2 = male, 3 = diverse, 4 = no information
# LMotherLanguage                   participants' mother language, 1 = German, 2 = English, 3 = other
# LProfession                       participants' profession (answers in German)
# Group                             group - 1 = singers; 2 = instrumentalists 
# Instrument                        participants' instrument (in German)
# AgeTraining                       age at instrument/singing training onset 
# YearsTraining                     years of formal music training (1 = 0, 2 = 1, 3 = 2, 4 = 3, 5 = 4-5, 6 = 6-9, 7 = 10 or more)
# LHearingImp                       indication of any known hearing impairments, 1 = No, 2 = yes, 3 = I don't know
# LHearingImp2                      if yes, which kind of impairment (free text response in German)
# LHearingImp3                      if yes, how much does it constrain you in daily life (1 = not at all, 2 = a little bit, 3 = to some degree, 4 = substantially)
# TimeTotal                         total time to complete the survey + experiment (minutes)
# AQ_SocialSkills : AQ_Total        different subscores of the AQ-Questionnaire - refer to Hoekstra et al. (2008) and Baron-Cohen et al. (2001)
# O : N (OCEAN)                     scores of the Big-Five questionnaire - refer to Rammstedt et al. (2018)
# Mean_active : ME_mean             mean scores of the Gold-MSI - refer to M?llensiefen et al. (2014)
# LMusicQualification_1             indication of non-academic music qualification (1 = yes, 2 = no)
# LMusicDegree_1                    indication of a academic music qualification (1 = yes, 2 = no)
# LMusicDegree2_1                   if yes, which one (1 = Bachelor, 2 = Master, 3 = Diploma, 4 = PhD, 5 = other)
# LPerfectPitch_1                   indication of a perfect pitch (absolute hearing ability)
# pos_Aff : neg_Aff                 scores of the PANAS - refer to Breyer & Bluemke (2016)
# Schulabschluss                    highest school degree (according to German system - refer to supplemental tables)
# Ausbildung                        highest level of education (according to German system - refer to supplemental tables)
# Einkommen                         monthly household income (1 = less than 1750???, 2 = 1750-2500???, 3 = 2500-3500???, 4 = 3500-5000???, 5 = more than 5000???)
# Instrument2                       recode the instrument column to account for any spelling mistakes or different expressions for the same instrument



#------------------------------------------------------------------------------------------------------------------#
#                                           Sample Demographics                                                    #
#------------------------------------------------------------------------------------------------------------------#


# Age
Age <- mySummary(survey, Age, Group)

range <- survey %>% group_by(Group) %>% summarise(range(Age))

#Age
age_diff <- t.test(data = survey, Age ~ Group) 
d_age_diff <- t_to_d(age_diff$statistic, age_diff$parameter, paired = FALSE)


# capture output:
capture.output(as.matrix(Age), as.matrix(range), age_diff, d_age_diff, 
               file="output/sample_description/age.txt")
rm(Age, range, age_diff, d_age_diff)


## other demographics
sex <- table(survey$Group, survey$LSex)
language <- table(survey$Group, survey$LMotherLanguage)
profession <- table(survey$LProfession)
hearingImp <- table(survey$Group, survey$LHearingImp)
hearingImp2 <- table(survey$Group, survey$LHearingImp2)
hearingImp3 <- table(survey$Group, survey$LHearingImp3)
duration <- survey %>% filter(TimeTotal < 999) %>%  group_by(Group) %>% summarise(Dur = mean(TimeTotal))
info <- c("Sex", "Language", "Profession", "HearingImpairment", "HearinImpairment2", "HearingImpairment3", "Duration")

capture.output(info[1], sex, info[2], language, info[3], profession, 
               info[4], hearingImp, info[5], hearingImp2, info[6], 
               hearingImp3, info[7], as.matrix(duration), 
               file="output/sample_description/demographics.txt")
rm(sex, language, profession, hearingImp, hearingImp2, hearingImp3, duration)


#------------------------------------------------------------------------------------------------------------------#
#                                           AQ Data                                                                #
#------------------------------------------------------------------------------------------------------------------#

# descriptive_data
AQ_summary <- survey %>% group_by(Group) %>% summarise(AQ_Tot = mean(AQ_Total),   # AQ Total
                                                       AQ_TotSD = sd(AQ_Total), 
                                                       AQ_Soc = mean(AQ_Social),  # AQ Social
                                                       AQ_SocSD = sd(AQ_Social),
                                                       AQ_Att = mean(AQ_AttentionToDetails), # AQ Attention to Detail
                                                       AQ_AttSD = sd(AQ_AttentionToDetails), 
                                                       AQ_SocSkills = mean(AQ_SocialSkills), # AQ Social Skills
                                                       AQ_SocSkillsSD = sd(AQ_SocialSkills),
                                                       AQ_Com = mean(AQ_Communication),    # AQ Communication
                                                       AQ_ComSD = sd(AQ_Communication), 
                                                       AQ_Imag = mean(AQ_Imagination),     # AQ Imagination
                                                       AQ_ImagSD = sd(AQ_Imagination),
                                                       AQ_AttSwitch = mean(AQ_AttentionSwitching), #AQ Attention Switching
                                                       AQ_AttSwitchSD = sd(AQ_AttentionSwitching))


#AQ TTest
AQ_total <- t.test(data = survey, AQ_Total ~ Group) 
d_AQ_total <- t_to_d(AQ_total$statistic, AQ_total$parameter, paired = FALSE)
AQ_ATT <- t.test(data = survey, AQ_AttentionToDetails ~ Group) 
d_AQ_ATT <- t_to_d(AQ_ATT$statistic, AQ_ATT$parameter, paired = FALSE)
AQ_Social <- t.test(data = survey, AQ_Social ~ Group) 
d_AQ_Social <- t_to_d(AQ_Social$statistic, AQ_Social$parameter, paired = FALSE)

AQ_SocSkills <- t.test(data = survey, AQ_SocialSkills ~ Group) 
d_AQ_SocSkills <- t_to_d(AQ_SocSkills$statistic, AQ_SocSkills$parameter, paired = FALSE)
AQ_Com <- t.test(data = survey, AQ_Communication ~ Group) 
d_AQ_Com <- t_to_d(AQ_Com$statistic, AQ_Com$parameter, paired = FALSE)
AQ_Imag <- t.test(data = survey, AQ_Imagination ~ Group) 
d_AQ_Imag <- t_to_d(AQ_Imag$statistic, AQ_Imag$parameter, paired = FALSE)
AQ_AttSwitch <- t.test(data = survey, AQ_AttentionSwitching ~ Group) 
d_AQ_AttSwitch <- t_to_d(AQ_AttSwitch$statistic, AQ_AttSwitch$parameter, paired = FALSE)

capture.output(as.matrix(AQ_summary), AQ_total, d_AQ_total, AQ_ATT, d_AQ_ATT,  AQ_Social, d_AQ_Social,
               AQ_SocSkills, d_AQ_SocSkills, AQ_Com, d_AQ_Com, AQ_Imag, d_AQ_Imag, AQ_AttSwitch, d_AQ_AttSwitch, file="output/sample_description/AQ.txt")
rm(AQ_summary, AQ_total, AQ_ATT, AQ_Social,  d_AQ_total,  d_AQ_ATT, d_AQ_Social, AQ_SocSkills, d_AQ_SocSkills, AQ_Com, d_AQ_Com, AQ_Imag, d_AQ_Imag, AQ_AttSwitch, d_AQ_AttSwitch)


#------------------------------------------------------------------------------------------------------------------#
#                                           PANAS                                                                #
#------------------------------------------------------------------------------------------------------------------#

# descriptive_data
PANAS <- survey %>% group_by(Group) %>% summarise(Neg = mean(neg_Aff),
                                                  NegSD = sd(neg_Aff),
                                                  Pos = mean(pos_Aff),
                                                  PosSD = sd(pos_Aff))

#PANAS
Neg <- t.test(data = survey, neg_Aff ~ Group) 
d_Neg <- t_to_d(Neg$statistic, Neg$parameter, paired = FALSE)
Pos <- t.test(data = survey, pos_Aff ~ Group) 
d_Pos <- t_to_d(Pos$statistic, Pos$parameter, paired = FALSE)

capture.output(as.matrix(PANAS), Neg, d_Neg, Pos, d_Pos, file="output/sample_description/PANAS.txt")
rm(PANAS, Neg, Pos, d_Neg, d_Pos)


#------------------------------------------------------------------------------------------------------------------#
#                                           SES                                                               #
#------------------------------------------------------------------------------------------------------------------#

#Einkommen (Income)
Einkommen <- chisq.test(survey$Group, survey$Einkommen)

#Schulabschluss (Education)
Schulabschluss <- chisq.test(survey$Group, survey$Schulabschluss)

#Ausbildung (Degree)
Ausbildung <- chisq.test(survey$Group, survey$Ausbildung)

capture.output(Einkommen, table(survey$Einkommen, survey$Group),  Schulabschluss, table(survey$Schulabschluss,survey$Group), Ausbildung, table(survey$Ausbildung, survey$Group), file="output/sample_description/SES.txt")
rm(Schulabschluss, Einkommen, Ausbildung)


#------------------------------------------------------------------------------------------------------------------#
#                                           GOLD MSI                                                               #
#------------------------------------------------------------------------------------------------------------------#

# descriptive_data
MSI_summary <- survey %>% group_by(Group) %>% summarise(ME = mean(ME_mean), 
                                                       ME_SD = sd(ME_mean), 
                                                       Active = mean(Mean_active),
                                                       Active_SD = sd(Mean_active),
                                                       Education = mean(Mean_education),
                                                       Education_SD = sd(Mean_education),
                                                       Emotion = mean(Mean_emotion),
                                                       Emotion_SD = sd(Mean_emotion),
                                                       Singing = mean(Mean_singing),
                                                       Singing_SD = sd(Mean_singing),
                                                       Perception = mean(Mean_perception), 
                                                       Perception_SD = sd(Mean_perception))


#GOLD-MSI
ME <- t.test(data = survey, ME_mean ~ Group) 
d_ME <- t_to_d(ME$statistic, ME$parameter, paired = FALSE)
Active <- t.test(data = survey, Mean_active ~ Group) 
d_Active <- t_to_d(Active$statistic, Active$parameter, paired = FALSE)
Education <- t.test(data = survey, Mean_education ~ Group) 
d_Education <- t_to_d(Education$statistic, Education$parameter, paired = FALSE)
Emotion <- t.test(data = survey, Mean_emotion ~ Group) 
d_Emotion <- t_to_d(Emotion$statistic, Emotion$parameter, paired = FALSE)
Singing <- t.test(data = survey, Mean_singing ~ Group) 
d_Singing <- t_to_d(Singing$statistic, Singing$parameter, paired = FALSE)
Perception <- t.test(data = survey, Mean_perception ~ Group) 
d_Perception <- t_to_d(Perception$statistic, Perception$parameter, paired = FALSE)

capture.output(as.matrix(MSI_summary), ME, d_ME, Active, d_Active, Education, d_Education, Emotion, d_Emotion, Singing, d_Singing, Perception, d_Perception, file="output/musicality/Gold-MSI.txt")
rm(MSI_summary, ME, Active, Education, Emotion, Singing, Perception, d_ME, d_Active, d_Education, d_Emotion, d_Singing, d_Perception)

#------------------------------------------------------------------------------------------------------------------#
#                                           OCEAN                                                               #
#------------------------------------------------------------------------------------------------------------------#

# descriptive_data
OCEAN <- survey %>% group_by(Group) %>% summarise(o = round(mean(O), 2),
                                                  O_SD = round(sd(O), 2),
                                                  c = round(mean(C), 2),
                                                  C_SD = round(sd(C), 2),
                                                  e = round(mean(E), 2),
                                                  E_SD = round(sd(E), 2),
                                                  a = round(mean(A), 2),
                                                  A_SD = round(sd(A), 2),
                                                  n = round(mean(N), 2),
                                                  N_SD = round(sd(N), 2))



#OCEAN
O = t.test(data = survey, O ~ Group) 
d_O <- t_to_d(O$statistic, O$parameter, paired = FALSE)
C = t.test(data = survey, C ~ Group) 
d_C <- t_to_d(C$statistic, C$parameter, paired = FALSE)
E = t.test(data = survey, E ~ Group)
d_E <- t_to_d(E$statistic, E$parameter, paired = FALSE)
A = t.test(data = survey, A ~ Group) 
d_A <- t_to_d(A$statistic, A$parameter, paired = FALSE)
N = t.test(data = survey, N ~ Group) 
d_N <- t_to_d(N$statistic, N$parameter, paired = FALSE)

capture.output(as.matrix(OCEAN), O,d_O, C, d_C, E, d_E, A, d_A, N, d_N, file="output/sample_description/OCEAN.txt")
rm(OCEAN, O,C,E,A,N, d_O, d_C, d_E, d_A, d_N)



#------------------------------------------------------------------------------------------------------------------#
#                                   Musical Training, singers only                                           #
#------------------------------------------------------------------------------------------------------------------#

## singers only
singers <- survey %>% filter(Group =="1")

#fix age column
singers$AgeTraining <- ifelse(is.na(singers$AgeTraining), 0, singers$AgeTraining)

AgeTrainingAll<- table(singers$AgeTraining)
TrainingSing <- singers %>% filter(AgeTraining > 0)

#Age at onset of training
AgeTrainingSubSet<- mySummary(TrainingSing, AgeTraining)
Range <- range(TrainingSing$AgeTraining)
capture.output(as.matrix(AgeTrainingAll), as.matrix(AgeTrainingSubSet), Range, file= "output/sample_description/Singers_onsetAge.txt")

# years of musical training
yearsTraining<- table(singers$YearsTraining)
yearsTraining_summary<- mySummary(TrainingSing, YearsTraining)
Range <- range(singers$YearsTraining)
capture.output(as.matrix(yearsTraining),yearsTraining_summary, Range, file= "output/sample_description/Singers_yearsTraining.txt")
rm(AgeTrainingAll, AgeTrainingSubSet,yearsTraining_summary, Range, yearsTraining)

#which Instruments: 
Instruments <- as.data.frame(table(singers$Instrument2))
capture.output(as.matrix(Instruments), file = "output/sample_description/Singers_Instruments.txt")

rm(Instruments, TrainingSing)
rm(singers)


#------------------------------------------------------------------------------------------------------------------#
#                                   Musical Training, instrumentalists only                                           #
#------------------------------------------------------------------------------------------------------------------#

## instrumentalists only
instrumentalists <- survey %>% filter(Group =="2")

#fix the AgeTraining column
instrumentalists$AgeTraining <- ifelse(is.na(instrumentalists$AgeTraining), 0, instrumentalists$AgeTraining)

AgeTrainingAll<- table(instrumentalists$AgeTraining)
TrainingInstr <- instrumentalists %>% filter(AgeTraining > 0)

#Age at onset of training
AgeTrainingSubSet<- mySummary(TrainingInstr, AgeTraining)
Range <- range(TrainingInstr$AgeTraining)
capture.output(as.matrix(AgeTrainingAll), as.matrix(AgeTrainingSubSet), Range, file= "output/sample_description/Instrumentalists_onsetAge.txt")

# years of musical training
yearsTraining<- table(instrumentalists$YearsTraining)
yearsTraining_summary<- mySummary(instrumentalists, YearsTraining)
Range <- range(instrumentalists$YearsTraining)
capture.output(as.matrix(yearsTraining),yearsTraining_summary, Range,  file= "output/sample_description/Instrumentalists_yearsTraining.txt")
rm(AgeTrainingAll, AgeTrainingSubSet, Range, yearsTraining, yearsTraining_summary)

#which Instruments: 
Instruments <- as.data.frame(table(instrumentalists$Instrument2))
capture.output(as.matrix(Instruments), file = "output/sample_description/Instrumentalists_Instruments.txt")

rm(Instruments, TrainingInstr)
rm(instrumentalists)

# End of Script