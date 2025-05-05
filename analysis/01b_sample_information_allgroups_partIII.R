##########################################################################
## File: 01b_sample_information_allgroups_partIII.R
## This script summarises the characteristics of our sample (all groups)
# authors: Christine Nussbaum, Jessica Senftleben
# date 01/2024, refined in 3/2025

#NOT PUBLISHED

# clear directory
rm(list=ls())

#set working directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# load required packages
library(tidyverse)  # version 1.3.1
library(effectsize) # version 0.4.5
library(ez)
library(schoRsch)

# load helpful functions
source("functions/mySummary.R") 
source("functions/tracedEZout.R")


#------------------------------------------------------------------------------------------------------------------#
#                                   Combine data sets (amateurs + professional musicians/non-musicians)            #
#------------------------------------------------------------------------------------------------------------------#

# load the respective data of the survey and questionnaires that were collected before and after the classification experiment
load(file="input/amateurs_survey_processed.RData") #amateurs
survey2 <- survey #rename the file
load(file="input/musicians vs nonmusicians/survey_processed.RData") #musicians & non-musicians

# make amateurs into one group ("A")
survey2$Group[survey2$Group=="2"] <-"A"
survey2$Group[survey2$Group=="1"] <-"A"
survey2 <- survey2 %>% mutate(Group = as.factor(Group))

# fix some variable names to match amateurs/survey2 to prepare merging
survey$AgeTraining <- survey$AgeInstrument
survey$LMusicQualification_1 <- survey$LMusicQualification.1
survey$LMusicDegree_1 <- survey$LMusicDegree.1 
survey$LMusicDegree2_1 <- survey$LMusicDegree2.1
survey$LPerfectPitch_1 <- survey$LPerfectPitch.1
musicians <- survey[,c(1:12,43,14:31,44:47,36:42)]

# merge data frames
survey <- rbind(musicians, survey2)

# change group of participant with degree in music science to musician
survey$Group[survey$Code=="AEB56L"] <- "M" # instrumentalists AEB56L

save(survey, file="input/allgroups_survey_processed.RData")

D <- survey

rm(survey,survey2,musicians)

#------------------------------------------------------------------------------------------------------------------#
#                                   Demographics all groups                                                        #
#------------------------------------------------------------------------------------------------------------------#

#-> this dataset entails the information of 40 musicians, 88 amateurs and 38 non-musicians

## Meaning of Variables

# participant                       individual participant code assigned by PsyToolkit
# Code                              self-generated Code by the participant
# Group                             group - M = Professionals; C = Non-Musicians/Controls; A = Amateurs
# Age                               participants' age
# LSex                              participants' sex, 1 = female, 2 = male, 3 = diverse, 4 = no information
# LMotherLanguage                   participants' mother language, 1 = German, 2 = English, 3 = other
# LProfession                       participants' profession (answers in German)
# LHearingImp                       indication of any known hearing impairments, 1 = No, 2 = yes, 3 = I don't know
# LHearingImp2                      if yes, which kind of impairment (free text response in German)
# LHearingImp3                      if yes, how much does it constrain you in daily life (1 = not at all, 2 = a little bit, 3 = to some degree, 4 = substantially)
# TimeTotal                         total time to complete the survey + experiment (minutes)
# AgeTraining                       age at instrument/singing training onset 
# AQ_SocialSkills : AQ_Total        different subscores of the AQ-Questionnaire - refer to Hoekstra et al. (2008) and Baron-Cohen et al. (2001)
# O : N (OCEAN)                     scores of the Big-Five questionnaire - refer to Rammstedt et al. (2018)
# Mean_active : ME_mean             mean scores of the Gold-MSI - refer to M?llensiefen et al. (2014)
# Instrument                        participants' instrument (in German)
# LMusicQualification_1             indication of non-academic music qualification (1 = yes, 2 = no)
# LMusicDegree_1                    indication of a academic music qualification (1 = yes, 2 = no)
# LMusicDegree2_1                   if yes, which one (1 = Bachelor, 2 = Master, 3 = Diploma, 4 = PhD, 5 = other)
# LPerfectPitch_1                   indication of a perfect pitch (absolute hearing ability)
# YearsTraining                     years of formal music training (1 = 0, 2 = 1, 3 = 2, 4 = 3, 5 = 4-5, 6 = 6-9, 7 = 10 or more)
# pos_Aff : neg_Aff                 scores of the PANAS - refer to Breyer & Bluemke (2016)
# Schulabschluss                    highest school degree (according to German system - refer to supplemental tables)
# Ausbildung                        highest level of education (according to German system - refer to supplemental tables)
# Einkommen                         monthly household income (1 = less than 1750???, 2 = 1750-2500???, 3 = 2500-3500???, 4 = 3500-5000???, 5 = more than 5000???)
# Instrument2                       recode the instrument column to account for any spelling mistakes or different expressions for the same instrument



#------------------------------------------------------------------------------------------------------------------#
#                                     Age                                    
#------------------------------------------------------------------------------------------------------------------# 

# descriptive data
Age <- mySummary(D, Age, Group)
range <- D %>% group_by(Group) %>% summarise(range(Age))

###### ANOVA ########
# data = D
# dv = Age
# wid = Code
# between = Group
anova_Age<-ezANOVA(data=D, wid=Code, dv=Age, between= Group, type=3, detailed=TRUE)
b_Age = tracedEzOut(anova_Age, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

### calculate omega effect sizes: 
o2_age <- F_to_omega2(unlist(c(anova_Age$ANOVA[6])),  unlist(c(anova_Age$ANOVA[2])),  unlist(c(anova_Age$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_Age <- D %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., Age ~ Group)
d_MA_Age <- t_to_d(MA_Age$statistic, MA_Age$parameter, paired = FALSE)
MC_Age <- D %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., Age ~ Group)
d_MC_Age <- t_to_d(MC_Age$statistic, MC_Age$parameter, paired = FALSE)
AC_Age <- D %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., Age ~ Group)
d_AC_Age <- t_to_d(AC_Age$statistic, AC_Age$parameter, paired = FALSE)

#add descriptions for output:
info <- c("Descriptive Data","Professinals vs Amateurs", "Professionals vs NonMusicians", "Amateurs vs NonMusicians")

capture.output(info[1], as.matrix(Age), as.matrix(range),b_Age, o2_age,
               info[2], MA_Age, d_MA_Age, info[3], MC_Age, d_MC_Age, info[4], 
               AC_Age, d_AC_Age, 
               file="output/sample_description/age_allgroups.txt")
rm(Age, range, anova_Age, b_Age, o2_age, MA_Age, MC_Age, AC_Age, d_MA_Age, d_MC_Age, d_AC_Age)

#------------------------------------------------------------------------------------------------------------------#
#                                     Other Demographics                                    
#------------------------------------------------------------------------------------------------------------------# 

sex <- table(D$Group, D$LSex)
language <- table(D$Group, D$LMotherLanguage)
profession <- table(D$LProfession)
hearingImp <- table(D$Group, D$LHearingImp)
hearingImp2 <- table(D$Group, D$LHearingImp2)
hearingImp3 <- table(D$Group, D$LHearingImp3)
duration <- D %>% filter(TimeTotal < 999) %>%  group_by(Group) %>% summarise(Dur = mean(TimeTotal))
info <- c("Sex", "Language", "Profession", "HearingImpairment", "HearinImpairment2", "HearingImpairment3", "Duration")

capture.output(info[1], sex, info[2], language, info[3], profession, 
               info[4], hearingImp, info[5], hearingImp2, info[6], 
               hearingImp3, info[7], as.matrix(duration), 
               file="output/sample_description/demographics_allgroups.txt")
rm(sex, language, profession, hearingImp, hearingImp2, hearingImp3, duration)


#------------------------------------------------------------------------------------------------------------------#
#                                           PANAS                                                                #
#------------------------------------------------------------------------------------------------------------------#

# descriptive_data
PANAS <- D %>% group_by(Group) %>% summarise(Neg = mean(neg_Aff),
                                                  NegSD = sd(neg_Aff),
                                                  Pos = mean(pos_Aff),
                                                  PosSD = sd(pos_Aff))

#PANAS neg_Aff
anova_neg <- ezANOVA(data=D, wid=Code, dv=neg_Aff, between= Group, type=3, detailed=TRUE)
neg = tracedEzOut(anova_neg, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

### calculate omega effect sizes: 
o2_neg <- F_to_omega2(unlist(c(anova_neg$ANOVA[6])),  unlist(c(anova_neg$ANOVA[2])),  unlist(c(anova_neg$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2

# PANAS neg_Aff: pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_neg <- D %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., neg_Aff ~ Group)
d_MA_neg <- t_to_d(MA_neg$statistic, MA_neg$parameter, paired = FALSE)
MC_neg <- D %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., neg_Aff ~ Group)
d_MC_neg <- t_to_d(MC_neg$statistic, MC_neg$parameter, paired = FALSE)
AC_neg <- D %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., neg_Aff ~ Group)
d_AC_neg <- t_to_d(AC_neg$statistic, AC_neg$parameter, paired = FALSE)

#PANAS pos_Aff
anova_pos <- ezANOVA(data=D, wid=Code, dv=pos_Aff, between= Group, type=3, detailed=TRUE)
pos = tracedEzOut(anova_pos, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

### calculate omega effect sizes: 
o2_pos <- F_to_omega2(unlist(c(anova_pos$ANOVA[6])),  unlist(c(anova_pos$ANOVA[2])),  unlist(c(anova_pos$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2

# PANAS pos_Aff:pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_pos <- D %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., pos_Aff ~ Group)
d_MA_pos <- t_to_d(MA_pos$statistic, MA_pos$parameter, paired = FALSE)
MC_pos <- D %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., pos_Aff ~ Group)
d_MC_pos <- t_to_d(MC_pos$statistic, MC_pos$parameter, paired = FALSE)
AC_pos <- D %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., pos_Aff ~ Group)
d_AC_pos <- t_to_d(AC_pos$statistic, AC_pos$parameter, paired = FALSE)

#add descriptions for output:
info <- c("Descriptive Data","Professionals vs Amateurs (neg_Aff)", "Professionals vs NonMusicians (neg_Aff)", "Amateurs vs NonMusicians (neg_Aff)",
          "Musicians vs Amateurs (pos_Aff)", "Musicians vs NonMusicians (pos_Aff)", "Amateurs vs NonMusicians (pos_Aff)", "neg_Aff", "pos_Aff")

capture.output(info[1], as.matrix(PANAS), info[8], neg, o2_neg, info[9], pos, o2_pos,
               info[2], MA_neg, d_MA_neg, info[3], MC_neg, d_MC_neg, info[4], AC_neg, d_AC_neg,
               info[5], MA_pos, d_MA_pos, info[6], MC_pos, d_MC_pos, info[7], AC_pos, d_AC_pos,
               file="output/sample_description/PANAS_allgroups.txt")
rm(PANAS, anova_neg, anova_pos, neg, pos, o2_neg, o2_pos,MA_neg, d_MA_neg, MC_neg, d_MC_neg, 
        AC_neg, d_AC_neg, MA_pos, d_MA_pos, MC_pos, d_MC_pos, AC_pos, d_AC_pos)

#------------------------------------------------------------------------------------------------------------------#
#                                           AQ Data                                                                #
#------------------------------------------------------------------------------------------------------------------#

# descriptive_data
AQ_summary <- D %>% group_by(Group) %>% summarise(AQ_Tot = mean(AQ_Total),   # AQ Total
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

##AQ ANOVA
# AQ Total

anova_AQTot<-ezANOVA(data=D, wid=Code, dv=AQ_Total, between= Group, type=3, detailed=TRUE)
AQ_total = tracedEzOut(anova_AQTot, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

### calculate omega effect sizes: 
o2_AQTot <- F_to_omega2(unlist(c(anova_AQTot$ANOVA[6])),  unlist(c(anova_AQTot$ANOVA[2])),  unlist(c(anova_AQTot$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_AQTot <- D %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., AQ_Total ~ Group)
d_MA_AQTot <- t_to_d(MA_AQTot$statistic, MA_AQTot$parameter, paired = FALSE)
MC_AQTot <- D %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., AQ_Total ~ Group)
d_MC_AQTot <- t_to_d(MC_AQTot$statistic, MC_AQTot$parameter, paired = FALSE)
AC_AQTot <- D %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., AQ_Total ~ Group)
d_AC_AQTot <- t_to_d(AC_AQTot$statistic, AC_AQTot$parameter, paired = FALSE)


# AQ Attention to Detail
anova_AQATT<-ezANOVA(data=D, wid=Code, dv=AQ_AttentionToDetails, between= Group, type=3, detailed=TRUE)
AQ_ATT = tracedEzOut(anova_AQATT, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

### calculate omega effect sizes: 
o2_AQATT <- F_to_omega2(unlist(c(anova_AQATT$ANOVA[6])),  unlist(c(anova_AQATT$ANOVA[2])),  unlist(c(anova_AQATT$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_AQ_ATT <- D %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., AQ_AttentionToDetails ~ Group)
d_MA_AQ_ATT <- t_to_d(MA_AQ_ATT$statistic, MA_AQ_ATT$parameter, paired = FALSE)
MC_AQ_ATT <- D %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., AQ_AttentionToDetails ~ Group)
d_MC_AQ_ATT <- t_to_d(MC_AQ_ATT$statistic, MC_AQ_ATT$parameter, paired = FALSE)
AC_AQ_ATT <- D %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., AQ_AttentionToDetails ~ Group)
d_AC_AQ_ATT <- t_to_d(AC_AQ_ATT$statistic, AC_AQ_ATT$parameter, paired = FALSE)



#AQ Social
anova_AQSoc<-ezANOVA(data=D, wid=Code, dv=AQ_Social, between= Group, type=3, detailed=TRUE)
AQ_Soc = tracedEzOut(anova_AQSoc, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

### calculate omega effect sizes: 
o2_AQSoc <- F_to_omega2(unlist(c(anova_AQSoc$ANOVA[6])),  unlist(c(anova_AQSoc$ANOVA[2])),  unlist(c(anova_AQSoc$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_AQ_Social <- D %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., AQ_Social ~ Group)
d_MA_AQ_Social <- t_to_d(MA_AQ_Social$statistic, MA_AQ_Social$parameter, paired = FALSE)
MC_AQ_Social <- D %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., AQ_Social ~ Group)
d_MC_AQ_Social <- t_to_d(MC_AQ_Social$statistic, MC_AQ_Social$parameter, paired = FALSE)
AC_AQ_Social <- D %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., AQ_Social ~ Group)
d_AC_AQ_Social <- t_to_d(AC_AQ_Social$statistic, AC_AQ_Social$parameter, paired = FALSE)



# AQ Social Skills
anova_AQSocSki<-ezANOVA(data=D, wid=Code, dv=AQ_SocialSkills, between= Group, type=3, detailed=TRUE)
AQ_SocSki = tracedEzOut(anova_AQSocSki, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

### calculate omega effect sizes: 
o2_AQSocSki <- F_to_omega2(unlist(c(anova_AQSocSki$ANOVA[6])),  unlist(c(anova_AQSocSki$ANOVA[2])),  unlist(c(anova_AQSocSki$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2


# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_AQ_SocSkills <- D %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., AQ_SocialSkills ~ Group)
d_MA_AQ_SocSkills <- t_to_d(MA_AQ_SocSkills$statistic, MA_AQ_SocSkills$parameter, paired = FALSE)
MC_AQ_SocSkills <- D %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., AQ_SocialSkills ~ Group)
d_MC_AQ_SocSkills <- t_to_d(MC_AQ_SocSkills$statistic, MC_AQ_SocSkills$parameter, paired = FALSE)
AC_AQ_SocSkills <- D %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., AQ_SocialSkills ~ Group)
d_AC_AQ_SocSkills <- t_to_d(AC_AQ_SocSkills$statistic, AC_AQ_SocSkills$parameter, paired = FALSE)



# AQ Communication
anova_AQCom<-ezANOVA(data=D, wid=Code, dv=AQ_Communication, between= Group, type=3, detailed=TRUE)
AQ_Com = tracedEzOut(anova_AQCom, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

### calculate omega effect sizes: 
o2_AQCom <- F_to_omega2(unlist(c(anova_AQCom$ANOVA[6])),  unlist(c(anova_AQCom$ANOVA[2])),  unlist(c(anova_AQCom$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_AQ_Com <- D %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., AQ_Communication ~ Group)
d_MA_AQ_Com <- t_to_d(MA_AQ_Com$statistic, MA_AQ_Com$parameter, paired = FALSE)
MC_AQ_Com <- D %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., AQ_Communication ~ Group)
d_MC_AQ_Com <- t_to_d(MC_AQ_Com$statistic, MC_AQ_Com$parameter, paired = FALSE)
AC_AQ_Com <- D %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., AQ_Communication ~ Group)
d_AC_AQ_Com <- t_to_d(AC_AQ_Com$statistic, AC_AQ_Com$parameter, paired = FALSE)



# AQ Imagination
anova_AQIma<-ezANOVA(data=D, wid=Code, dv=AQ_Imagination, between= Group, type=3, detailed=TRUE)
AQ_Ima = tracedEzOut(anova_AQIma, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

### calculate omega effect sizes: 
o2_AQIma <- F_to_omega2(unlist(c(anova_AQIma$ANOVA[6])),  unlist(c(anova_AQIma$ANOVA[2])),  unlist(c(anova_AQIma$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_AQ_Ima <- D %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., AQ_Imagination ~ Group)
d_MA_AQ_Ima <- t_to_d(MA_AQ_Ima$statistic, MA_AQ_Ima$parameter, paired = FALSE)
MC_AQ_Ima <- D %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., AQ_Imagination ~ Group)
d_MC_AQ_Ima <- t_to_d(MC_AQ_Ima$statistic, MC_AQ_Ima$parameter, paired = FALSE)
AC_AQ_Ima <- D %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., AQ_Imagination ~ Group)
d_AC_AQ_Ima <- t_to_d(AC_AQ_Ima$statistic, AC_AQ_Ima$parameter, paired = FALSE)


#######################################################
#expected output (reduced)
# F-statistic: 1.885 on 2 and 163 DF,  p-value: 0.1552
#######################################################

# AQ Attention Switch
anova_AQAttSwi<-ezANOVA(data=D, wid=Code, dv=AQ_AttentionSwitching, between= Group, type=3, detailed=TRUE)
AQ_AttSwi = tracedEzOut(anova_AQAttSwi, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

### calculate omega effect sizes: 
o2_AQAttSwi <- F_to_omega2(unlist(c(anova_AQAttSwi$ANOVA[6])),  unlist(c(anova_AQAttSwi$ANOVA[2])),  unlist(c(anova_AQAttSwi$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_AQ_AttSwi <- D %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., AQ_AttentionSwitching ~ Group)
d_MA_AQ_AttSwi <- t_to_d(MA_AQ_AttSwi$statistic, MA_AQ_AttSwi$parameter, paired = FALSE)
MC_AQ_AttSwi <- D %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., AQ_AttentionSwitching ~ Group)
d_MC_AQ_AttSwi <- t_to_d(MC_AQ_AttSwi$statistic, MC_AQ_AttSwi$parameter, paired = FALSE)
AC_AQ_AttSwi <- D %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., AQ_AttentionSwitching ~ Group)
d_AC_AQ_AttSwi <- t_to_d(AC_AQ_AttSwi$statistic, AC_AQ_AttSwi$parameter, paired = FALSE)



#add descriptions for output:
info <- c("Descriptive Data","AQ Total","AQ Attention","AQ Social","AQ Social Skills","AQ Communication","AQ Imagination","AQ Attention Switch",
          "Professionals vs Amateurs (AQ Att)", "Professionals vs NonMusicians (AQ Att)", "Amateurs vs NonMusicians (AQ Att)",
          "Professionals vs Amateurs (AQ Soc)", "Professionals vs NonMusicians (AQ Soc)", "Amateurs vs NonMusicians (AQ Soc)",
          "Professionals vs Amateurs (AQ SocSkills)", "Professionals vs NonMusicians (AQ SocSkills)", "Amateurs vs NonMusicians (AQ SocSkilss)",
          "Professionals vs Amateurs (AQ Total)", "Professionals vs NonMusicians (AQ Total)", "Amateurs vs NonMusicians (AQ Total)",
          "Professionals vs Amateurs (AQ Communication)", "Professionals vs NonMusicians (AQ Communication)", "Amateurs vs NonMusicians (AQ Communication)",
          "Professionals vs Amateurs (AQ Imagination)", "Professionals vs NonMusicians (AQ Imagination)", "Amateurs vs NonMusicians (AQ Imagination)",
          "Professionals vs Amateurs (AQ Attention Switching)", "Professionals vs NonMusicians (AQ Attention Switching)", "Amateurs vs NonMusicians (AQ Attention Switching)")


capture.output(info[1], as.matrix(AQ_summary), info[2],AQ_total, o2_AQTot, 
               info[3], AQ_ATT, o2_AQATT, info[4], AQ_Soc, o2_AQSoc, 
               info[5], AQ_SocSki, o2_AQSocSki, info[6], AQ_Com, o2_AQCom, 
               info[7], AQ_Ima, o2_AQIma, info[8], AQ_AttSwi, o2_AQAttSwi,
               info[9], MA_AQ_ATT, d_MA_AQ_ATT, info[10],MC_AQ_ATT,d_MC_AQ_ATT, info[11],AC_AQ_ATT,d_AC_AQ_ATT,
               info[12],MA_AQ_Social,d_MA_AQ_Social, info[13],MC_AQ_Social,d_MC_AQ_Social, info[14], AC_AQ_Social, d_AC_AQ_Social,
               info[15],MA_AQ_SocSkills,d_MA_AQ_SocSkills,info[16], MC_AQ_SocSkills,d_MC_AQ_SocSkills, info[17], AC_AQ_SocSkills,d_AC_AQ_SocSkills,
               info[18], MA_AQTot, d_MA_AQTot, info[19], MC_AQTot, d_MC_AQTot, info[20], AC_AQTot, d_AC_AQTot,
               info[21], MA_AQ_Com, d_MA_AQ_Com, info[22], MC_AQ_Com, d_MC_AQ_Com, info[23], AC_AQ_Com, d_AC_AQ_Com,
               info[24],MA_AQ_Ima, d_MA_AQ_Ima, info[25], MC_AQ_Ima, d_MC_AQ_Ima, info[26], AC_AQ_Ima, d_AC_AQ_Ima,
               info[27], MA_AQ_AttSwi, d_MA_AQ_AttSwi, info[28], MC_AQ_AttSwi, d_MC_AQ_AttSwi, info[29], AC_AQ_AttSwi, d_AC_AQ_AttSwi,
               file="output/sample_description/AQ_allgroups.txt")

rm(AQ_summary, AQ_total, AQ_ATT, AQ_Soc, AQ_SocSki, AQ_Com, AQ_Ima, AQ_AttSwi, o2_AQTot,o2_AQATT, o2_AQSoc, o2_AQSocSki, o2_AQCom, o2_AQIma, o2_AQAttSwi, 
   anova_AQATT, anova_AQAttSwi, anova_AQCom, anova_AQIma, anova_AQSoc, anova_AQSocSki, anova_AQTot,
   MA_AQ_ATT, d_MA_AQ_ATT, MC_AQ_ATT,d_MC_AQ_ATT, AC_AQ_ATT,d_AC_AQ_ATT, MA_AQ_Social,d_MA_AQ_Social, MC_AQ_Social,d_MC_AQ_Social, AC_AQ_Social, d_AC_AQ_Social,
   MA_AQ_SocSkills,d_MA_AQ_SocSkills,MC_AQ_SocSkills,d_MC_AQ_SocSkills, AC_AQ_SocSkills,d_AC_AQ_SocSkills, 
   MA_AQTot, d_MA_AQTot, MC_AQTot, d_MC_AQTot, AC_AQTot, d_AC_AQTot,
   MA_AQ_Com, d_MA_AQ_Com, MC_AQ_Com, d_MC_AQ_Com, AC_AQ_Com, d_AC_AQ_Com,
   MA_AQ_Ima, d_MA_AQ_Ima, MC_AQ_Ima, d_MC_AQ_Ima, AC_AQ_Ima, d_AC_AQ_Ima,
   MA_AQ_AttSwi, d_MA_AQ_AttSwi, MC_AQ_AttSwi, d_MC_AQ_AttSwi, AC_AQ_AttSwi, d_AC_AQ_AttSwi)

#------------------------------------------------------------------------------------------------------------------#
#                                           GOLD MSI                                                               #
#------------------------------------------------------------------------------------------------------------------#

# descriptive_data
MSI_summary <- D %>% group_by(Group) %>% summarise(ME = mean(ME_mean), 
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
ME <- ezANOVA(data=D, wid=Code, dv=ME_mean, between= Group, type=3, detailed=TRUE)
b_ME = tracedEzOut(ME, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

### calculate omega effect sizes: 
o2_ME <- F_to_omega2(unlist(c(ME$ANOVA[6])),  unlist(c(ME$ANOVA[2])),  unlist(c(ME$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_ME <- D %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., ME_mean ~ Group)
d_MA_ME <- t_to_d(MA_ME$statistic, MA_ME$parameter, paired = FALSE)
MC_ME <- D %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., ME_mean ~ Group)
d_MC_ME <- t_to_d(MC_ME$statistic, MC_ME$parameter, paired = FALSE)
AC_ME <- D %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., ME_mean ~ Group)
d_AC_ME <- t_to_d(AC_ME$statistic, AC_ME$parameter, paired = FALSE)



Active <- ezANOVA(data=D, wid=Code, dv=Mean_active, between= Group, type=3, detailed=TRUE)
b_Active = tracedEzOut(Active, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

### calculate omega effect sizes: 
o2_Active <- F_to_omega2(unlist(c(Active$ANOVA[6])),  unlist(c(Active$ANOVA[2])),  unlist(c(Active$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_Active <- D %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., Mean_active ~ Group)
d_MA_Active <- t_to_d(MA_Active$statistic, MA_Active$parameter, paired = FALSE)
MC_Active <- D %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., Mean_active ~ Group)
d_MC_Active <- t_to_d(MC_Active$statistic, MC_Active$parameter, paired = FALSE)
AC_Active <- D %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., Mean_active ~ Group)
d_AC_Active <- t_to_d(AC_Active$statistic, AC_Active$parameter, paired = FALSE)



Education <- ezANOVA(data=D, wid=Code, dv=Mean_education, between= Group, type=3, detailed=TRUE)
b_Education = tracedEzOut(Education, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

### calculate omega effect sizes: 
o2_Education <- F_to_omega2(unlist(c(Education$ANOVA[6])),  unlist(c(Education$ANOVA[2])),  unlist(c(Education$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_Education <- D %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., Mean_education ~ Group)
d_MA_Education <- t_to_d(MA_Education$statistic, MA_Education$parameter, paired = FALSE)
MC_Education <- D %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., Mean_education ~ Group)
d_MC_Education <- t_to_d(MC_Education$statistic, MC_Education$parameter, paired = FALSE)
AC_Education <- D %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., Mean_education ~ Group)
d_AC_Education <- t_to_d(AC_Education$statistic, AC_Education$parameter, paired = FALSE)



Emotion <- ezANOVA(data=D, wid=Code, dv=Mean_emotion, between= Group, type=3, detailed=TRUE)
b_Emotion = tracedEzOut(Emotion, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

### calculate omega effect sizes: 
o2_Emotion <- F_to_omega2(unlist(c(Emotion$ANOVA[6])),  unlist(c(Emotion$ANOVA[2])),  unlist(c(Emotion$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2


# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_Emotion <- D %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., Mean_emotion ~ Group)
d_MA_Emotion <- t_to_d(MA_Emotion$statistic, MA_Emotion$parameter, paired = FALSE)
MC_Emotion <- D %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., Mean_emotion ~ Group)
d_MC_Emotion <- t_to_d(MC_Emotion$statistic, MC_Emotion$parameter, paired = FALSE)
AC_Emotion <- D %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., Mean_emotion ~ Group)
d_AC_Emotion <- t_to_d(AC_Emotion$statistic, AC_Emotion$parameter, paired = FALSE)



Singing <- ezANOVA(data=D, wid=Code, dv=Mean_singing, between= Group, type=3, detailed=TRUE)
b_Singing = tracedEzOut(Singing, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

### calculate omega effect sizes: 
o2_Singing <- F_to_omega2(unlist(c(Singing$ANOVA[6])),  unlist(c(Singing$ANOVA[2])),  unlist(c(Singing$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2

# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_Singing <- D %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., Mean_singing ~ Group)
d_MA_Singing <- t_to_d(MA_Singing$statistic, MA_Singing$parameter, paired = FALSE)
MC_Singing <- D %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., Mean_singing ~ Group)
d_MC_Singing <- t_to_d(MC_Singing$statistic, MC_Singing$parameter, paired = FALSE)
AC_Singing <- D %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., Mean_singing ~ Group)
d_AC_Singing <- t_to_d(AC_Singing$statistic, AC_Singing$parameter, paired = FALSE)



Perception <- ezANOVA(data=D, wid=Code, dv=Mean_perception, between= Group, type=3, detailed=TRUE)
b_Perception = tracedEzOut(Perception, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

### calculate omega effect sizes: 
o2_Perception <- F_to_omega2(unlist(c(Perception$ANOVA[6])),  unlist(c(Perception$ANOVA[2])),  unlist(c(Perception$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2


# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_Perc <- D %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., Mean_perception ~ Group)
d_MA_Perc <- t_to_d(MA_Perc$statistic, MA_Perc$parameter, paired = FALSE)
MC_Perc <- D %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., Mean_perception ~ Group)
d_MC_Perc <- t_to_d(MC_Perc$statistic, MC_Perc$parameter, paired = FALSE)
AC_Perc <- D %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., Mean_perception ~ Group)
d_AC_Perc <- t_to_d(AC_Perc$statistic, AC_Perc$parameter, paired = FALSE)


info <- c("ME","Active","Education","Emotion","Singing","Perception",
          "Professionals vs Amateurs (ME)", "Professionals vs NonMusicians (ME)", "Amateurs vs NonMusicians (ME)",
          "Professionals vs Amateurs (Active)", "Professionals vs NonMusicians (Active)", "Amateurs vs NonMusicians (Active)",
          "Professionals vs Amateurs (Education)", "Professionals vs NonMusicians (Education)", "Amateurs vs NonMusicians (Education)",
          "Professionals vs Amateurs (Emotion)", "Professionals vs NonMusicians (Emotion)", "Amateurs vs NonMusicians (Emotion)",
          "Professionals vs Amateurs (Singing)", "Professionals vs NonMusicians (Singing)", "Amateurs vs NonMusicians (Singing)",
          "Professionals vs Amateurs (Perception)", "Professionals vs NonMusicians (Perception)", "Amateurs vs NonMusicians (Perception)")

capture.output(as.matrix(MSI_summary),info[1],b_ME, o2_ME,info[2],b_Active, o2_Active,info[3],b_Education, o2_Education,info[4],b_Emotion, o2_Emotion,info[5], b_Singing, o2_Singing, info[6],b_Perception, o2_Perception,
               info[7], MA_ME, d_MA_ME, info[8],MC_ME,d_MC_ME,info[9], AC_ME, d_AC_ME, 
               info[10],MA_Active,d_MA_Active, info[11],MC_Active,d_MC_Active, info[12],AC_Active,d_AC_Active, 
               info[13],MA_Education,d_MA_Education,info[14], MC_Education, d_MC_Education, info[15],AC_Education,d_AC_Education,
               info[16],MA_Emotion,d_MA_Emotion, info[17],MC_Emotion ,d_MC_Emotion,info[18],AC_Emotion,d_AC_Emotion, 
               info[19],MA_Singing,d_MA_Singing,info[20],MC_Singing,d_MC_Singing,info[21],AC_Singing,d_AC_Singing,
               info[22],MA_Perc,d_MA_Perc,info[23],MC_Perc,d_MC_Perc,info[24],AC_Perc,d_AC_Perc,
               file="output/musicality/Gold-MSI_allgroups.txt")

rm(MSI_summary, ME, Active, Education, Emotion, Singing, Perception, info, o2_Perception, o2_Singing, o2_Emotion, o2_Education, o2_Active, o2_ME,b_ME, b_Active,b_Perception,b_Singing,b_Education,b_Emotion,
   MA_ME, d_MA_ME, MC_ME,d_MC_ME,AC_ME, d_AC_ME, 
   MA_Active,d_MA_Active, MC_Active,d_MC_Active, AC_Active,d_AC_Active, 
   MA_Education,d_MA_Education, MC_Education, d_MC_Education, AC_Education,d_AC_Education,
   MA_Emotion,d_MA_Emotion, MC_Emotion,d_MC_Emotion,AC_Emotion,d_AC_Emotion, 
   MA_Singing,d_MA_Singing,MC_Singing,d_MC_Singing,AC_Singing,d_AC_Singing,
   MA_Perc,d_MA_Perc,MC_Perc,d_MC_Perc,AC_Perc,d_AC_Perc)


#------------------------------------------------------------------------------------------------------------------#
#                                           OCEAN                                                                  #
#------------------------------------------------------------------------------------------------------------------#

# descriptive_data
OCEAN <- D %>% group_by(Group) %>% summarise(o = round(mean(O), 2),
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
O <- ezANOVA(data=D, wid=Code, dv= O, between= Group, type=3, detailed=TRUE)
b_O = tracedEzOut(O, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

### calculate omega effect sizes: 
o2_O <- F_to_omega2(unlist(c(O$ANOVA[6])),  unlist(c(O$ANOVA[2])),  unlist(c(O$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2




C <- ezANOVA(data=D, wid=Code, dv= C, between= Group, type=3, detailed=TRUE)
b_C = tracedEzOut(C, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

### calculate omega effect sizes: 
o2_C <- F_to_omega2(unlist(c(C$ANOVA[6])),  unlist(c(C$ANOVA[2])),  unlist(c(C$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2




E <- ezANOVA(data=D, wid=Code, dv= E, between= Group, type=3, detailed=TRUE)
b_E = tracedEzOut(E, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

### calculate omega effect sizes: 
o2_E <- F_to_omega2(unlist(c(E$ANOVA[6])),  unlist(c(E$ANOVA[2])),  unlist(c(E$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2


# perform pairwise t-tests, Bonferroni-corrected a-level: .05/3 = .0167
MA_E <- D %>% filter(Group == "M" | Group == "A") %>% t.test(data = ., E ~ Group)
d_MA_E <- t_to_d(MA_E$statistic, MA_E$parameter, paired = FALSE)
MC_E <- D %>% filter(Group == "M" | Group == "C") %>% t.test(data = ., E ~ Group)
d_MC_E <- t_to_d(MC_E$statistic, MC_E$parameter, paired = FALSE)
AC_E <- D %>% filter(Group == "A" | Group == "C") %>% t.test(data = ., E ~ Group)
d_AC_E <- t_to_d(AC_E$statistic, AC_E$parameter, paired = FALSE)



A <- ezANOVA(data=D, wid=Code, dv= A, between= Group, type=3, detailed=TRUE)
b_A = tracedEzOut(A, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

### calculate omega effect sizes: 
o2_A <- F_to_omega2(unlist(c(A$ANOVA[6])),  unlist(c(A$ANOVA[2])),  unlist(c(A$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2




N <- ezANOVA(data=D, wid=Code, dv= N, between= Group, type=3, detailed=TRUE)
b_N = tracedEzOut(N, print = TRUE, sph.cor = "HF", mau.p = 0.05, etasq = "partial", dfsep = ", ")

### calculate omega effect sizes: 
o2_N <- F_to_omega2(unlist(c(N$ANOVA[6])),  unlist(c(N$ANOVA[2])),  unlist(c(N$ANOVA[3])), ci = 0.95, alternative = "two.sided")   # enter F, df1, df2




#add descriptions for output:
info <- c("Professionals vs Amateurs (E)", "Professionals vs NonMusicians (E)", "Amateurs vs NonMusicians (E)",
          "Openness", "Conscientiousness", "Extraversion","Agreeableness","Neuroticism")
          
capture.output(as.matrix(OCEAN), info[4], b_O, o2_O, info[5],b_C, o2_C, info[6],b_E, o2_E, info[7],b_A, o2_A, info[8], b_N, o2_N,
               info[1],MA_E,d_MA_E,info[2],MC_E,d_MC_E,info[3],AC_E,d_AC_E,
               file="output/sample_description/OCEAN_allgroups.txt")
rm(info, OCEAN, O,C,E,A,N,o2_O, o2_C, o2_E,o2_A,o2_N, b_O, b_C,b_E,b_A,b_N,MA_E,d_MA_E,MC_E,d_MC_E,AC_E,d_AC_E)


#------------------------------------------------------------------------------------------------------------------#
#                                              SES                                                                 #
#------------------------------------------------------------------------------------------------------------------#

#Einkommen (Income)
Einkommen <- chisq.test(D$Group, D$Einkommen)

library(rcompanion)
cohenW <- cohenW(D$Group, D$Einkommen)
cramerV <- cramerV(D$Group, D$Einkommen)

#Schulabschluss (Education)
Schulabschluss <- chisq.test(D$Group, D$Schulabschluss)

#Ausbildung (Degree)
Ausbildung <- chisq.test(D$Group, D$Ausbildung)

capture.output(Einkommen, cohenW, cramerV, table(D$Einkommen, D$Group),  Schulabschluss, table(D$Schulabschluss,D$Group), Ausbildung, table(D$Ausbildung, D$Group), file="output/sample_description/SES_allgroups.txt")
rm(Schulabschluss, Einkommen, Ausbildung, cohenW, cramerV)



#------------------------------------------------------------------------------------------------------------------#
#                                   Musical Training, Professionals only                                           #
#------------------------------------------------------------------------------------------------------------------#


### Age at onset

## select  professionals only
musicians <- D %>% filter(Group =="M")


AgeTraining <- mySummary(musicians, AgeTraining)
RangeAge <- range(musicians$AgeTraining)

capture.output(as.matrix(AgeTraining), RangeAge, file= "output/sample_description/Professionals_onsetAge.txt")

rm(AgeTraining, RangeAge)


######## music qualification ################


#non-academic music qualifications: 
# - earned money with it
# - recorded and published an album
# - won a competition
# - were enrolled as music student (made it through the qualifying examination)


Qualification <- table(musicians$LMusicQualification_1)
Degree <- table(musicians$LMusicDegree_1)
Qualification_without_degree <- table(musicians$LMusicQualification_1[musicians$LMusicDegree_1== 2])

info <- c("Music Qualification", "Music Degree", "Music Qualification in absence of degree")

capture.output(info[1], as.matrix(Qualification), info[2], as.matrix(Degree), info[3], as.matrix(Qualification_without_degree), file= "output/sample_description/Musicians_degree.txt")

rm(info, Qualification, Degree, Qualification_without_degree)

# years of musical training

yearsTraining <- table(musicians$YearsTraining)
capture.output(as.matrix(yearsTraining), file = "output/sample_description/Professionals_yearsTraining.txt")

# Instruments

Instruments <- as.data.frame(table(musicians$Instrument2))
capture.output(as.matrix(Instruments), file = "output/sample_description/Professionals_Instruments.txt")

rm(Instruments, yearsTraining)
rm(musicians)



#------------------------------------------------------------------------------------------------------------------#
#                                   Musical Training, Non-Musicians only                                           #
#------------------------------------------------------------------------------------------------------------------#

## non-musicians only
nonmusicians <- D %>% filter(Group =="C")

#fix the age column
nonmusicians$AgeTraining <- ifelse(is.na(nonmusicians$AgeTraining), 0, nonmusicians$AgeTraining )

AgeInstrumentAll<- table(nonmusicians$AgeTraining)
instruments <- nonmusicians %>% filter(AgeTraining > 0)

#Age at onset of training
AgeInstrumentSubSet<- mySummary(instruments, AgeTraining)
Range <- range(instruments$AgeTraining)
capture.output(as.matrix(AgeInstrumentAll), as.matrix(AgeInstrumentSubSet), Range, file= "output/sample_description/NonMusicians_onsetAge.txt")


# years of musical training
yearsTraining<- table(nonmusicians$YearsTraining)
capture.output(as.matrix(yearsTraining), file= "output/sample_description/NonMusicians_yearsTraining.txt")
rm(AgeInstrumentAll, AgeInstrumentSubSet, Range, yearsTraining)


#which Instruments: 
Instruments <- as.data.frame(table(nonmusicians$Instrument2))
capture.output(as.matrix(Instruments), file = "output/sample_description/NonMusicians_Instruments.txt")

rm(Instruments, instruments)
rm(nonmusicians)

#------------------------------------------------------------------------------------------------------------------#
#                                   Musical Training, Amateurs only                                           #
#------------------------------------------------------------------------------------------------------------------#

## amateurs only
amateurs <- D %>% filter(Group =="A")

#fix the age column
amateurs$AgeTraining <- ifelse(is.na(amateurs$AgeTraining), 0, amateurs$AgeTraining )

AgeInstrumentAll<- table(amateurs$AgeTraining)
instruments <- amateurs %>% filter(AgeTraining > 0)

#Age at onset of training
AgeInstrumentSubSet<- mySummary(instruments, AgeTraining)
Range <- range(instruments$AgeTraining)
capture.output(as.matrix(AgeInstrumentAll), as.matrix(AgeInstrumentSubSet), Range, file= "output/sample_description/Amateurs_onsetAge.txt")

# years of musical training
yearsTraining<- table(amateurs$YearsTraining)
capture.output(as.matrix(yearsTraining), file= "output/sample_description/Amateurs_yearsTraining.txt")
rm(AgeInstrumentAll, AgeInstrumentSubSet, Range, yearsTraining)

#which Instruments: 
Instruments <- as.data.frame(table(amateurs$Instrument2))
capture.output(as.matrix(Instruments), file = "output/sample_description/Amateurs_Instruments.txt")

rm(Instruments, instruments)
rm(amateurs)


# End of Script