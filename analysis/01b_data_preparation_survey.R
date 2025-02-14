##########################################################################
## File: 01b_data_preparation_survey.R
## Data Preparation for the Online Survey Data
# authors: Christine Nussbaum, Jessica Senftleben
# date 01/2024

#NOT PUBLISHED

# clear directory
rm(list=ls())


#set working directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# load required packages
library(stringr)  # f?r datenaufbereitung
library(tidyverse)


#------------------------------------------------------------------------------------------------------------------#
#                                          Formatting Survey data                                                  #
#------------------------------------------------------------------------------------------------------------------# 

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
    
    # fix some cells (spelling)
    Raw[Raw=="13 (nicht täglich)"] <- 13
    Raw[Raw=="0,5"] <- 0.5
    Raw[Raw==",5"] <- 0.5
    Raw[Raw=="einmal 2, einmal 3"] <- 3
    Raw[Raw=="14 Jahre Waldhorn"] <- 14
    Raw[Raw=="12,5"] <- 12.5
    Raw[Raw=="23 Jahre"] <- 23
    Raw[Raw=="15 (nicht täglich)"] <- 15
    Raw[Raw=="7-8"] <- 7
    Raw[Raw== "8, zuletzt mit 16"] <- 8
    
    N <- length(Raw$participant)
    Raw$VPN <- paste0("VPN_",c(1:N))
    
    #extract participant and Code
    Raw$participant <- str_sub(Raw$participant , 31, 38)
    Raw$Partcode <- paste0(Raw$LPartCode_1, Raw$LPartCode_2, Raw$LPartCode_3, Raw$LPartCode_4, Raw$LPartCode_5, Raw$LPartCode_6)
    
    #remove irrelevant variables
    Raw <- Raw[ , -c(180:185)]
    
    
    nSubs <- length(unique(Raw$participant))
    print(paste("loaded data from", nSubs, "participants", sep=" "))


        
    #isolate different questionnaires:
    Sample <- Raw[, c(185,1, 3:17, 180, 183, 184)]      # basic sample information
    After_Exp <- Raw[, c(1,19:29)]                      # questionnaire after experiment
    AQ <- Raw[, c(1, 31:80)]                            # AQ questionnaire
    BFI <- Raw[, c(1, 81:110)]                          # Big-Five personality questionnaire
    MSI <- Raw[, c(1, 111:148)]                         # Music-Sophistication-Intex
    Music_Quali <- Raw[, c(1, 149:154, 142)]                 # Musical Qualifications
    SES <-Raw[, c(1, 155:159)]                          # Social Economic Status
    panas <-  Raw[, c(1, 160:179)]                      # Positive Affect Negative Affect Scale
    MYears <- Raw[, c(1,142)]                           # Years of Musical Training
   
    
#------------------------------------------------------------------------------------------------------------------#
# create dataset for later correlation of Musical Training and lessons (in years)
MLessons <- Raw[,c(1,185,142,146)]

# make Code uppercase
MLessons <- MLessons %>% rename(Code = Partcode, YearsTraining = Question32_1, YearsLessons = Question36_1)
MLessons$Code <- toupper(MLessons$Code)

# save dataset 
save(MLessons, file="input/amateurs_Corrdata_MSI_YearsTraining.RData")

# tidy environment
rm(MLessons)

#------------------------------------------------------------------------------------------------------------------#
#                                          Setting up sample information                                           #
#------------------------------------------------------------------------------------------------------------------# 
    
  names(Sample) <- c("Code", "participant", "Age", "LSex", "LMotherLanguage", "LProfession", "Group", 
                     "Chor", "Orchester", "Band", "Gesangsensemble", "Kammerorchester", "anderes", "anderesSpezifisch",
                     "LHearingImp",  "LHearingImp2", "LHearingImp3", "Comments", "TimeTotal", "VPN")              
              
    # make code uppercase
    Sample$Code <-toupper(Sample$Code)
    
#------------------------------------------------------------------------------------------------------------------#
#                                          Calculating AQ Scores                                                   #
#------------------------------------------------------------------------------------------------------------------#
print("Caluclate AQ scores ....")
    
#fix names
names(AQ) <- c("participant", paste0("AQ", c(1:50), "_raw"))
    
    
    ###########################################################"##############
    ## "approximately half of the items were worded to produce a "disagree" response, 
    ## and half an "agree" response in a high scoring person with AS/HFA. 
    ## This was to avoid a response bias either way:" (Baron-Cohen et al., 2001, p.6)
    #########################################################################
    # as already Hoekstra et al. 2008 said: 
    ## "For the items  in which an "agree" response is characterisitc for autism, the scoring was recoded:"
    
    # get the 24 "agree" items:
    # Definitely agree  (score = 1) or slightly agree (score = 2) responses will score 1 point
    # "slightly disagree" (score = 3) or "definitely disagree" (score = 4) will score 0 point on the following items
    #  2,  4,  5,  6, 
    #  7,  9, 12, 13, 16, 
    # 18, 19, 20, 21, 22, 
    # 23, 26, 33, 35, 39,  
    # 41, 42, 43, 45, 46, e.g.
    
    dfAQAgree <-subset(AQ,select=c( "participant", #"AQ01_raw",   
                                    "AQ2_raw", "AQ4_raw", "AQ5_raw", "AQ6_raw", 
                                    "AQ7_raw", "AQ9_raw", "AQ12_raw", "AQ13_raw", "AQ16_raw", 
                                    "AQ18_raw", "AQ19_raw", "AQ20_raw", "AQ21_raw", "AQ22_raw", 
                                    "AQ23_raw", "AQ26_raw", "AQ33_raw", "AQ35_raw", "AQ39_raw", 
                                    "AQ41_raw", "AQ42_raw", "AQ43_raw", "AQ45_raw", "AQ46_raw"))
    
    dfAQAgreeRecoded = dfAQAgree
    #change the colnames --> replace "raw" with "recoded"
    colnames(dfAQAgreeRecoded) = gsub("raw", "recoded", colnames(dfAQAgreeRecoded))
    #recode the items (i.e. 4 --> 1, 3 --> 2, 2 --> 3, 1--4):
    dfAQAgreeRecoded[2:25] = abs(dfAQAgree[2:25]-5)
    remove(dfAQAgree)
    ############################################
    ##get the 26 "disagree" items
    # Definitely disagree (4) or slightly disagree (3) responses will score 1 point
    # "Definitiely agree" (1) and "slightly agree" (2) score 0 points on the following items
    #  1 #this item belongs to this set (it is wrongly assigned to the other set in Baron-Cohen et al., 2001)
    #  3,  8, 10, 11, 14, 
    # 15, 17, 24, 25, 27, 
    # 28, 29, 30, 31, 32, 
    # 34, 36, 37, 38, 40,
    # 44, 47, 48, 49, 50                   
    #examples:
    
    dfAQDisagree <-subset(AQ,select=c("participant", "AQ1_raw", #this item is wronly in the other set (in the orginal paper)
                                      "AQ3_raw", "AQ8_raw", "AQ10_raw", "AQ11_raw", "AQ14_raw", 
                                      "AQ15_raw", "AQ17_raw", "AQ24_raw", "AQ25_raw", "AQ27_raw", 
                                      "AQ28_raw", "AQ29_raw", "AQ30_raw", "AQ31_raw", "AQ32_raw", 
                                      "AQ34_raw", "AQ36_raw", "AQ37_raw", "AQ38_raw", "AQ40_raw", 
                                      "AQ44_raw", "AQ47_raw", "AQ48_raw", "AQ49_raw", "AQ50_raw"))
    
    
    #we need a novel column name, because we wanna have later raw and changed values within the same dataframe:
    #orig stands for "original value" (i.e. not recoded)
    colnames(dfAQDisagree) = gsub("raw", "orig", colnames(dfAQDisagree))
    #############################################
    
    ##combine both the diasgree and the agree items again:
    AQ <-merge(dfAQDisagree, dfAQAgreeRecoded, by = "participant")
    remove(dfAQAgreeRecoded, dfAQDisagree) #cleanup
    
    #bring the agree/disagree items in an alphabetical/numerical order again:  
    AQ = AQ[ , order(names(AQ))] ##--> subject is the last column now
    #reorder the columns, i.e. bring the "subject" column back to front:
    AQ = AQ[c(51,1:50)]

  
    ##change column names:
    colnames(AQ) = gsub("_recoded", "", colnames(AQ))
    colnames(AQ) = gsub("_orig",    "", colnames(AQ))
    
    ## change the values from 1-4 to 0-1 coding:
    ### responses 1 and 2  --> will become  0
    AQ[AQ=="1"]<-0
    AQ[AQ=="2"]<-0
    ### responses 2 and 3 --> will become 1
    AQ[AQ=="3"]<-1
    AQ[AQ=="4"]<-1
    
    #calculate 4 of 5 subdomaines of Baron-Cohen (2001)
    AQ$AQ_SocialSkills       <- rowSums(subset(AQ, select=c("AQ1", "AQ11", "AQ13", "AQ15", "AQ22", "AQ36", "AQ44", "AQ45", "AQ47", "AQ48")), na.rm = TRUE)
    
    AQ$AQ_AttentionSwitching <- rowSums(subset(AQ, select=c("AQ2", "AQ4", "AQ10", "AQ16", "AQ25", "AQ32", "AQ34", "AQ37", "AQ43", "AQ46")),na.rm = TRUE)
    
    AQ$AQ_Communication      <- rowSums(subset(AQ, select=c("AQ7", "AQ17", "AQ18", "AQ26", "AQ27", "AQ31", "AQ33", "AQ35", "AQ38", "AQ39")),na.rm = TRUE)
    
    AQ$AQ_Imagination        <- rowSums(subset(AQ, select=c("AQ3", "AQ8", "AQ14", "AQ20", "AQ21", "AQ24", "AQ40", "AQ41", "AQ42", "AQ50")), na.rm = TRUE)
    
    
    #the first Hoekstra scores is the same as localdetails subdomaine in Baron-Cohen (2001):
    AQ$AQ_AttentionToDetails       <- rowSums(subset(AQ, select=c("AQ5", "AQ6", "AQ9", "AQ12", "AQ19", "AQ23", "AQ28", "AQ29", "AQ30", "AQ49")), na.rm = TRUE)
    ##the Hoekstra score is the same as the sum of all 4 scores of Baron-cohen
    AQ$AQ_Social                   <- rowSums(subset(AQ, select = c("AQ_AttentionSwitching", "AQ_Communication", "AQ_Imagination", "AQ_SocialSkills")), na.rm = TRUE)
    AQ$AQ_Total = AQ$AQ_AttentionToDetails + AQ$AQ_Social
    
    ####keep only the original subscores and the Hoechstra Information
    AQ <- AQ[, c(1, 52:58)]
    
    print(paste0("AQ scores of ", length(unique((AQ$participant))), " participants calculated. Return data frame."))
    
    
#------------------------------------------------------------------------------------------------------------------#
#                                          Calculating BFI Scores                                                  #
#------------------------------------------------------------------------------------------------------------------#

print("Caluclate BFI scores ....")    
    

#fix names 
names(BFI) <- c("participant", "BFI01_raw",  "BFI02_raw",  "BFI03_raw",  "BFI04_raw",  "BFI05_raw",  "BFI06_raw",  "BFI07_raw",
                "BFI08_raw",  "BFI09_raw",  "BFI10_raw", "BFI11_raw", "BFI12_raw", "BFI13_raw", "BFI14_raw", "BFI15_raw",
                "BFI16_raw", "BFI17_raw", "BFI18_raw", "BFI19_raw", "BFI20_raw","BFI21_raw", "BFI22_raw", "BFI23_raw",
                "BFI24_raw", "BFI25_raw", "BFI26_raw","BFI27_raw", "BFI28_raw", "BFI29_raw", "BFI30_raw")
  

# Auswertungshinweise BFI-2-S
# Mit dem BFI-2 k?nnen Skalenwerte f?r die Dom?nen Extraversion, Vertr?glichkeit, Gewissenhaftigkeit,
# Negative Emotionalit?t und Offenheit gebildet werden. Zus?tzlich k?nnen Skalenwerte f?r insgesamt 15
# Facetten gebildet werden. Jeweils drei Facetten lassen sich inhaltlich einer Dimension zuordnen. Dazu werden
# die Antwortkategorien von 1 (stimme ?berhaupt nicht zu) bis 5 (stimme voll und ganz zu) kodiert und
# anschlie?end aggregiert. Negativ kodierte Items m?ssen zuvor rekodiert werden (rekodierter Wert = 6 -Rohwert).
# Extraversion: 1R, 6, 11, 16, 21R, 26R
# Vertr?glichkeit: 2, 7R, 12, 17R, 22, 27R
# Gewissenhaftigkeit: 3R, 8R, 13, 18, 23, 28R
# Negative Emotionalit?t: 4, 9, 14R, 19R, 24R, 29
# Offenheit: 5, 10R, 15, 20R, 25, 30R
 


dfBFINeg <-subset(BFI,select=c( "participant",
                               "BFI01_raw", "BFI21_raw", "BFI26_raw",  # Extraversion  
                               "BFI07_raw", "BFI17_raw", "BFI27_raw",  # Vertr?glichkeit 
                               "BFI03_raw", "BFI08_raw", "BFI28_raw",  # Gewissenhaftigkeit 
                               "BFI14_raw", "BFI19_raw", "BFI24_raw",  # Neurotizismus
                               "BFI10_raw", "BFI20_raw", "BFI30_raw")) # Offenheit

#change the colnames --> replace "raw" with "recoded"
dfBFINegRaw = dfBFINeg
colnames(dfBFINeg) = gsub("raw", "recoded", colnames(dfBFINeg))
#recode the items (i.e. 5-->1, 4 --> 2, 3 --> 3, 2 --> 4, 1-->5):
dfBFINeg[2:16] = abs(dfBFINeg[2:16]-6)

############################################
##get the other 15 items
dfBFIPos <-subset(BFI,select=c( "participant",
                               "BFI06_raw", "BFI11_raw", "BFI16_raw",  # Extraversion  
                               "BFI02_raw", "BFI12_raw", "BFI22_raw",  # Verträglichkeit 
                               "BFI13_raw", "BFI18_raw", "BFI23_raw",  # Gewissenhaftigkeit 
                               "BFI04_raw", "BFI09_raw", "BFI29_raw",  # Neurotizismus
                               "BFI05_raw", "BFI15_raw", "BFI25_raw")) # Offenheit

#we need a novel column name, because we wanna have later raw and changed values within the same dataframe:
#orig stands for "original value" (i.e. not recoded)
colnames(dfBFIPos) = gsub("raw", "orig", colnames(dfBFIPos))
#############################################

##combine both the diasgree and the agree items again:
BFI <-merge(dfBFIPos, dfBFINeg, by = "participant")
remove(dfBFINeg, dfBFIPos, dfBFINegRaw) #cleanup

BFI = BFI[ , order(names(BFI))] ##--> subject is the last column now
#reorder the columns, i.e. bring the "subject" column back to front:
BFI = BFI[c(31,1:30)]


select(BFI, BFI05_orig, BFI10_recoded, BFI15_orig, BFI20_recoded, BFI25_orig, BFI30_recoded)

#Offenheit: 5, 10R, 15, 20R, 25, 30R
BFI$O <- rowMeans(select(BFI, BFI05_orig, BFI10_recoded, BFI15_orig, BFI20_recoded, BFI25_orig, BFI30_recoded))  
#Conscientiousness/Gewissenhaftigkeit: 3R, 8R, 13, 18, 23, 28R
BFI$C <- rowMeans(select(BFI, BFI03_recoded, BFI08_recoded, BFI13_orig, BFI18_orig, BFI23_orig, BFI28_recoded))
#Extraversion: 1R, 6, 11, 16, 21R, 26R
BFI$E <- rowMeans(select(BFI, BFI01_recoded, BFI06_orig, BFI11_orig, BFI16_orig, BFI21_recoded, BFI26_recoded))
#Agreeableness/Verträglcihkeit: 2, 7R, 12, 17R, 22, 27R
BFI$A <- rowMeans(select(BFI, BFI02_orig, BFI07_recoded, BFI12_orig, BFI17_recoded, BFI22_orig, BFI27_recoded))
#Neuroticism/Negative Emotionalität: 4, 9, 14R, 19R, 24R, 29
BFI$N <- rowMeans(select(BFI, BFI04_orig, BFI09_orig, BFI14_recoded, BFI19_recoded, BFI24_recoded, BFI29_orig))


####now,keep only the variables of interest
BFI <- BFI[, c(1, 32:36)]

print(paste0("BFI scores of ", length(unique((BFI$participant))), " participants calculated. Return data frame."))


#------------------------------------------------------------------------------------------------------------------#
#                                                 MSI scores                                                       #
#------------------------------------------------------------------------------------------------------------------#

print("Caluclate MSI scores ....")

names(MSI) <- c("participant", paste0("MSI", c(1:38)))

#fix Q32 und Q36 to original scale
library(dplyr)
MSI <-  mutate(MSI, MSI32 = case_when(
                            MSI32 < 1 ~ 1,
                             MSI32 == 1 ~ 2,
                             MSI32 == 2 ~ 3,
                             MSI32 == 3 ~ 4,
                             MSI32 >= 4 & MSI32 <= 5 ~ 5,
                             MSI32 >= 6 & MSI32 <= 9 ~ 6,
                             MSI32 >= 10 ~ 7)) 
MSI <-  mutate(MSI, MSI36 = case_when(
                            MSI36 < 1 ~ 1,
                             MSI36 == 1 ~ 2,
                             MSI36 == 2 ~ 3,
                             MSI36 == 3 ~ 4,
                             MSI36 >= 4 & MSI36 <= 5 ~ 5,
                             MSI36 >= 6 & MSI36 <= 9 ~ 6,
                             MSI36 >= 10 ~ 7)) 

MSI <- pivot_longer(MSI, names_to =  "item", values_to = "Resp", cols = MSI1:MSI38)


#get information about the MSI items into the dataframe
MSI_items <- read.delim(file= "input/MSI_info.txt", header=FALSE, sep="\t")
MSI_items <- MSI_items[,c(4:8)]
names(MSI_items) <- c("factor", "GeneralME", "Item", "Recode", "No")
MSI_items$item <- c(paste0("MSI", c(1:38)))

MSI <- merge(MSI, MSI_items, by = "item")

rm(MSI_items)

# Recode Response
MSI$RESP <- ifelse(MSI$Recode == "neg", 8- MSI$Resp,   MSI$Resp)

#remove unnecessary variables
MSI <- MSI[, c(2,4,5,9)]

#calculate Sub-Factors

MSI_subs <- MSI %>% group_by(participant, factor) %>% summarise(N = length(RESP), 
                                                                Mean = mean(RESP), 
                                                                Sum = sum(RESP))
#convert to wide format
MSI_subs$N <- NULL
MSI_subs <- pivot_wider(MSI_subs, names_from = "factor", values_from = c(Mean, Sum), names_sep = "_")

MSI_general <- MSI %>% filter(GeneralME =="ME") %>% group_by(participant) %>% summarise(N = length(RESP), 
                                                                                        ME_mean = mean(RESP), 
                                                                                        ME_sum = sum(RESP))
MSI_general$N <- NULL

MSI <- merge(MSI_subs, MSI_general, by = "participant")

rm(MSI_subs, MSI_general)


#------------------------------------------------------------------------------------------------------------------#
#                       Merge all datasets which were preprocessed so far together                                 #
#------------------------------------------------------------------------------------------------------------------#

D <- merge(Sample, AQ)
rm(AQ, Sample)
D <- merge(D, BFI)
rm(BFI)
D <- merge(D, MSI)
rm(MSI)
D <- merge(D, Music_Quali)
rm(Music_Quali)   
D <- merge(D, MYears)
rm(MYears)



#------------------------------------------------------------------------------------------------------------------#
#                                                 After_Exp                                                       #
#------------------------------------------------------------------------------------------------------------------#

survey <- D[,c(1,3, 4:14, 2,  15:51)]

survey <- merge(survey, After_Exp)

rm(After_Exp)

#------------------------------------------------------------------------------------------------------------------#
#                                                   PANAS                                                          #
#------------------------------------------------------------------------------------------------------------------#

PA_s <- panas[, (1+c(0, 1,3,4,6,10,11, 13,15, 17,18))] # items 1,3,4,6,10,11, 13,15, 17,18

NA_s <- panas[, (1+c(0, 2,5,7,8,9, 12, 14,16, 19, 20))] # items 2,5,7,8,9, 12, 14,16, 19, 20

PA_s$pos_Aff <- apply(PA_s[, 2:11], 1, mean) # calculate mean positive affect
NA_s$neg_Aff <- apply(NA_s[,2:11], 1, mean) # calculate mean negative affect

#add it to the survey
survey <- merge(survey, PA_s[,c(1,12)])
survey <- merge(survey, NA_s[,c(1,12)])

rm(PA_s, NA_s, panas)

#------------------------------------------------------------------------------------------------------------------#
#                                                 SES                                                              #
#------------------------------------------------------------------------------------------------------------------#

names(SES) <- c("participant", "Schulabschluss", "Ausbildung", "Einkommen", "Land_Kindheit", "Land_jetzt")

SES$Schulabschluss <- recode(SES$Schulabschluss, `1` = "Schüler", `2` = "ohne", 
                             `3` = "Hauptschule", `4` = "Mittelschule", 
                             `5` = "Fachschule", `6` = "Abitur")

SES$Schulabschluss <- factor(SES$Schulabschluss, levels = c("Schüler", "ohne", "Hauptschule","Mittelschule", "Fachschule","Abitur"))


SES$Ausbildung <- recode(SES$Ausbildung, `1` = "inAusbildung", `2` = "Schüler", 
                             `3` = "nochKeine", `4` = "Lehre", 
                             `5` = "Fachschule", `6` = "Meister", 
                             `7` = "Bachelor", `8` = "Fachhochschulabschluss",
                             `9` = "Master/Diplom", `10` = "Promotion")
SES$Ausbildung <- factor(SES$Ausbildung, levels = c("inAusbildung","Schüler", 
                                                    "nochKeine", "Lehre", 
                                                    "Fachschule", "Meister", 
                                                    "Bachelor",  "Fachhochschulabschluss",
                                                    "Master/Diplom", "Promotion"))


SES$Einkommen <- recode(SES$Einkommen, `1` = "<1750", `2` = "1750-2500", 
                        `3` = "2500-3500", `4` = "3500-5000", 
                        `5` = ">5000")
SES$Einkommen <- factor(SES$Einkommen, levels = c("<1750","1750-2500", 
                                                  "2500-3500","3500-5000", 
                                                  ">5000"))

survey <- merge(survey, SES[,1:4])

rm(SES)



#------------------------------------------------------------------------------------------------------------------#
#                                                 Fix                                                              #
#------------------------------------------------------------------------------------------------------------------#


#Fix some names of Variables
survey <- survey %>% rename(YearsTraining = Question32_1,
                            Instrument= Question39_1,
                            AgeTraining = Question40_1)

save(survey, file="input/amateurs_survey_preprocessed.RData")

rm(D, Raw)





#------------------------------------------------------------------------------------------------------------------#
#                                Process survey data for further Analyses                                                              #
#------------------------------------------------------------------------------------------------------------------#

# fix some columns (Age,YearsTraining,AgeTraining) due to spelling changes further up
survey <- survey %>%  mutate(Age = as.numeric(Age))
survey <- survey %>%  mutate(YearsTraining = as.numeric(YearsTraining))
survey <- survey %>%  mutate(AgeTraining = as.numeric(AgeTraining))

# code new variable to standardise Instrument names
survey$Instrument2 <- survey$Instrument
survey$Instrument2[survey$Instrument2 == "Geige"] <- "Violine"
survey$Instrument2[survey$Instrument2 == "Chor"] <- "Gesang"
survey$Instrument2[survey$Instrument2 == "gesang"] <- "Gesang"
survey$Instrument2[survey$Instrument2 == "Gesang, Blockflöte, Saxophon"] <- "Gesang"
survey$Instrument2[survey$Instrument2 == "Ich spiele seit 13 Jahren Klavier und singe im Chor und kann nicht sagen was ich besser kann"] <- "Gesang"
survey$Instrument2[survey$Instrument2 == "Cello, Gesang"] <- "Cello"
survey$Instrument2[survey$Instrument2 == "Trompete (Gesang fast gleichermaßen)"] <- "Trompete"

# remove participants who omitted >5% of trials
survey <- subset(survey, Code!="SCS58E") # singer, ecd4a2cb
survey <- subset(survey, Code!="EHM92A") # singer, c7e7e936

# remove participants who reported faulty audio-files
survey <- subset(survey, Code!="AIG27E") # instrumentalist
survey <- subset(survey, Code!="IHP42U") # singer
# participant SCS58E also reported faulty audio-files, but is already removed because of omitting >5% of trials

# remove participant who sings in choir and an instrument in a band
survey <- subset(survey, Code!="NIR46E") 

paste("There are currently", length(unique(survey$Code)), "datasets loaded.")

# correct Group assignment for one participant (AEM51L)
survey$Group[survey$Code == "AEM51L"] <- 1

# process amateurs down to relevant variables
survey <- survey[,c(1:7,46,47,14,16:18,20, 22:28,29:33,34:38,44,48:51,63,64,65:67,68)]

save(survey, file="input/amateurs_survey_processed.RData")


#End of Script