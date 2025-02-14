####################################################################################
# Author: Verena Skuk, adjusted by Christine Nussbaum
# 
####################################################################################

library(stringr)  # für datenaufbereitung
library(readxl)

loadPTKSurveyData <- function(relPathDirFile, REVERSEITEMS = FALSE) {
  
  relPathDirFile = "input/survey/data.csv"
  
  
    Raw <- read.csv(relPathDirFile, header= TRUE, sep = ",", row.names = NULL)
    cols <- names(Raw)
    names(Raw) <-cols[2:182]
    Raw <- Raw[,1:181]
    
    #remove all incompleted datasets (they dont have time_end)
    Raw <- Raw %>% filter(!is.na(Raw$TIME_total))
    
    Raw$participant <- str_sub(Raw$participant , 31, 38)
    
    Raw$Partcode <- paste0(Raw$LPartCode.1, Raw$LPartCode.2, Raw$LPartCode.3, Raw$LPartCode.4, Raw$LPartCode.5, Raw$LPartCode.6)
    
    Raw <- Raw[ , -c(172:177)]
    
    nSubs <- length(unique(Raw$participant))
    print(paste("loaded data from", nSubs, "participants", sep=" "))
    
    #isolate different questionnaires:
    Sample <- Raw[, c(1, 3:9, 172, 175, 176)]
    After_Exp <- Raw[, c(1,11:21)]
    AQ <- Raw[, c(1, 23:72)]
    BFI <- Raw[, c(1, 73:102)]
    MSI <- Raw[, c(1, 103:140)]
    Music_Quali <- Raw[, c(1, 141:146)]
    SES <-Raw[, c(1, 147:151)]
    panas <-  Raw[, c(1, 152:171)]
    
    #------------------------------------------------------------------------------------------------------------------#
    #                                          Setting up sample information                                           #
    #------------------------------------------------------------------------------------------------------------------# 
    
    names(Sample) <- c("participant", "Age", "LSex", "LMotherLanguage", "LProfession","LHearingImp",  "LHearingImp2", "LHearingImp2", "Comments", "TimeTotal", "Code")              
              
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
    # Definitely disagreeâ (4) or slightly disagree (3) responses will score 1 point
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
    ##the Hoekstra score is te same as the sum of all 4 scores of Baron-cohen
    AQ$AQ_Social                   <- rowSums(subset(AQ, select = c("AQ_AttentionSwitching", "AQ_Communication", "AQ_Imagination", "AQ_SocialSkills")), na.rm = TRUE)
    AQ$AQ_Total = AQ$AQ_AttentionToDetails + AQ$AQ_Social
    
    ####keep only the Hoechstra Information
    AQ <- AQ[, c(1, 56:58)]
    
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
# Mit dem BFI-2 können Skalenwerte für die DomÃ¤nen Extraversion, VertrÃ¤glichkeit, Gewissenhaftigkeit,
# Negative EmotionalitÃ¤t und Offenheit gebildet werden. Zusätzlich können Skalenwerte für insgesamt 15
# Facetten gebildet werden. Jeweils drei Facetten lassen sich inhaltlich einer Dimension zuordnen. Dazu werden
# die Antwortkategorien von 1 (stimme überhaupt nicht zu) bis 5 (stimme voll und ganz zu) kodiert und
# anschließend aggregiert. Negativ kodierte Items müssen zuvor rekodiert werden (rekodierter Wert = 6 -Rohwert).
# Extraversion: 1R, 6, 11, 16, 21R, 26R
# VertrÃ¤glichkeit: 2, 7R, 12, 17R, 22, 27R
# Gewissenhaftigkeit: 3R, 8R, 13, 18, 23, 28R
# Negative EmotionalitÃ¤t: 4, 9, 14R, 19R, 24R, 29
# Offenheit: 5, 10R, 15, 20R, 25, 30R
 


dfBFINeg <-subset(BFI,select=c( "participant",
                               "BFI01_raw", "BFI21_raw", "BFI26_raw",  # Extraversion  
                               "BFI07_raw", "BFI17_raw", "BFI27_raw",  # VertrÃ¤glichkeit 
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
                               "BFI02_raw", "BFI12_raw", "BFI22_raw",  # VertrÃ¤glichkeit 
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


#Offenheit: 5, 10R, 15, 20R, 25, 30R
BFI$O <- rowSums(select(BFI, BFI05_orig, BFI10_recoded, BFI15_orig, BFI20_recoded, BFI25_orig, BFI30_recoded))  
#Conscientiousness/Gewissenhaftigkeit: 3R, 8R, 13, 18, 23, 28R
BFI$C <- rowSums(select(BFI, BFI03_recoded, BFI08_recoded, BFI13_orig, BFI18_orig, BFI23_orig, BFI28_recoded))
#Extraversion: 1R, 6, 11, 16, 21R, 26R
BFI$E <- rowSums(select(BFI, BFI01_recoded, BFI06_orig, BFI11_orig, BFI16_orig, BFI21_recoded, BFI26_recoded))
#Agreeableness/VertrÃ¤glcihkeit: 2, 7R, 12, 17R, 22, 27R
BFI$A <- rowSums(select(BFI, BFI02_orig, BFI07_recoded, BFI12_orig, BFI17_recoded, BFI22_orig, BFI27_recoded))
#Neuroticism/Negative EmotionalitÃ¤t: 4, 9, 14R, 19R, 24R, 29
BFI$N <- rowSums(select(BFI, BFI04_orig, BFI09_orig, BFI14_recoded, BFI19_recoded, BFI24_recoded, BFI29_orig))


####now,keep only the variables of interest
BFI <- BFI[, c(1, 32:36)]

print(paste0("BFI scores of ", length(unique((BFI$participant))), " participants calculated. Return data frame."))


#------------------------------------------------------------------------------------------------------------------#
#                                                 MSI scores                                                       #
#------------------------------------------------------------------------------------------------------------------#

print("Caluclate MSI scores ....")

names(MSI) <- c("participant", paste0("MSI", c(1:38)))
MSI <- pivot_longer(MSI, names_to =  "item", values_to = "Resp", cols = MSI1:MSI38)


#get information about the MSI items into the dataframe
setwd("~/Christine/Arbeit/01_Promotion/03_Experiment2/exp_online/analysis")
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
MSI_general <- NULL

MSI <- merge(MSI_subs, MSI_general, by = "participant")

rm(MSI_subs, MSI_general)
#############################################################################################


    
    return(list(dfDemoEnd, dfPostExp, dfAQ))
}

