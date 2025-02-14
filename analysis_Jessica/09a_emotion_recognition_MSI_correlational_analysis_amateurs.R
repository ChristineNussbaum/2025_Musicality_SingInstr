##########################################################################
## File: 09a_emotion_recognition_MSI_correlational_analysis_amateurs.R
## This script runs some correlational analyses on the emotion recognition data with the Gold-MSI data
# author: Christine Nussbaum (christine.nussbaum@uni-jena.de)
# date 07/2023

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
#get the preprocessed data: 

#Experiment data
load(file="input/amateurs_Exp_processed.RData")

# remove participant with degree in music science
D2 <- subset(D2, Code!="AEB56L") # instrumentalist

#survey data including the Gold-MSI
load(file="input/amateurs_survey_processed.RData")

# remove participant with degree in music science
survey2 <- subset(survey2, Code!="AEB56L") # instrumentalist


# rename data set to make script easier to read
D <- D2
survey <- survey2
rm(D2,survey2)

#---------------------------------------------------------------------------------#
#                 Prepare Datasets for Correlation Analyses                       #
#---------------------------------------------------------------------------------#

## Gold-MSI
MSI <- survey[, c(1, 31, 26:30 )]
rm(survey)

## Emotion recognition data

# remove avg trials
B <- D %>% filter(Emo != "avg")

# average data

B <- B %>% group_by(Code, Group, MType) %>% summarise(ACC = mean(ACC), 
                                                      RT = mean(RT),
                                                      N = length(Emo))

B$N <- NULL # that was just included to check if averaging worked properly

# convert to wide format

B <- B %>% pivot_wider(names_from = c(MType), values_from = c(ACC, RT))

B$ACC_all <- (B$ACC_f0 + B$ACC_full + B$ACC_tbr)/3
B$RT_all <- (B$RT_f0 + B$RT_full + B$RT_tbr)/3

# merge the datasets
Corrdata <- merge(B, MSI)

#save for later analysis: 
save(Corrdata, file="input/amateurs_Corrdata_MSI.RData")

#---------------------------------------------------------------------------------#
#              1. Correlational Analyses - uncontrolled                           #
#---------------------------------------------------------------------------------#

Correlations_ACC <- rcorr(as.matrix(Corrdata[,c(9, 3:5, 11:16)]), type="spearman") # correlate with ACC 

Correlations_RT <- rcorr(as.matrix(Corrdata[,c(10, 6:8, 11:16)]), type="spearman") # correlate with RT


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

### prepare RT data as well: 
Cor_r <- data.frame(Correlations_RT$r[1:4, 5:10])
Cor_p <- data.frame(Correlations_RT$P[1:4, 5:10])
Cor_n <- Correlations_RT$n[1:1]

Cor_r$MType <- rownames(Cor_r)
Cor_p$MType <- rownames(Cor_p)

#convert to long format
Cor_r = Cor_r %>% pivot_longer(cols = ME_mean:Mean_singing, names_to = "Test", values_to = "r")
Cor_p = Cor_p %>% pivot_longer(cols = ME_mean:Mean_singing, names_to = "Test", values_to = "p")

Cor_RT <-merge(Cor_r, Cor_p)
Cor_RT$N <- Cor_n

rm(Cor_r, Cor_p, Cor_n)

# p-values dont survive correction

# tidy dataset
Cor_RT$MType <- recode(Cor_RT$MType, RT_all = "Averaged", RT_full = "Full", RT_f0 = "F0", RT_tbr = "Timbre")
Cor_RT$MType <- factor(Cor_RT$MType, levels = c("Averaged", "Full", "F0", "Timbre"))

Cor$Test <- recode(Cor$Test, ME_mean = "General ME", Mean_active = "Active", Mean_perception = "Perception", Mean_singing = "Singing", Mean_emotion = "Emotion", Mean_education = "Education")
Cor$Test <- factor(Cor$Test, levels = c("General ME", "Active", "Perception", "Singing", "Emotion", "Education"))

# save the dataset
capture.output(as.matrix(Cor_RT), file="output/emotion_classification/amateurs_C_Correlations_RT_MSI.txt")

rm(Cor_RT,Correlations_ACC, Correlations_RT)


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

filename = paste0("plots/amateurs_05_correlations_ACC_MSI.png")

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

rm(Cor, B, Data, p)

#-----------------------------------------------------------------------------------------------------------------#
#                            Correlations split for Singers and Instrumentalists                                   #
#-----------------------------------------------------------------------------------------------------------------#


#################
# Singers only 
#################

S <- Corrdata %>% filter(Group == "1")

Correlations_ACC <- rcorr(as.matrix(S[,c(6, 3:5,7:12)]), type="spearman")

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

#perform the multiple comparison correction: 
#https://www.youtube.com/watch?v=rZKa4tW2NKs
FDR = 0.05  # set false discovery rate to 5%
Cor <- Cor %>% arrange(p) #order the dataset by p-value
Cor$i <- c(1:24) # assign rank of p-value
m <- 24  # assing number of tests 
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

Cor$Test <- recode(Cor$Test, ME_mean = "General ME", Mean_active = "Active", Mean_perception = "Perception", Mean_singing = "Singing", Mean_emotion = "Emotion", Mean_education = "Education")
Cor$Test <- factor(Cor$Test, levels = c("General ME", "Active", "Perception", "Singing", "Emotion", "Education"))

# save the dataset
capture.output(as.matrix(Cor), file="output/emotion_classification/amateurs_C_Correlations_ACC_MSI_Singers_only.txt")


############################################
# plot a 4 x 6 matrix with all the correlations and Scatterplots

# prepare dataset


Emo <- S[, c(1:6)] %>% pivot_longer(cols = ACC_f0:ACC_all, names_to = "MType", values_to = "ACC")
MSI <- S[, c(1,2,7:12)] %>% pivot_longer(cols = ME_mean:Mean_singing, names_to = "Test", values_to = "Score")

Data <- merge(Emo, MSI)

rm(Emo, MSI)

# recode factors
Data$MType <- recode(Data$MType, ACC_all = "Averaged", ACC_full = "Full", ACC_f0 = "F0", ACC_tbr = "Timbre")
Data$MType <- factor(Data$MType, levels = c("Averaged", "Full", "F0", "Timbre"))

Data$Test <- recode(Data$Test, ME_mean = "General ME", Mean_active = "Active", Mean_perception = "Perception", Mean_singing = "Singing", Mean_emotion = "Emotion", Mean_education = "Education")
Data$Test <- factor(Data$Test, levels = c("General ME", "Active", "Perception", "Singing", "Emotion", "Education"))

Data$Group <- factor(Data$Group, levels = c("1", "2"))

# format Cor

Cor$p_corrected2 <- round(Cor$p_corrected2, 3)
Cor$pplot <- paste0("= ", Cor$p_corrected2)
Cor$pplot <- ifelse(Cor$pplot == "= 0", "< 0.001", Cor$pplot)

Cor$sig <- ifelse(Cor$p_corrected2 < 0.05, "bold.italic", "plain")



title = paste0("Correlation between Emotion Classification Performance and Gold-MSI, Singers only (N = 45)")

filename = paste0("plots/amateurs_05b_correlations_ACC_MSI_Singers_only.png")

p<-(ggplot(data= Data, aes(x = Score, y=ACC)) +
      geom_point(shape= 18, size = 3, color = "green3") +
      labs(y = "Emotion Classification Performance" , x = "Gold-MSI (Sumscore)" , title = title, color = "") +       #, title = title
      facet_grid(rows = vars(MType), cols = vars(Test), switch = "y", ) + #scales = "free_x"
      geom_smooth(method='lm', formula= y~x, color = "black") + 
      geom_text(data= Cor, 
                aes(label= paste0("r = ", round(r, 2), "\n p ", pplot), x = 2, y = 0.85, fontface = sig), color = "black") +
      scale_y_continuous(position = "right") + 
      scale_x_continuous(limits=c(1, 7), breaks = c( 1, 2, 3, 4, 5, 6)) + 
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

rm(S, Data, p, Correlations_ACC, Cor)

########################
# Instrumentalists only 
#######################

I <- Corrdata %>% filter(Group == "2")

Correlations_ACC <- rcorr(as.matrix(I[,c(6, 3:5,7:12)]), type="spearman")

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

#none of the p-values are significant / survive correction

# tidy dataset
Cor$MType <- recode(Cor$MType, ACC_all = "Averaged", ACC_full = "Full", ACC_f0 = "F0", ACC_tbr = "Timbre")
Cor$MType <- factor(Cor$MType, levels = c("Averaged", "Full", "F0", "Timbre"))

Cor$Test <- recode(Cor$Test, ME_mean = "General ME", Mean_active = "Active", Mean_perception = "Perception", Mean_singing = "Singing", Mean_emotion = "Emotion", Mean_education = "Education")
Cor$Test <- factor(Cor$Test, levels = c("General ME", "Active", "Perception", "Singing", "Emotion", "Education"))

# save the dataset
capture.output(as.matrix(Cor), file="output/emotion_classification/amateurs_C_Correlations_ACC_MSI_Instrumentalists_only.txt")


############################################
# plot a 4 x 6 matrix with all the correlations and Scatterplots

# prepare dataset
Emo <- I[, c(1:6)] %>% pivot_longer(cols = ACC_f0:ACC_all, names_to = "MType", values_to = "ACC")
MSI <- I[, c(1,2,7:12)] %>% pivot_longer(cols = ME_mean:Mean_singing, names_to = "Test", values_to = "Score")

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



title = paste0("Correlation between Emotion Classification Performance and Gold-MSI, Instrumentalists only (N = 43)")

filename = paste0("plots/amateurs_05c_correlations_ACC_MSI_Instrumentalists_only.png")

p<-(ggplot(data= Data, aes(x = Score, y=ACC)) +
      geom_point(shape= 18, size = 3, color = "darkgreen") +
      labs(y = "Emotion Classification Performance" , x = "Gold-MSI (Summenscore)" , title = title, color = "") +       #, title = title
      facet_grid(rows = vars(MType), cols = vars(Test), switch = "y", ) + #scales = "free_x"
      geom_smooth(method='lm', formula= y~x, color = "black") + 
      geom_text(data= Cor, 
                aes(label= paste0("r = ", round(r, 2), "\n p ", pplot), x =  2, y = 0.85, fontface = sig), color = "black") +
      scale_y_continuous(position = "right") + 
      scale_x_continuous(limits=c(1, 7), breaks = c( 1, 2, 3, 4, 5, 6)) + 
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

rm(I, Data, p, Correlations_ACC, Cor)

#tidy environment completely
rm(list=ls())


#---------------------------------------------------------------------------------#
#       2. Correlational Analyses - controlled for formal musical education       #
#---------------------------------------------------------------------------------#

#reload all data
load(file="input/amateurs_Corrdata_PROMS.RData")
PROMS <- Corrdata
load(file="input/amateurs_Corrdata_MSI.RData")
MSI <- Corrdata
load(file="input/amateurs_Corrdata_OCEAN_AQ.RData")
OCEAN <- Corrdata

#merge the data

Corrdata <- merge(PROMS, MSI)
Corrdata <- merge(Corrdata, OCEAN)

rm(MSI, PROMS, OCEAN)

#remove the Variables we are not interested in: 

Corrdata <- Corrdata[,-c(6:8, 10)]


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

rm(Cor)
#---------------------------------------------------------------------------------#
#             3. Logistic regression - uncontrolled                               #
#---------------------------------------------------------------------------------#
#I used these resources: https://www.r-bloggers.com/2021/02/how-to-run-logistic-regression-on-aggregate-data-in-r/

Corrdata$n <- 24

Comb <- expand.grid(c("ACC_all", "ACC_full", "ACC_f0", "ACC_tbr"),c("ME_mean", "Mean_active", "Mean_education",
                                                                    "Mean_emotion", "Mean_perception", "Mean_singing"))

log_results <- Comb # create "empty" results data frame
log_results$coeff <- NA
log_results$p <- NA
log_results$N <- 88

#run logistic regression:

for(i in 1:length(Comb$Var1)) {
  mod <- as.formula(paste(Comb$Var1[i], "~", Comb$Var2[i]))
  m<-glm(formula = mod, data=Corrdata, weights = n, family = quasibinomial("logit"))
  sum_m <- summary(m)
  log_results$coeff[i] <-  m$coefficients[2]
  log_results$p[i] <- sum_m$coefficients[2,4]
}


Log <- log_results
names(Log) <- c("MType", "Test", "Log", "p", "N")
rm(Comb, log_results, m, sum_m)


# tidy dataset
Log$MType <- recode(Log$MType, ACC_all = "Averaged", ACC_full = "Full", ACC_f0 = "F0", ACC_tbr = "Timbre")
Log$MType <- factor(Log$MType, levels = c("Averaged", "Full", "F0", "Timbre"))

Log$Test <- recode(Log$Test, ME_mean = "General ME", Mean_active = "Active", Mean_education = "Education", Mean_perception = "Perception", Mean_singing = "Singing", Mean_emotion = "Emotion")
Log$Test <- factor(Log$Test, levels = c("General ME", "Active", "Education", "Perception", "Singing", "Emotion"))

#perform the multiple comparison Correction: 
#https://www.youtube.com/watch?v=rZKa4tW2NKs
FDR = 0.05  # set false discovery rate to 5%
Log <- Log %>% arrange(p) #order the dataset by p-value
Log$i <- c(1:24) # assign rank of p-value
m <- 24  # assing number of tests 
Log$CritValue <- (Log$i / m) * FDR  #(i/m)*Q  # Logrect the critical value
Log$p_corrected <- Log$p * (m / Log$i)        # Logrect the p-value itself

#perform adjustement of the corrected value: 
Log$p_corrected2 <- Log$p_corrected 

for (j in (m-1):1){
  Log$p_corrected2[j] <- ifelse(Log$p_corrected[j] > Log$p_corrected2[j+1], Log$p_corrected2[j+1], Log$p_corrected[j] )
}

# save the dataset
capture.output(as.matrix(Log), file="output/emotion_classification/amateurs_C_LogisticReg_ACC_MSI.txt")


#---------------------------------------------------------------------------------#
#       4. Logistic regression - controlled for formal musical education          #
#---------------------------------------------------------------------------------#

Corrdata$n <- 24

Comb <- expand.grid(c("ACC_all", "ACC_full", "ACC_f0", "ACC_tbr"),c("ME_mean", "Mean_active", 
                                                                    "Mean_emotion", "Mean_perception", "Mean_singing"))

log_results <- Comb # create "empty" results data frame
log_results$coeff <- NA
log_results$p <- NA
log_results$N <- 88

# logistic regression with an effect estimated for musical education

for(i in 1:length(Comb$Var1)) {
  mod <- as.formula(paste(Comb$Var1[i], "~", Comb$Var2[i], "+ Mean_education"))
  m<-glm(formula = mod, data=Corrdata, weights = n, family = quasibinomial("logit"))
  sum_m <- summary(m)
  log_results$coeff[i] <-  m$coefficients[2]
  log_results$p[i] <- sum_m$coefficients[2,4]
}


Log <- log_results
names(Log) <- c("MType", "Test", "Log", "p", "N")
rm(Comb, log_results, m, sum_m)



# tidy dataset
Log$MType <- recode(Log$MType, ACC_all = "Averaged", ACC_full = "Full", ACC_f0 = "F0", ACC_tbr = "Timbre")
Log$MType <- factor(Log$MType, levels = c("Averaged", "Full", "F0", "Timbre"))

Log$Test <- recode(Log$Test, ME_mean = "General ME", Mean_active = "Active",  Mean_perception = "Perception", Mean_singing = "Singing", Mean_emotion = "Emotion")
Log$Test <- factor(Log$Test, levels = c("General ME", "Active", "Perception", "Singing", "Emotion"))

#perform the multiple comparison Logrection: 
#https://www.youtube.com/watch?v=rZKa4tW2NKs
FDR = 0.05  # set false discovery rate to 5%
Log <- Log %>% arrange(p) #order the dataset by p-value
Log$i <- c(1:20) # assign rank of p-value
m <- 20  # assing number of tests 
Log$CritValue <- (Log$i / m) * FDR  #(i/m)*Q  # Logrect the critical value
Log$p_corrected <- Log$p * (m / Log$i)        # Logrect the p-value itself

#perform adjustement of the corrected value: 
Log$p_corrected2 <- Log$p_corrected 

for (j in (m-1):1){
  Log$p_corrected2[j] <- ifelse(Log$p_corrected[j] > Log$p_corrected2[j+1], Log$p_corrected2[j+1], Log$p_corrected[j] )
}

# save the dataset
capture.output(as.matrix(Log), file="output/emotion_classification/amateurs_C_LogisticReg_ACC_MSI_c-Education.txt")


### End of Script
