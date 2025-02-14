####################################################################################
# Author: Verena Skuk
# File: DFG-Personality\04_Experiments\E1_analysis\functions\loadPTKExperimentDataEmpraSoSe.R
# 
####################################################################################
library(stringr)  # für datenaufbereitung

loadPTKExperimentData <- function(relDirPath) {
  
  relDirPath = "input/emo_exp/"
    file_list <- list.files(relDirPath)   # get list with all filenames

    rawDF <- data.frame()               # create empty dataset
    
    #read all the files, give it a SubjectCode and paste it together
    for (file in file_list){
  
      filename = paste0(relDirPath,file)
      printstr = paste0("load ", filename)
      print(printstr)
      expName     = str_sub(file, 1, 19)
      date        = str_sub(file, 16, 25)
      SubjectCode = str_sub(file, 65, 72)
      temp_data<-read.delim(filename, header=FALSE, sep=" ", stringsAsFactors=FALSE, fill = TRUE)
      temp_data$Experiment = expName
      temp_data$Date = date
      temp_data$Subject <- SubjectCode
      rawDF<-rbind(rawDF, temp_data)
      rm(temp_data)
    }
    
    # tidy up
    rm(file, file_list, filename, relDirPath, SubjectCode)
        
    # check SubjectCodes
    unique(rawDF$Subject)
    return(rawDF)
}


