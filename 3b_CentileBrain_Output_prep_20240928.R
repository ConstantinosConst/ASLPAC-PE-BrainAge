###### Project: Exploring associations between psychotic experiences and structural brain age: a population-based study in late adolescence ######
###### Script 3b: CentileBrain brain age model performance check and dataset preparation for downstream analyses - step 3b ######
###### Description: Generates performance metrics/plots for the current sample and prepare ouput for downstream analyses ######
###### Written by: Constantinos Constantinides ######
###### Last edited: 28/09/2024 ######


# Set working directory to file location
# setwd ("...")


# install/load libraries
cat("Prep: installing/loading libraries\n")

load.lib <- function(x){
  for( i in x ){
    if( ! require( i , character.only = TRUE ) ){
      install.packages( i , dependencies = TRUE )
      #require( i , character.only = TRUE )
      library(i)
    }
  }
}

#  Then try/install packages...
load.lib( c("ppcor" , "lsmeans" , "multcomp","data.table","plyr","ModelMetrics",
            "caret","gridExtra","Hmisc","pastecs","psych","ggplot2", "dplyr","smplot2", "sensemakr", "rJava", "xlsx") )


# Source function
source("CB.model.fits.brainAge.R")
source("CB.model.fits.brainAge.PE.R")

# Load ALSPAC-PE imaging dataset (as prepared in script 1)
data <- read.csv("ALSPAC_PE_Imaging.csv", header=TRUE)

# Isolate covariates (non-imaging variables) to merge later with model output
## Non-imaging variables
Covariates <- data[,c("SubjID", "PE", "Age", "Sex",                                 
                      "PE_3Level", "PE_4Level_NASF", "PE_Sum_NASF", "PsyDis",                              
                      "PE_age_12", "BW", "ME", "mSC", "pSC", "IQ_8",        
                      "CorticalQC", "SubCorticalQC" 
)]
### Check for duplicated columns
duplicated(Covariates) 
rm(data)

# Load CB model input files and merge with covariates (as prepared in script 2)
# Males
males <- read.xlsx("CB_males_raw.xlsx", 1)
names(males)[names(males) == "SubjectID"] <- "SubjID"
males <- merge(males, Covariates, by='SubjID')
# females
females <- read.xlsx("CB_females_raw.xlsx", 1)
names(females)[names(females) == "SubjectID"] <- "SubjID"
females <- merge(females, Covariates, by='SubjID')

rm (Covariates)
cat("Step 3: performance metrics for the current sample")

#Create log file for performance metrics
sink("CentileBrain_Performance_Metrics.log", type="output")

model.fits.brainAge(males,"CB_males_raw_out.csv")
model.fits.brainAge(females,"CB_females_raw_out.csv")

model.fits.brainAge.PE(males,"CB_males_raw_out.csv")
model.fits.brainAge.PE(females,"CB_females_raw_out.csv")

sink()

# check that 4 pdf files and 1 log file with performance metrics/plots have been generated in the working directory. 

# Read-in CB output files to get brain-predicted age
# males 
output <- read.csv("CB_males_raw_out.csv", header=T)
males$predAge_CB <- output$x
rm(output)

# females
output <- read.csv("CB_females_raw_out.csv", header=T)
females$predAge_CB <- output$x
rm(output)

BA=rbind(males,females)

# check that no sample duplication
cat("duplicated IDs: ",which(duplicated(BA$SubjID)),"\n")

# Isolate and export covariates and brain-predicted age for downstream statistical analyses
data <- BA %>% select(c("SubjID", "PE", "Age", "Sex",                                 
                      "PE_3Level", "PE_4Level_NASF", "PE_Sum_NASF", "PsyDis",                              
                      "PE_age_12", "BW", "ME", "mSC", "pSC", "IQ_8",        
                      "CorticalQC", "SubCorticalQC", "predAge_CB"))

# Read-in and merge with excluded participants due to missing IDP
load("ALSPAC_PE_IDP_Missing.RData")
ALSPAC_PE_IDP_Missing$predAge_CB <- NA
data <- rbind(data, ALSPAC_PE_IDP_Missing)
# Export dataset for downstream stastical analyses
save(data,file="ALSPAC_PE_BA_CB.RData")

# Isolate IDP and essential covariates for feature importance analysis later on
data_FI <- BA %>% select(-c("SITE", "age", "sex", "ScannerType", "FreeSurfer_Version", "PE_3Level", "PE_4Level_NASF",
                         "PE_Sum_NASF", "PsyDis", "PE_age_12" , "BW", "ME","mSC", "pSC", "IQ_8"))
## re-order
## colnames(data)
data_FI <- data_FI[,c(1,152:156,2:151, 157)]

# Export dataset for downstream analyses of feature importance
save(data,file="ALSPAC_PE_BA_CB_FI.RData")

cat('finished with step 3b')

#Done
# Procceed to step 4a
