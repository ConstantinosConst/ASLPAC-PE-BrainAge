# Project: Exploring associations between psychotic experiences and structural brain age: a population-based study in late adolescence [WORKING PAPER]
# Script: 4a. Data transformation and cleaning for data visualization and statistical analyses - step 4a
# Description: Re-codes/derives (new) variables and remove observation with failed image QC for downstream analyses
# Script author: Constantinos Constaninides
# Written/last update on: 28/09/2024

# Load relevant packages
# library (ggplot2)
library(dplyr)
# library(pastecs)
# library(psych)
# library(sensemakr)
# library(olsrr)
# library(smplot2)
# library(effectsize)
library(gtsummary)
# library(tidyverse)

options(max.print=999999)

# setwd to working directory

# Load ALSPAC-PE dataset with ENIGMA-predicted brain age
load('ALSPAC_PE_BA_ENG.RData')
ALSPAC_PE_BA_ENG <- data
rm(data)
# keep SubjID and predicted age only
ALSPAC_PE_BA_ENG <- ALSPAC_PE_BA_ENG[,c("SubjID", "predAge_ENG")]

# Load ALSPAC-PE dataset with CentileBrain-predicted brain age
load("ALSPAC_PE_BA_CB.RData")
ALSPAC_PE_BA_CB <- data
rm(data)

# Merge CentileBrain- and ENIGMA-predicted brain age (plus covariates)
data <- merge(ALSPAC_PE_BA_CB, ALSPAC_PE_BA_ENG, by = "SubjID")

#Inspect dataset
## View(data)
str(data)
## remove SPSS labels from column names
head(data)
summary(data)
colnames(data)
## [1] "SubjID"         "PE"             "Age"            "Sex"           
## [5] "PE_3Level"      "PE_4Level_NASF" "PE_Sum_NASF"    "PsyDis"        
## [9] "PE_age_12"      "BW"             "ME"             "mSC"           
## [13] "pSC"            "IQ_8"           "CorticalQC"     "SubCorticalQC" 
## [17] "predAge_CB"     "predAge_ENG"   


# Step 1. Fix and derive variables

# Set as categorical 
## Sex at birth 
data$Sex <- as.factor(data$Sex)
## PEs at age 18
data$PE <- factor(data$PE)
## Meeting criteria for clinical/psychotic disorder
data$PsyDis <- as.factor(data$PsyDis)
## Number of PEs per participants
data$PE_Sum_NASF <- as.numeric(data$PE_Sum_NASF)
## Image QC variables
data$CorticalQC <- as.factor(data$CorticalQC)
data$SubCorticalQC <- as.factor(data$SubCorticalQC)


## Derive Brain-PAD (outcome), plus absolute errors

### CentileBrain-derived brain-PAD
data$devAge_CB <- (data$predAge_CB - data$Age)
### Absolute error of CentileBrain brain-age model, Version 2
data$AE_CB <- abs(data$predAge_CB - data$Age)

### ENIGMA_derived brain-PAD
data$devAge_ENG <- (data$predAge_ENG - data$Age)
### Absolute error of ENIGMA brain-age model (for model performance)
data$AE_ENG <- abs(data$predAge_ENG - data$Age)


## Derive new predictors/covariates

### Convert 3-level PE ordinal variable to a 4-level variable regardless of sleep/fever 
### (0=none, 1 = suspected, 2 = definite, 3 = definite, clinical)
data$PE_3Level[data$PE_4Level_NASF == 3] <- 3
data <- data %>% 
  rename(PE_4Level = PE_3Level)
### convert both ordinal variables to factors
#### Ordinal PE
data$PE_4Level <- as.factor(data$PE_4Level)
#### ordinal PE not attributable to sleep/fever
data$PE_4Level_NASF <- as.factor(data$PE_4Level_NASF)


### Derive a binary variable for PEs not attributable to sleep or fever
data <- data %>% 
  mutate(PE_NASF = case_when(PE_4Level_NASF == 0 ~ 0,
                             PE_4Level_NASF == 1 ~ 1,
                             PE_4Level_NASF == 2 ~ 1,
                             PE_4Level_NASF == 3 ~ 1))
data$PE_NASF <- as.factor(data$PE_NASF)


# Convert ordinal PE at age 12 to a binary variable
data$PE_age_12[data$PE_age_12 == 2] <- 1
data$PE_age_12 <- as.factor(data$PE_age_12)

# Create a variable for ascertainment of PE at age 12
data <- data %>% 
  mutate(PE_age_12_Ascert = case_when(PE_age_12 == 0 ~ 1, 
                                      PE_age_12 == 1 ~ 1,
                                      is.na(PE_age_12) ~ 0))
data$PE_age_12_Ascert <- as.factor(data$PE_age_12_Ascert)
                          
# Derive a categorical variable with resolving (=1) and emergent (=2) and reccuring (=3) PEs between age 12 and 18
data <- data %>% 
  mutate(PE_18_12= case_when(is.na(PE) & is.na(PE_age_12) ~ NA, 
                             is.na(PE_age_12) & PE == 0 ~ NA,
                             PE_age_12 == 0 & PE == 0 ~ 0,
                             PE_age_12 == 1 & PE == 0 ~ 1,
                             PE_age_12 == 0 & PE == 1 ~ 2,
                             PE_age_12 == 1 & PE == 1 ~ 3))
                             
data$PE_18_12 <- as.factor(data$PE_18_12)

## Further create a version where resolving and emergent PE are merged into one category (=1)
data <- data %>% 
  mutate(PE_18_12_R = case_when(is.na(PE_18_12) ~ NA, 
                                PE_18_12 == 0 ~ 0,
                                PE_18_12 == 1 ~ 1,
                                PE_18_12 == 2 ~ 1,
                                PE_18_12 == 3 ~ 2))
data$PE_18_12_R <- as.factor(data$PE_18_12_R)
summary(data)

# Convert maternal education from a 5-level to a 3-level variable
data <- data %>% 
  mutate(ME = case_when(is.na(ME) ~ NA, 
                        ME == 1 ~ 1,
                        ME == 2 ~ 1,
                        ME == 3 ~ 2,
                        ME == 4 ~ 3,
                        ME == 5 ~ 3))
                              
data$ME <- as.factor(data$ME)
summary(data)

# Derive a binary variable for parental social class
# First, collapse maternal and paternal social class into a binary variable
## maternal
data$mSC[data$mSC < 4] <- 1
data$mSC[data$mSC > 3] <- 0
data$mSC <- as.factor(data$mSC)
## paternal
data$pSC[data$pSC < 4] <- 1
data$pSC[data$pSC > 3] <- 0
data$pSC <- as.factor(data$pSC)

## Derived combined paternal social class (highest across both parents)
data <- data %>% 
  mutate(parSC = case_when(is.na(mSC) & is.na(pSC) ~ NA, 
                           is.na(mSC) & pSC == 0 ~ 0,
                           mSC == 0 & is.na(pSC) ~ 0,
                           mSC == 0 & pSC == 0 ~ 0, 
                           mSC == 1 & is.na(pSC) ~ 1,
                           is.na(mSC) & pSC == 1 ~ 1,
                           mSC == 0 & pSC == 1 ~ 1, 
                           mSC == 1 & pSC == 0 ~ 1,
                           mSC == 1 & pSC == 1 ~ 1))
data$parSC <- as.factor(data$parSC)

## summary(data)

# last, create a variable representing the reason for exclusion due to image quality (2=failed QC, NA = failed reconstruction)
data <- data %>% 
  mutate(excl_reason = case_when(is.na(CorticalQC) ~ 1, 
                                 is.na(SubCorticalQC) ~ 1,
                                 CorticalQC== 2 ~ 2,
                                 SubCorticalQC== 2 ~ 2
                                 ))
data$excl_reason <- as.factor(data$excl_reaso)


# Step 2. Inspection and exclusion of scans that failed image reconstruction/QC

# Select those who failed image reconstruction (= NA) /QC for cortical/sub-cortical QC (=2)
data_exc <- data %>% filter(CorticalQC == 2 | is.na(CorticalQC) | SubCorticalQC == 2 | is.na(SubCorticalQC))
summary(data_exc)
# Tabulate basic characteristics of excluded participants for table S1
data_exc %>%  
  tbl_summary(
    include = c(Age, Sex, PE_4Level, excl_reason),
    by = PE,
    statistic = 
      all_continuous() ~ "{mean} ({sd})",
    digits = list(all_categorical() ~ c(0, 2),
                  all_continuous() ~ c(0,2))
  )

## Select those with 'pass' or 'moderate' cortical and subcortical QC for downstream analyses 
data <- data %>% filter((CorticalQC == "0" | CorticalQC == "1") & 
                          (SubCorticalQC == "0" | SubCorticalQC == "1"))

# save final dataset
save(data, file='ALSPAC_PE_BA_FINAL.RData')

cat('finished with step 4a')

# done
# Move to script 4b