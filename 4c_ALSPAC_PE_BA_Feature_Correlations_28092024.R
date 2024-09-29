# Project: Exploring associations between psychotic experiences and structural brain age: a population-based study in late adolescence [WORKING PAPER]
# Script: 4c. Calculating IDPs-predicted brain age pearson's correlation coefficients for feature importance
# Description: Generate correlation coffeicents for each input feature (IDP) and brain-predicted age. 
# Written by: Constantinos Constaninides
# Written/last update on: 28/09/2024


# Load packages
library (ggplot2)
library(dplyr)
#library(pastecs)
#library(psych)
#library(haven)
#library(labelled)
#library(sensemakr)
#library(olsrr)
#library(smplot2)
#library(effectsize)
library(Hmisc)
options(max.print=999999)

# Load IDPs for ENIGMA and CentileBrain brain age prediction (including brain-predicted age)
## ENIGMA
load('ALSPAC_PE_BA_ENG_FI.RData')
data_FI_ENG <- data_FI
rm(data_FI)

## CentileBrain
load('ALSPAC_PE_BA_CB_FI.RData')
data_FI_CB <- data
rm(data)


# A.Produce correlation matrix for IDP-CentileBrain-predicted age

## Exclude failed QC
data_FI_CB <- data_FI_CB  %>% filter(CorticalQC != 2)
data_FI_CB <- data_FI_CB  %>% filter(SubCorticalQC != 2)

##  Now generate correlations for total sample
dat_matrix=data.matrix(data_FI_CB)
correlations=rcorr(dat_matrix)
FI.r=data.frame(correlations$r)
FI.r$FreeSurfer_ROI <- rownames(FI.r)
### remove unnesary columns/rows
colnames(FI.r)
FI.r <- FI.r[,c("FreeSurfer_ROI", "predAge_CB")]
rownames(FI.r)
FI.r <- FI.r[-c(1:6, 157),]
### rename
names(FI.r)[names(FI.r) == "predAge_CB"] <- "Corr_All"

## Repeat for PE and no PE groups

# Isolate those without PE
FI_NoPE <- data_FI_CB %>% filter(PE==0)
# Isolate those with PE
FI_PE <- data_FI_CB %>% filter(PE==1)
rm(data_FI_CB)

## Generate correlations for those without PE and with PE
### NoPE
dat_matrix=data.matrix(FI_NoPE)
rm(FI_NoPE)
correlations=rcorr(dat_matrix)
FI.r.NoPE=data.frame(correlations$r)
FI.r.NoPE$FreeSurfer_ROI <- rownames(FI.r.NoPE)
### remove unnecessary columns/rows
colnames(FI.r.NoPE)
FI.r.NoPE <- FI.r.NoPE[,c("FreeSurfer_ROI", "predAge_CB")]
rownames(FI.r.NoPE)
FI.r.NoPE <- FI.r.NoPE[-c(1:6, 157),]
### rename
names(FI.r.NoPE)[names(FI.r.NoPE) == "predAge_CB"] <- "Corr_NoPE"


### PE
dat_matrix=data.matrix(FI_PE)
rm(FI_PE)
correlations=rcorr(dat_matrix)
FI.r.PE=data.frame(correlations$r)
FI.r.PE$FreeSurfer_ROI <- rownames(FI.r.PE)
### remove unnecessary columns/rows
colnames(FI.r.PE)
FI.r.PE <- FI.r.PE[,c("FreeSurfer_ROI", "predAge_CB")]
rownames(FI.r.PE)
FI.r.PE <- FI.r.PE[-c(1:6, 157),]
### rename
names(FI.r.PE)[names(FI.r.PE) == "predAge_CB"] <- "Corr_PE"


# merge all coefficients for total sample and PE/No PE groups
FI.r.All <- merge(FI.r, FI.r.NoPE, by="FreeSurfer_ROI")
rm(FI.r.NoPE)
FI.r.All <- merge(FI.r.All, FI.r.PE, by="FreeSurfer_ROI")
rm(FI.r.PE)

# Sort correlations from negative to positive values (for total sample)
FI.r.All <- FI.r.All[order(FI.r.All$Corr_All, decreasing = FALSE),]

# Round values to 3 significant figures
FI.r.All$Corr_All <- round(FI.r.All$Corr_All, digits = 2)
FI.r.All$Corr_NoPE <- round(FI.r.All$Corr_NoPE, digits = 2)
FI.r.All$Corr_PE <- round(FI.r.All$Corr_PE, digits = 2)

# Write csv file for table S8
write.csv(FI.r.All, "CB_FI.csv", row.names = FALSE)
# Check file location to see if table with coeffcients has been successfully generated.

# Let's calculate average correlation for the total sample
rownames(FI.r)
# Cortical Thickness
FI.r.thick <- FI.r[c(1:68),]
mean(FI.r.thick$Corr_All)
sd(FI.r.thick$Corr_All)
# Surface area
FI.r.SA <- FI.r[c(69:136),]
mean(FI.r.SA$Corr_All)
sd(FI.r.SA$Corr_All)
# Subcortical Volumes
FI.r.vol <- FI.r[c(137:150),]
mean(FI.r.vol$Corr_All)
sd(FI.r.vol$Corr_All)


# done for CentileBrain-predicted age-IDP correlations.  

# B. Produce correlation matrix for IDPs and ENIGMA-predicted age

## Exclude failed QC
data_FI_ENG <- data_FI_ENG  %>% filter(CorticalQC != 2)
data_FI_ENG <- data_FI_ENG  %>% filter(SubCorticalQC != 2)

##  Now generate correlations for total sample
dat_matrix=data.matrix(data_FI_ENG)
correlations=rcorr(dat_matrix)
FI.r=data.frame(correlations$r)
FI.r$FreeSurfer_ROI <- rownames(FI.r)
### remove unnesary columns/rows
colnames(FI.r)
FI.r <- FI.r[,c("FreeSurfer_ROI", "predAge_ENG")]
rownames(FI.r)
FI.r <- FI.r[-c(1:6, 84),]
### rename
names(FI.r)[names(FI.r) == "predAge_ENG"] <- "Corr_All"

## Repeat for PE and no PE groups

# Isolate those without PE
FI_NoPE <- data_FI_ENG %>% filter(PE==0)
# Isolate those with PE
FI_PE <- data_FI_ENG %>% filter(PE==1)
rm(data_FI_ENG)

## Generate correlations for those without PE and with PE
### NoPE
dat_matrix=data.matrix(FI_NoPE)
rm(FI_NoPE)
correlations=rcorr(dat_matrix)
FI.r.NoPE=data.frame(correlations$r)
FI.r.NoPE$FreeSurfer_ROI <- rownames(FI.r.NoPE)
### remove unnecessary columns/rows
colnames(FI.r.NoPE)
FI.r.NoPE <- FI.r.NoPE[,c("FreeSurfer_ROI", "predAge_ENG")]
rownames(FI.r.NoPE)
FI.r.NoPE <- FI.r.NoPE[-c(1:6, 84),]
### rename
names(FI.r.NoPE)[names(FI.r.NoPE) == "predAge_ENG"] <- "Corr_NoPE"


### PE
dat_matrix=data.matrix(FI_PE)
rm(FI_PE)
correlations=rcorr(dat_matrix)
FI.r.PE = data.frame(correlations$r)
FI.r.PE$FreeSurfer_ROI <- rownames(FI.r.PE)
### remove unnecessary columns/rows
colnames(FI.r.PE)
FI.r.PE <- FI.r.PE[,c("FreeSurfer_ROI", "predAge_ENG")]
rownames(FI.r.PE)
FI.r.PE <- FI.r.PE[-c(1:6, 157),]
### rename
names(FI.r.PE)[names(FI.r.PE) == "predAge_ENG"] <- "Corr_PE"


# merge all coefficients for total sample and PE/No PE groups
FI.r.All <- merge(FI.r, FI.r.NoPE, by="FreeSurfer_ROI")
rm(FI.r.NoPE)
FI.r.All <- merge(FI.r.All, FI.r.PE, by="FreeSurfer_ROI")
rm(FI.r.PE)

# Sort correlations from negative to positive values (for total sample)
FI.r.All <- FI.r.All[order(FI.r.All$Corr_All, decreasing = FALSE),]

# Round values to 3 significant figures
FI.r.All$Corr_All <- round(FI.r.All$Corr_All, digits = 2)
FI.r.All$Corr_NoPE <- round(FI.r.All$Corr_NoPE, digits = 2)
FI.r.All$Corr_PE <- round(FI.r.All$Corr_PE, digits = 2)

# Write csv file for table S8
write.csv(FI.r.All, "ENG_FI.csv", row.names = FALSE)
# Check file location to see if table with coeffcients has been successfully generated.

# Let's calculate average correlation for the total sample
rownames(FI.r)
##  Cortical Thickness
FI.r.thick <- FI.r[c(10:43),]
mean(FI.r.thick$Corr_All)
sd(FI.r.thick$Corr_All)
##  Surface area
FI.r.SA <- FI.r[c(44:77),]
mean(FI.r.SA$Corr_All)
sd(FI.r.SA$Corr_All)
##  Sucortical Volumes (excl. latteral ventricles and ICV)
FI.r.vol <- FI.r[c(3:9),]
mean(FI.r.vol$Corr_All)
sd(FI.r.vol$Corr_All)
## Latteral ventricles
FI.r.lv <- FI.r[c(2),]
FI.r.lv
## ICV
FI.r.icv <- FI.r[c(1),]
FI.r.icv

# Done with ENIGMA-predicted age-IDP correlations. 
# Done with step 4


## END ##

