###### Project: Exploring associations between psychotic experiences and structural brain age: a population-based study in late adolescence ######
###### Script 2a: Preparing ALSPAC-PE imaging dataset for brain age prediction using the ENIGMA model - step 2a
###### Description: Preparing input files for step 2b by splitting variables and formatting columns as needed
###### Written by: Constantinos Constantinides ######
###### Last edited: 28/09/2024 ######


# Set working directory to file location
# setwd ("...")

# Load libraries
library(haven)
library(labelled)
library(arsenal)
library(stringr)
library(dplyr) 
library(finalfit)
library(scrutiny)
options(max.print = 1000000)

# Load ALSPAC-PE Imaging dataset (as prepared by script 1)
ALSPAC_PE_Imaging <- read.csv("ALSPAC_PE_Imaging.csv", header = TRUE)

# load/prepare headers for ENIGMA model input files
headers <- read.csv("ENIGMA_headers.csv", header = FALSE)
## view (headers)

# First, remove those observation with missing or invalid IDP values (i.e. '0' or 'NA) due to failed image pre-processing 
ALSPAC_PE_IDP_Missing <- ALSPAC_PE_Imaging %>% filter(is.na(CorticalQC) | is.na(SubCorticalQC))
# Exclude those participants from the original dateset
ALSPAC_PE_Imaging <- anti_join(ALSPAC_PE_Imaging, ALSPAC_PE_IDP_Missing, by="SubjID")
# Save characteristics of excluded participants separately for descriptive purposes later on
ALSPAC_PE_IDP_Missing <- ALSPAC_PE_IDP_Missing[,c("SubjID", "PE", "Age", "Sex",                                 
                                                 "PE_3Level", "PE_4Level_NASF", "PE_Sum_NASF", "PsyDis",                                                                                "PE_age_12", "BW", "ME", "mSC", "pSC", "IQ_8",        
                                                  "CorticalQC", "SubCorticalQC" 
)]
save(ALSPAC_PE_IDP_Missing,file="ALSPAC_PE_IDP_Missing.RData")


# Split variables to non-imaging variables, cortical thickness, cortical surface area and subcortical volumes
# Plus, fix variables names/coding

## Non-imaging variables
Covariates <- ALSPAC_PE_Imaging[,c("SubjID", "PE", "Age", "Sex",                                 
                                   "PE_3Level", "PE_4Level_NASF", "PE_Sum_NASF", "PsyDis",                              
                                   "PE_age_12", "BW", "ME", "mSC", "pSC", "IQ_8",        
                                   "CorticalQC", "SubCorticalQC" 
)]

### Check for duplicated columns
duplicated(Covariates) 

## Cortical thickness                                   
CorticalMeasures_ALSPAC_ThickAvg  <- ALSPAC_PE_Imaging[, c("SubjID", "l_bankssts_thickavg", "l_caudalanteriorcingulate_thickavg", "l_caudalmiddlefrontal_thickavg",    
                                                       "l_cuneus_thickavg", "l_entorhinal_thickavg", "l_fusiform_thickavg", "l_inferiorparietal_thickavg",       
                                                       "l_inferiortemporal_thickavg", "l_isthmuscingulate_thickavg", "l_lateraloccipital_thickavg", "l_lateralorbitofrontal_thickavg",   
                                                       "l_lingual_thickavg", "l_medialorbitofrontal_thickavg", "l_middletemporal_thickavg", "l_parahippocampal_thickavg",        
                                                       "l_paracentral_thickavg", "l_parsopercularis_thickavg", "l_parsorbitalis_thickavg", "l_parstriangularis_thickavg",       
                                                       "l_pericalcarine_thickavg", "l_postcentral_thickavg", "l_posteriorcingulate_thickavg", "l_precentral_thickavg",              
                                                       "l_precuneus_thickavg", "l_rostralanteriorcingulate_thick", "l_rostralmiddlefrontal_thickavg", "l_superiorfrontal_thickavg",     
                                                       "l_superiorparietal_thickavg", "l_superiortemporal_thickavg", "l_supramarginal_thickavg", "l_frontalpole_thickavg",            
                                                       "l_temporalpole_thickavg", "l_transversetemporal_thickavg", "l_insula_thickavg", "r_bankssts_thickavg",               
                                                       "r_caudalanteriorcingulate_thicka", "r_caudalmiddlefrontal_thickavg", "r_cuneus_thickavg", "r_entorhinal_thickavg",             
                                                       "r_fusiform_thickavg", "r_inferiorparietal_thickavg", "r_inferiortemporal_thickavg", "r_isthmuscingulate_thickavg",     
                                                       "r_lateraloccipital_thickavg", "r_lateralorbitofrontal_thickavg", "r_lingual_thickavg", "r_medialorbitofrontal_thickavg",  
                                                       "r_middletemporal_thickavg", "r_parahippocampal_thickavg", "r_paracentral_thickavg", "r_parsopercularis_thickavg",       
                                                       "r_parsorbitalis_thickavg", "r_parstriangularis_thickavg", "r_pericalcarine_thickavg", "r_postcentral_thickavg",            
                                                       "r_posteriorcingulate_thickavg", "r_precentral_thickavg", "r_precuneus_thickavg", "r_rostralanteriorcingulate_thick", 
                                                       "r_rostralmiddlefrontal_thickavg", "r_superiorfrontal_thickavg", "r_superiorparietal_thickavg", "r_superiortemporal_thickavg",       
                                                       "r_supramarginal_thickavg","r_frontalpole_thickavg", "r_temporalpole_thickavg", "r_transversetemporal_thickavg", 
                                                       "r_insula_thickavg", "lthickness", "rthickness", "lsurfarea",                         
                                                       "rsurfarea", "icv")]              
### Check for duplicated columns
duplicated(CorticalMeasures_ALSPAC_ThickAvg)                                            

## Cortical surface Area
CorticalMeasures_ALSPAC_SurfAvg <- ALSPAC_PE_Imaging[, c("SubjID", "l_bankssts_surfavg", "l_caudalanteriorcingulate_surfav", "l_caudalmiddlefrontal_surfavg",   
                                                         "l_cuneus_surfavg", "l_entorhinal_surfavg", "l_fusiform_surfavg", "l_inferiorparietal_surfavg",      
                                                         "l_inferiortemporal_surfavg", "l_isthmuscingulate_surfavg", "l_lateraloccipital_surfavg", "l_lateralorbitofrontal_surfavg", 
                                                         "l_lingual_surfavg", "l_medialorbitofrontal_surfavg", "l_middletemporal_surfavg", "l_parahippocampal_surfavg",      
                                                         "l_paracentral_surfavg", "l_parsopercularis_surfavg", "l_parsorbitalis_surfavg", "l_parstriangularis_surfavg",      
                                                         "l_pericalcarine_surfavg", "l_postcentral_surfavg", "l_posteriorcingulate_surfavg", "l_precentral_surfavg",             
                                                         "l_precuneus_surfavg", "l_rostralanteriorcingulate_surfa", "l_rostralmiddlefrontal_surfavg", "l_superiorfrontal_surfavg",       
                                                         "l_superiorparietal_surfavg", "l_superiortemporal_surfavg", "l_supramarginal_surfavg", "l_frontalpole_surfavg",           
                                                         "l_temporalpole_surfavg", "l_transversetemporal_surfavg", "l_insula_surfavg", "r_bankssts_surfavg",              
                                                         "r_caudalanteriorcingulate_surfav", "r_caudalmiddlefrontal_surfavg", "r_cuneus_surfavg", "r_entorhinal_surfavg",            
                                                         "r_fusiform_surfavg", "r_inferiorparietal_surfavg", "r_inferiortemporal_surfavg", "r_isthmuscingulate_surfavg",      
                                                         "r_lateraloccipital_surfavg", "r_lateralorbitofrontal_surfavg", "r_lingual_surfavg", "r_medialorbitofrontal_surfavg",  
                                                         "r_middletemporal_surfavg", "r_parahippocampal_surfavg", "r_paracentral_surfavg", "r_parsopercularis_surfavg",       
                                                         "r_parsorbitalis_surfavg", "r_parstriangularis_surfavg", "r_pericalcarine_surfavg", "r_postcentral_surfavg",           
                                                         "r_posteriorcingulate_surfavg", "r_precentral_surfavg", "r_precuneus_surfavg", "r_rostralanteriorcingulate_surfa",
                                                         "r_rostralmiddlefrontal_surfavg", "r_superiorfrontal_surfavg", "r_superiorparietal_surfavg", "r_superiortemporal_surfavg",      
                                                         "r_supramarginal_surfavg", "r_frontalpole_surfavg", "r_temporalpole_surfavg", "r_transversetemporal_surfavg",    
                                                         "r_insula_surfavg", "lthickness", "rthickness", "lsurfarea",
                                                         "rsurfarea", "icv")]   
### Check for duplicated columns
duplicated(CorticalMeasures_ALSPAC_SurfAvg)

## Subcortical Volumes
SubcorticalMeasures_ALSPAC_VolAvg <- ALSPAC_PE_Imaging[, c("SubjID", "llatvent", "rlatvent","lthal", "rthal", 
                                                           "lcaud", "rcaud", "lput", "rput", "lpal",    
                                                           "rpal", "lhippo", "rhippo", "lamyg", "ramyg", 
                                                           "laccumb", "raccumb", "icv")]
### Check for duplicated columns
duplicated(SubcorticalMeasures_ALSPAC_VolAvg)

# Compare with tamplate headers and fix IDP column names as needed

## Cortical Thickness
headers_TC <- headers[2,]
headers_TC <- row_to_colnames(headers_TC, row = 1L, collapse = " ", drop = TRUE)
## Cortical Surface area
headers_SA <- headers[3,]
headers_SA <- row_to_colnames(headers_SA, row = 1L, collapse = " ", drop = TRUE)
## Cortical Surface area
headers_Vol <- headers[1,]
headers_Vol <- headers_Vol[,c(1:18)]
headers_Vol <- row_to_colnames(headers_Vol, row = 1L, collapse = " ", drop = TRUE)
rm(headers)

## Compare and fix Cortical thickness variables
summary(comparedf(headers_TC, CorticalMeasures_ALSPAC_ThickAvg))
### Capitalize l_(for Left) and r_(for Right)
names(CorticalMeasures_ALSPAC_ThickAvg) <- sub('l_', 'L_', names(CorticalMeasures_ALSPAC_ThickAvg))
names(CorticalMeasures_ALSPAC_ThickAvg) <- sub('r_', 'R_', names(CorticalMeasures_ALSPAC_ThickAvg))
### Uncapitalize "aL_" to "al_"
names(CorticalMeasures_ALSPAC_ThickAvg) <- sub('aL_', 'al_', names(CorticalMeasures_ALSPAC_ThickAvg))
# Rename individual cortical thickness variables to match those of the template
names(CorticalMeasures_ALSPAC_ThickAvg)[names(CorticalMeasures_ALSPAC_ThickAvg) == "L_rostralanteriorcingulate_thick"] <- "L_rostralanteriorcingulate_thickavg"
names(CorticalMeasures_ALSPAC_ThickAvg)[names(CorticalMeasures_ALSPAC_ThickAvg) == "R_caudalanteriorcingulate_thicka"] <- "R_caudalanteriorcingulate_thickavg"
names(CorticalMeasures_ALSPAC_ThickAvg)[names(CorticalMeasures_ALSPAC_ThickAvg) == "R_rostralanteriorcingulate_thick"] <- "R_rostralanteriorcingulate_thickavg"
names(CorticalMeasures_ALSPAC_ThickAvg)[names(CorticalMeasures_ALSPAC_ThickAvg) == "icv"] <- "ICV"
names(CorticalMeasures_ALSPAC_ThickAvg)[names(CorticalMeasures_ALSPAC_ThickAvg) == "lthickness"] <- "LThickness"
names(CorticalMeasures_ALSPAC_ThickAvg)[names(CorticalMeasures_ALSPAC_ThickAvg) == "rthickness"] <- "RThickness"
names(CorticalMeasures_ALSPAC_ThickAvg)[names(CorticalMeasures_ALSPAC_ThickAvg) == "lsurfarea"] <- "LSurfArea"
names(CorticalMeasures_ALSPAC_ThickAvg)[names(CorticalMeasures_ALSPAC_ThickAvg) == "rsurfarea"] <- "RSurfArea"
# See revised column names and compare again with template
colnames(CorticalMeasures_ALSPAC_ThickAvg)
summary(comparedf(headers_TC, CorticalMeasures_ALSPAC_ThickAvg))
# variables names are now identical 
rm(headers_TC)

## Compare and fix Cortical surface area variables
summary(comparedf(headers_SA, CorticalMeasures_ALSPAC_SurfAvg))
### Capitalize l_(for Left) and r_(for Right)
names(CorticalMeasures_ALSPAC_SurfAvg) <- sub('l_', 'L_', names(CorticalMeasures_ALSPAC_SurfAvg))
names(CorticalMeasures_ALSPAC_SurfAvg) <- sub('r_', 'R_', names(CorticalMeasures_ALSPAC_SurfAvg))
### Uncapitalize "aL_" to "al_"
names(CorticalMeasures_ALSPAC_SurfAvg) <- sub('aL_', 'al_', names(CorticalMeasures_ALSPAC_SurfAvg))
### Rename individual cortical surface area variables to match those of the template
names(CorticalMeasures_ALSPAC_SurfAvg)[names(CorticalMeasures_ALSPAC_SurfAvg) == "L_caudalanteriorcingulate_surfav"] <- "L_caudalanteriorcingulate_surfavg"
names(CorticalMeasures_ALSPAC_SurfAvg)[names(CorticalMeasures_ALSPAC_SurfAvg) == "L_rostralanteriorcingulate_surfa"] <- "L_rostralanteriorcingulate_surfavg"
names(CorticalMeasures_ALSPAC_SurfAvg)[names(CorticalMeasures_ALSPAC_SurfAvg) == "R_caudalanteriorcingulate_surfav"] <- "R_caudalanteriorcingulate_surfavg"
names(CorticalMeasures_ALSPAC_SurfAvg)[names(CorticalMeasures_ALSPAC_SurfAvg) == "R_rostralanteriorcingulate_surfa"] <- "R_rostralanteriorcingulate_surfavg"
names(CorticalMeasures_ALSPAC_SurfAvg)[names(CorticalMeasures_ALSPAC_SurfAvg) == "icv"] <- "ICV"
names(CorticalMeasures_ALSPAC_SurfAvg)[names(CorticalMeasures_ALSPAC_SurfAvg) == "lthickness"] <- "LThickness"
names(CorticalMeasures_ALSPAC_SurfAvg)[names(CorticalMeasures_ALSPAC_SurfAvg) == "rthickness"] <- "RThickness"
names(CorticalMeasures_ALSPAC_SurfAvg)[names(CorticalMeasures_ALSPAC_SurfAvg) == "lsurfarea"] <- "LSurfArea"
names(CorticalMeasures_ALSPAC_SurfAvg)[names(CorticalMeasures_ALSPAC_SurfAvg) == "rsurfarea"] <- "RSurfArea"
# See revised column names and compare again with template
colnames(CorticalMeasures_ALSPAC_SurfAvg)
summary(comparedf(headers_SA, CorticalMeasures_ALSPAC_SurfAvg))
# variables names are now identical
rm(headers_SA)

## Compare and fix subcortical volume variables
summary(comparedf(headers_Vol, SubcorticalMeasures_ALSPAC_VolAvg))
# Rename individuals columns to much those of the template
names(SubcorticalMeasures_ALSPAC_VolAvg)[names(SubcorticalMeasures_ALSPAC_VolAvg) == "llatvent"] <- "LLatVent"
names(SubcorticalMeasures_ALSPAC_VolAvg)[names(SubcorticalMeasures_ALSPAC_VolAvg) == "rlatvent"] <- "RLatVent"
names(SubcorticalMeasures_ALSPAC_VolAvg)[names(SubcorticalMeasures_ALSPAC_VolAvg) == "lthal"] <- "Lthal"
names(SubcorticalMeasures_ALSPAC_VolAvg)[names(SubcorticalMeasures_ALSPAC_VolAvg) == "rthal"] <- "Rthal"
names(SubcorticalMeasures_ALSPAC_VolAvg)[names(SubcorticalMeasures_ALSPAC_VolAvg) == "lcaud"] <- "Lcaud"
names(SubcorticalMeasures_ALSPAC_VolAvg)[names(SubcorticalMeasures_ALSPAC_VolAvg) == "rcaud"] <- "Rcaud"
names(SubcorticalMeasures_ALSPAC_VolAvg)[names(SubcorticalMeasures_ALSPAC_VolAvg) == "lput"] <- "Lput"
names(SubcorticalMeasures_ALSPAC_VolAvg)[names(SubcorticalMeasures_ALSPAC_VolAvg) == "rput"] <- "Rput"
names(SubcorticalMeasures_ALSPAC_VolAvg)[names(SubcorticalMeasures_ALSPAC_VolAvg) == "lpal"] <- "Lpal"
names(SubcorticalMeasures_ALSPAC_VolAvg)[names(SubcorticalMeasures_ALSPAC_VolAvg) == "rpal"] <- "Rpal"
names(SubcorticalMeasures_ALSPAC_VolAvg)[names(SubcorticalMeasures_ALSPAC_VolAvg) == "lamyg"] <- "Lamyg"
names(SubcorticalMeasures_ALSPAC_VolAvg)[names(SubcorticalMeasures_ALSPAC_VolAvg) == "ramyg"] <- "Ramyg"
names(SubcorticalMeasures_ALSPAC_VolAvg)[names(SubcorticalMeasures_ALSPAC_VolAvg) == "lhippo"] <- "Lhippo"
names(SubcorticalMeasures_ALSPAC_VolAvg)[names(SubcorticalMeasures_ALSPAC_VolAvg) == "rhippo"] <- "Rhippo"
names(SubcorticalMeasures_ALSPAC_VolAvg)[names(SubcorticalMeasures_ALSPAC_VolAvg) == "laccumb"] <- "Laccumb" 
names(SubcorticalMeasures_ALSPAC_VolAvg)[names(SubcorticalMeasures_ALSPAC_VolAvg) == "raccumb"] <- "Raccumb" 
names(SubcorticalMeasures_ALSPAC_VolAvg)[names(SubcorticalMeasures_ALSPAC_VolAvg) == "icv"] <- "ICV"
# See column names compare with tamplate,
colnames(SubcorticalMeasures_ALSPAC_VolAvg)
summary(comparedf(headers_Vol, SubcorticalMeasures_ALSPAC_VolAvg))
# variables names are now identical
rm(headers_Vol)

# Export csv files for step 2 of preparation of input files for brain age prediction using the ENIGMA model
write.csv(Covariates,'Covariates.csv', row.names=FALSE)
write.csv(CorticalMeasures_ALSPAC_SurfAvg,'CorticalMeasuresENIGMA_SurfAvg.csv', row.names=FALSE)
write.csv(CorticalMeasures_ALSPAC_ThickAvg,'CorticalMeasuresENIGMA_ThickAvg.csv', row.names=FALSE)
write.csv(SubcorticalMeasures_ALSPAC_VolAvg,'SubcorticalMeasuresENIGMA_VolAvg.csv', row.names=FALSE)

cat('finished with step 2a!\n')

### Done ###
### Proceed to script 2b ###
