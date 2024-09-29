###### Project: Exploring associations between psychotic experiences and structural brain age: a population-based study in late adolescence ######
###### Script 3a: Preparing ALSPAC-PE imaging dataset for brain age prediction using the CentileBrain model - step 3a
###### Description: Preparing input files for brain age prediction by adding/formatting required columns and splitting sample with respect to sex
###### Written by: Constantinos Constantinides ######
###### Last edited: 28/09/2024 ######

# Set working directory
# setwd ("...")

# Load libraries
#library(haven)
#library(labelled)
library(arsenal)
library(stringr)
library(dplyr) 
library(finalfit)
library(scrutiny)
library(rJava)
library(xlsx)
options(max.print = 1000000)

# Load ALSPAC-PE Imaging dataset (as prepared by script #1)
data <- read.csv("ALSPAC_PE_Imaging.csv", header=TRUE)

# load column headers for CentileBrain model input files
headers <- read.csv("CentileBrain_headers.csv", header = TRUE)
## view (headers)

# First, remove those observation with missing or invalid IDP values (i.e. '0' or 'NA) due to failed image pre-processing 
ALSPAC_PE_IDP_Missing <- data %>% filter(is.na(CorticalQC) | is.na(SubCorticalQC))
## Exclude those participants from the original dateset
data  <- anti_join(data, ALSPAC_PE_IDP_Missing, by="SubjID")
## Save characteristics of excluded participants separately for descriptive purposes later on
ALSPAC_PE_IDP_Missing <- ALSPAC_PE_IDP_Missing[,c("SubjID", "PE", "Age", "Sex",                                 
                                                  "PE_3Level", "PE_4Level_NASF", "PE_Sum_NASF", "PsyDis",                                                                                "PE_age_12", "BW", "ME", "mSC", "pSC", "IQ_8",        
                                                  "CorticalQC", "SubCorticalQC" 
)]
save(ALSPAC_PE_IDP_Missing,file="ALSPAC_PE_IDP_Missing.RData")


# Remove covariates and IDPs not required for CentileBrain model input

data <- data %>% select(-c("PE", "PE_3Level", "PE_4Level_NASF", "PE_Sum_NASF", "PsyDis",
                                                         "PE_age_12", "BW", "ME", "mSC", "pSC", "IQ_8", "CorticalQC", "SubCorticalQC", 
                                                         "lthickness", "rthickness", "lsurfarea", "rsurfarea", "icv", "llatvent", "rlatvent"))


# Compare data frame with CentileBrain headers 
summary(comparedf(headers, data))

# Format basic covariates for CentileBrain model input
data <- rename(data, age = Age,
                sex = Sex,
                SubjectID = SubjID)


# compare IDP variables with template headers and fix column names
summary(comparedf(headers, data))
## Capitalize l_(for Left) and r_(for Right)
names(data) <- sub('l_', 'L_', names(data))
names(data) <- sub('r_', 'R_', names(data))
## Uncapitalize "aL_" to "al_"
names(data) <- sub('aL_', 'al_', names(data))
## Rename individual IDPs to match those of the template
names(data)[names(data) == "L_rostralanteriorcingulate_thick"] <- "L_rostralanteriorcingulate_thickavg"
names(data)[names(data)  == "R_caudalanteriorcingulate_thicka"] <- "R_caudalanteriorcingulate_thickavg"
names(data)[names(data)  == "R_rostralanteriorcingulate_thick"] <- "R_rostralanteriorcingulate_thickavg"
names(data)[names(data)  == "L_entorhinal_thickavg"] <- "L_entorhil_thickavg"
names(data)[names(data)  == "L_supramarginal_thickavg"] <- "L_supramargil_thickavg"
names(data)[names(data)  == "R_entorhinal_thickavg"] <- "R_entorhil_thickavg"
names(data)[names(data)  == "R_supramarginal_thickavg"] <- "R_supramargil_thickavg"
names(data)[names(data)  == "L_caudalanteriorcingulate_surfav"] <- "L_caudalanteriorcingulate_surfavg"
names(data)[names(data)  == "L_rostralanteriorcingulate_surfa"] <- "L_rostralanteriorcingulate_surfavg"
names(data)[names(data)  == "R_caudalanteriorcingulate_surfav"] <- "R_caudalanteriorcingulate_surfavg"
names(data)[names(data) == "R_rostralanteriorcingulate_surfa"] <- "R_rostralanteriorcingulate_surfavg"
names(data)[names(data)  == "L_entorhinal_surfavg"] <- "L_entorhil_surfavg"
names(data)[names(data) == "L_supramarginal_surfavg"] <- "L_supramargil_surfavg"
names(data)[names(data) == "R_entorhinal_surfavg"] <- "R_entorhil_surfavg"
names(data)[names(data)  == "R_supramarginal_surfavg"] <- "R_supramargil_surfavg"
names(data)[names(data)  == "lthal"] <- "Lthal"
names(data)[names(data)  == "rthal"] <- "Rthal"
names(data)[names(data)  == "lcaud"] <- "Lcaud"
names(data)[names(data)  == "rcaud"] <- "Rcaud"
names(data)[names(data)  == "lput"] <- "Lput"
names(data)[names(data)  == "rput"] <- "Rput"
names(data)[names(data)  == "lpal"] <- "Lpal"
names(data)[names(data)  == "rpal"] <- "Rpal"
names(data)[names(data) == "lamyg"] <- "Lamyg"
names(data)[names(data)  == "ramyg"] <- "Ramyg"
names(data)[names(data)  == "lhippo"] <- "Lhippo"
names(data)[names(data)  == "rhippo"] <- "Rhippo"
names(data)[names(data)  == "laccumb"] <- "Laccumb" 
names(data)[names(data)  == "raccumb"] <- "Raccumb" 
## compare again with headers
summary(comparedf(headers, data))
# Non non-shared variable


# Add some required non-imaging variables 
data$SITE <- "ALSPAC_PE"
data$ScannerType <- "3T_General_Electric_HDx"
data$FreeSurfer_Version <- "6.0.0"

# Reorder columns
colnames(data)
data <- data[, c(154,1:3, 155:156,4:153)]

## Do a final check to assess whether all required columns are in the dataset
names(headers) %in% names(data)
## All TRUE 

## split with respect to sex
CB_male_raw <- data %>% filter (sex==0)
CB_female_raw <- data %>% filter (sex==1)

# Export csv files for brain age prediction
write.xlsx(CB_male_raw,'CB_males_raw.xlsx', row.names = FALSE)
write.xlsx(CB_female_raw,'CB_females_raw.xlsx', row.names = FALSE)

# Upload .xlsx files on the CentileBrain platform as described in README file and download .csv ouput file for predicted brain age. 
# NOTE: You need to rename the downloaded files to 'CB_male_raw_out' and 'CB_female_raw_out' for males and females, respectively.  
# and before running the script 3b

cat('finished with step 3a')

### End ###
### Proceed to script 3b
