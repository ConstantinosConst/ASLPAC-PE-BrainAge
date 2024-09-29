###### Project: Exploring associations between psychotic experiences and structural brain age: a population-based study in late adolescence ######
###### Script 2b: Preparing ALSPAC-PE imaging dataset for brain age prediction using the ENIGMA model - step 2b
###### Description: Calculating averages IDPs across hemispheres and splitting dataset by sex 
###### Written by: Constantinos Constantinides ######
###### Adapted from: ENIGMA-SCZ-BrainAge study (https://github.com/ConstantinosConst/ENIGMA-SZ-BrainAge.git)
###### Last edited: 28/09/2024 ######

# Set working directory to file location
# setwd

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
       "caret","gridExtra","Hmisc","pastecs","psych","ggplot2") )

# source functions
cat("Prep: sourcing functions\n")

source("get.means.R")  
source("prepare.files.R")
# source("model.fits.brainAge.R")
# source("model.fits.brainAge.PE.R")

# derive mean volumnes / thickness and surfarea

cat("Step 2: Obtaining measures of brain age\n")
cat("deriving mean values for thickness, surface and volume\n") 
  
Thick=get.means("CorticalMeasuresENIGMA_ThickAvg.csv") 
Thick$ICV=NULL  

Surf=get.means("CorticalMeasuresENIGMA_SurfAvg.csv") 
Surf$ICV=NULL 

Vol=get.means("SubcorticalMeasuresENIGMA_VolAvg.csv") 

# merged all together
TS=merge(Thick,Surf,by="row.names")

TSV=merge(TS,Vol,by.x="Row.names",by.y="row.names")

# read in covariates
Covs <- read.csv("Covariates.csv"); #Read in the covariates file

# check for missings in Dx, Age, Sex
if (length(table(is.na(Covs[,c("PE","Sex","Age")])))>1){
  stop("missing data in PE, Age or Sex")
}

# Check that all of the required columns are present
mcols=c("SubjID","PE","Sex","Age")
colind=match(mcols,names(Covs))
if(length(which(is.na(colind))) > 0){
  stop('At least one of the required columns in your Covariates.csv file is missing. Make sure that the column names are spelled exactly as listed\n
       It is possible that the problem column(s) is: ', mcols[which(is.na(colind))])
}

# Check for duplicated SubjIDs that may cause issues with merging data sets.
if(anyDuplicated(Covs[,c("SubjID")]) != 0) { stop('You have duplicate SubjIDs in your Covariates.csv file.\nMake sure there are no repeat SubjIDs.') }

#combine the files into one dataframe
data = merge(Covs, TSV, by.x="SubjID", by.y="Row.names")

cat("creating csv files for brainAge estimation\n")
df=prepare.files(data,names(Covs))

males=df$males
females=df$females
rm(df)

invisible(readline(prompt="You should see 2 csv files in your working directory:\n
-females_raw.csv\n
-males_raw.csv.\n

These should be uploaded on the PHOTON-AI platform as described in README file.
Link to ENIGMA model (PHOTON-AI): https://photon-ai.com/enigma_brainage 
Once you have dowloaded the output files, re-name these to 
'males_raw_out.csv' and 'females_raw_out' for males and females and store them in your working directory, respectively"))

cat('finished with step 2b!')

### Done ###
### Proceed to script to 2c ###