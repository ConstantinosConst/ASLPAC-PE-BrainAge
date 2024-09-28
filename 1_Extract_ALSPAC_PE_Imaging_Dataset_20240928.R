###### Project: Exploring associations between psychotic experiences and structural brain age: a population-based study in late adolescence ######
###### Script 1: Extraction of the ALSPAC-PE imaging sub-sample from the ALSPAC dataset ######
###### Description: Selection participants of the original ALSPAC-PE imaging sub-study along with variables of interest from the ALSPAC core dataset ######
###### Written by: Constantinos Constantinides ######
###### Last edited: 28/09/2024 ######

# Set working directory to data file location
# setwd("...")

# Load libraries
library(haven)
library(labelled)
library(stringr)
library(dplyr)
options(max.print = 1000000)

# Load ALSPAC dataset for approved project (B3067)
df <- read_sav("./CIDB3067_26Aug2023.sav")

# generate a searchable data dictionary from the full dataset
# ALSPAC_CIDB3067_Dictionary <- labelled::generate_dictionary(df)
## save(ALSPAC_CIDB3067_Dictionary,file="ALSPAC_CIDB3067_26Aug_Dictionary.RData")

# merge the cidB3067 and qlet to derive an ID column for each G1 offspring
df$SubjID <-str_c(df$cidB3067, '', df$qlet)
## cidB3067 provides a unique number per each mother for the approved ALSPAC project (B3067)
## qlet indicates the order of birth (i.e, A, B) of each offspring

# Get the total number of G1 offspring participated in the ALSPAC-PE imaging study (MRI.study1)
MRI_Study1_size <- factor(df$MRI.study1)
summary(MRI_Study1_size)
## N=252

# Subset offspring who participated in the ALSPAC-PE imaging sub-study study 
ALSPAC_PE_Imaging <- df[which(df$MRI.study1 == 1),]
rm(df)

# Select variables of interest for the ALSPAC-PE-BraiAge study
## NOTE (SOS): Column names for imaging-derived phenotype (IDPs) may differ in future data releases
## See accompanied excel spreadsheet for column names as appear in the ALSPAC dictionary/search tool (https://variables.alspac.bris.ac.uk)

ALSPAC_PE_Imaging <- ALSPAC_PE_Imaging [, c("SubjID", "casecontrol.1", "ageyears.1", "kz021", 
                          "david_plikscat", "FJPL167_new", "FJPL170","FJPL172", 
                          "ff5263", "kz030", "c645a", "c755", "c765", 
                          "f8ws112",  "MRI_YP_D1157", "MRI_YP_D1158", 
                          "l_bankssts_thickavg.1", "l_caudalanteriorcingulate_thickavg.1", "l_caudalmiddlefrontal_thickavg.1",    
                          "l_cuneus_thickavg.1", "l_entorhinal_thickavg.1", "l_fusiform_thickavg.1", "l_inferiorparietal_thickavg.1",       
                          "l_inferiortemporal_thickavg.1", "l_isthmuscingulate_thickavg.1", "l_lateraloccipital_thickavg.1", "l_lateralorbitofrontal_thickavg.1",   
                          "l_lingual_thickavg.1", "l_medialorbitofrontal_thickavg.1", "l_middletemporal_thickavg.1", "l_parahippocampal_thickavg.1",        
                          "l_paracentral_thickavg.1", "l_parsopercularis_thickavg.1", "l_parsorbitalis_thickavg.1", "l_parstriangularis_thickavg.1",       
                          "l_pericalcarine_thickavg.1", "l_postcentral_thickavg.1", "l_posteriorcingulate_thickavg.1", "l_precentral_thickavg.1",              
                          "l_precuneus_thickavg.1", "l_rostralanteriorcingulate_thick.1", "l_rostralmiddlefrontal_thickavg.1", "l_superiorfrontal_thickavg.1",     
                          "l_superiorparietal_thickavg.1", "l_superiortemporal_thickavg.1", "l_supramarginal_thickavg.1", "l_frontalpole_thickavg.1",            
                          "l_temporalpole_thickavg.1", "l_transversetemporal_thickavg.1", "l_insula_thickavg.1", "r_bankssts_thickavg.1",               
                          "r_caudalanteriorcingulate_thicka.1", "r_caudalmiddlefrontal_thickavg.1", "r_cuneus_thickavg.1", "r_entorhinal_thickavg.1",             
                          "r_fusiform_thickavg.1", "r_inferiorparietal_thickavg.1", "r_inferiortemporal_thickavg.1", "r_isthmuscingulate_thickavg.1",     
                          "r_lateraloccipital_thickavg.1", "r_lateralorbitofrontal_thickavg.1", "r_lingual_thickavg.1", "r_medialorbitofrontal_thickavg.1",  
                          "r_middletemporal_thickavg.1", "r_parahippocampal_thickavg.1", "r_paracentral_thickavg.1", "r_parsopercularis_thickavg.1",       
                          "r_parsorbitalis_thickavg.1", "r_parstriangularis_thickavg.1", "r_pericalcarine_thickavg.1", "r_postcentral_thickavg.1",            
                          "r_posteriorcingulate_thickavg.1", "r_precentral_thickavg.1", "r_precuneus_thickavg.1", "r_rostralanteriorcingulate_thick.1", 
                          "r_rostralmiddlefrontal_thickavg.1", "r_superiorfrontal_thickavg.1", "r_superiorparietal_thickavg.1", "r_superiortemporal_thickavg.1",       
                          "r_supramarginal_thickavg.1","r_frontalpole_thickavg.1", "r_temporalpole_thickavg.1", "r_transversetemporal_thickavg.1", 
                          "r_insula_thickavg.1", "lthickness.1", "rthickness.1", 
                          "l_bankssts_surfavg.1", "l_caudalanteriorcingulate_surfav.1", "l_caudalmiddlefrontal_surfavg.1",   
                          "l_cuneus_surfavg.1", "l_entorhinal_surfavg.1", "l_fusiform_surfavg.1", "l_inferiorparietal_surfavg.1",      
                          "l_inferiortemporal_surfavg.1", "l_isthmuscingulate_surfavg.1", "l_lateraloccipital_surfavg.1", "l_lateralorbitofrontal_surfavg.1", 
                          "l_lingual_surfavg.1", "l_medialorbitofrontal_surfavg.1", "l_middletemporal_surfavg.1", "l_parahippocampal_surfavg.1",      
                          "l_paracentral_surfavg.1", "l_parsopercularis_surfavg.1", "l_parsorbitalis_surfavg.1", "l_parstriangularis_surfavg.1",      
                          "l_pericalcarine_surfavg.1", "l_postcentral_surfavg.1", "l_posteriorcingulate_surfavg.1", "l_precentral_surfavg.1",             
                          "l_precuneus_surfavg.1", "l_rostralanteriorcingulate_surfa.1", "l_rostralmiddlefrontal_surfavg.1", "l_superiorfrontal_surfavg.1",       
                          "l_superiorparietal_surfavg.1", "l_superiortemporal_surfavg.1", "l_supramarginal_surfavg.1", "l_frontalpole_surfavg.1",           
                          "l_temporalpole_surfavg.1", "l_transversetemporal_surfavg.1", "l_insula_surfavg.1", "r_bankssts_surfavg.1",              
                          "r_caudalanteriorcingulate_surfav.1", "r_caudalmiddlefrontal_surfavg.1", "r_cuneus_surfavg.1", "r_entorhinal_surfavg.1",            
                          "r_fusiform_surfavg.1", "r_inferiorparietal_surfavg.1", "r_inferiortemporal_surfavg.1", "r_isthmuscingulate_surfavg.1",      
                          "r_lateraloccipital_surfavg.1", "r_lateralorbitofrontal_surfavg.1", "r_lingual_surfavg.1", "r_medialorbitofrontal_surfavg.1",  
                          "r_middletemporal_surfavg.1", "r_parahippocampal_surfavg.1", "r_paracentral_surfavg.1", "r_parsopercularis_surfavg.1",       
                          "r_parsorbitalis_surfavg.1", "r_parstriangularis_surfavg.1", "r_pericalcarine_surfavg.1", "r_postcentral_surfavg.1",           
                          "r_posteriorcingulate_surfavg.1", "r_precentral_surfavg.1", "r_precuneus_surfavg.1", "r_rostralanteriorcingulate_surfa.1",
                          "r_rostralmiddlefrontal_surfavg.1", "r_superiorfrontal_surfavg.1", "r_superiorparietal_surfavg.1", "r_superiortemporal_surfavg.1",      
                          "r_supramarginal_surfavg.1", "r_frontalpole_surfavg.1", "r_temporalpole_surfavg.1", "r_transversetemporal_surfavg.1",    
                          "r_insula_surfavg.1", "lsurfarea.1", "rsurfarea.1",
                          "llatvent.1", "rlatvent.1","lthal.1", "rthal.1", 
                          "lcaud.1", "rcaud.1", "lput.1", "rput.1", "lpal.1",    
                          "rpal.1", "lhippo.1", "rhippo.1", "lamyg.1", "ramyg.1", 
                          "laccumb.1", "raccumb.1", "icv.1"
                           )] 
# Imaging-derived phenotype were previously derived from sMRI scans by Sharp et al (2020). Read data note for more details: 
# Sharp TH, et al. Population neuroimaging: generation of a comprehensive data resource within the ALSPAC pregnancy and birth cohort. 
# Wellcome Open Res. 2020 Aug 28;5:203. doi: 10.12688/wellcomeopenres.16060.1. PMID: 33043145; PMCID: PMC7531050.


# Rename non-imaging variables/covariates
ALSPAC_PE_Imaging <- ALSPAC_PE_Imaging %>% 
  rename(PE = casecontrol.1,
         Sex = kz021,
         Age = ageyears.1,
         PE_3Level = david_plikscat, 
         PE_4Level_NASF = FJPL167_new, 
         PE_Sum_NASF = FJPL170, 
         PsyDis = FJPL172,
         PE_age_12 = ff5263,
         BW = kz030,
         IQ_8 = f8ws112,
         ME = c645a,
         mSC = c755,
         pSC = c765,
         CorticalQC = MRI_YP_D1157,
         SubCorticalQC = MRI_YP_D1158)
colnames(ALSPAC_PE_Imaging)

### Recode sex
ALSPAC_PE_Imaging$Sex[ALSPAC_PE_Imaging$Sex == 1] <- 0
ALSPAC_PE_Imaging$Sex[ALSPAC_PE_Imaging$Sex == 2] <- 1

# Remove .1 from column names of imaging-derived phenotype (IDPs)
names(ALSPAC_PE_Imaging) <- sub('.1$', '', names(ALSPAC_PE_Imaging))
colnames(ALSPAC_PE_Imaging)

# save dataset for brain age prediction (steps 2 and 3)
write.csv(ALSPAC_PE_Imaging, file="ALSPAC_PE_Imaging.csv", row.names = FALSE)

#### End ### 