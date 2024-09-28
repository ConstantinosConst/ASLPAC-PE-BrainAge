# ASLPAC-PE-BrainAge analyses
This repository provides code/scripts for secondary data analyses as reported in the following (draft) manuscript: Constantinides, C., Caramaschi, D., Zammit, S., Freeman, T. P., &amp; Walton, E. (2024). Exploring associations between psychotic experiences and structural brain age: a population-based study in late adolescence [WORKING PAPER]

### Step 1. Sample selection 

The following R script was used to extract the origng ALSPAC-PE imaging sample (and variables of interest) from the wider ALSPAC cohort: 

'1_Extract_ALSPAC_PE_Imaging_Dataset_20240928'

A desicription of the ALSPAC variables used for the current analyses can be found in the following Excel spreadsheet: 'Variables_Info_ALSPAC_PE_CC_20240926.xlsx'. Note that column names for imaging-derived phenotype (IDPs) may differ in future data releases. See spreadsheet for column names as they currently appear in the ALSPAC data disctionary/variable search tool (https://variables.alspac.bris.ac.uk). 

### Step 2. Brain age prediction using the ENIGMA Brain Age model

b. For guidance on brain age prediction using the ENIGMA model (supplementary analysis), please refer to the following resources: (a) https://photon-ai.com/enigma_brainage, (b) https://github.com/ConstantinosConst/ENIGMA-SZ-BrainAge

### Step 3. Brain age prediction using the CentileBrain Brain Age model

a. For guidance on brain age prediction using the the CentileBrain model, please refer to the following resource: https://centilebrain.org/#/brainAge2

### Step 4. Statistical analyses

The R code used for statistical analyses (incl. sample discriptives and data visualisation) can be found in the relevant script: '3_ALSPAC_PE_BA_Stats_CC_04092024.R'. Scripts for producing correlations between brain age and FreeSurfer measure (feature imprtortance) will be added soon. 

### References
TBA
