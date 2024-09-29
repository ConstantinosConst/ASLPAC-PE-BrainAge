# ASLPAC-PE-BrainAge analyses

This repository provides code/scripts for secondary data analyses as reported in the following (draft) manuscript: 

Constantinides, C., Caramaschi, D., Zammit, S., Freeman, T. P., &amp; Walton, E. (2024). Exploring associations between psychotic experiences and structural brain age: a population-based study in late adolescence [WORKING PAPER]

Here we used data from the ALSPAC-Psychotic Experiences (PE) imaging study as described previously (see data note by Sharp et al., 2020). 

### Step 1. Sample selection (including variables of interest)

The following R script was used to extract the original ALSPAC-PE imaging sample (and variables of interest) from the wider ALSPAC birth cohort: 

(1) '1_Extract_ALSPAC_PE_Imaging_Dataset_20240928'

A desicription of the ALSPAC variables used for the current analyses can be found in the following Excel spreadsheet: 'Variables_Info_ALSPAC_PE_CC_20240926.xlsx'. Note that column names for imaging-derived phenotype (IDPs) may differ in future data releases. See spreadsheet for column names as they currently appear in the ALSPAC data disctionary/variable search tool (https://variables.alspac.bris.ac.uk).

Imaging derived-phenotypes (IDPs) were previously derived from sMRI/T1-weighted scans (Sharp et al., 2020). 

### Step 2. Brain age prediction using the ENIGMA Brain Age model (Han et al, 2020) 
(NB. You may skip this step if you want to use the CentileBrain model only)

The following R scripts were used sequencically to prepare the model input files for the ALSPAC-PE imaging sample:

(2a) '2a_ENIGMA_Input_Prep_Step_2a_20240920.R' 

(2b) '2b_ENIGMA_Input_Prep_Step_2b_20240928.R'

Plus two additional scripts that are loaded by script 2b as functions:
'prepare.files.R',
'get.means.R'

After running script 2a and 2b, the generated input files (males_raw.csv/females.raw.csv) should uploaded on the PHOTONAI platform to get predictions: 

https://photon-ai.com/enigma_brainage

Once you are on the platform, make sure that you click on the correct sex group (males/females) before uploading the corresponding input file. The downloaded output files should be renamed to 'males_raw_out' and 'females_raw_out' for males and females respectively. 

Finally, the following script was run to generate model performance metrics/plots for the current sample and to prepare dataset for downstream statistical analyses (step 4):
(2c) '2c_ENIGMA_Output_Prep_Step_2c_20240928.R'
Plus two additional scripts that loaded by script 2c as 'functions': 
'model.fits.brainAge.R'
'model.fits.brainAge.PE.R'

### Step 3. Brain age prediction using the CentileBrain Brain Age model

a. For guidance on brain age prediction using the the CentileBrain model, please refer to the following resource: https://centilebrain.org/#/brainAge2

### Step 4. Statistical analyses

The R code used for statistical analyses (incl. sample discriptives and data visualisation) can be found in the relevant script: '3_ALSPAC_PE_BA_Stats_CC_04092024.R'. Scripts for producing correlations between brain age and FreeSurfer measure (feature imprtortance) will be added soon. 

### References

Sharp, T. H., McBride, N. S., Howell, A. E., Evans, C. J., Jones, D. K., Perry, G., Dimitriadis, S. I., Lancaster, T. M., Zuccolo, L., Relton, C., Matthews, S. M., Breeze, T., David, A. S., Drakesmith, M., Linden, D. E. J., Paus, T., & Walton, E. (2020). Population neuroimaging: generation of a comprehensive data resource within the ALSPAC pregnancy and birth cohort. Wellcome open research, 5, 203. https://doi.org/10.12688/wellcomeopenres.16060.
