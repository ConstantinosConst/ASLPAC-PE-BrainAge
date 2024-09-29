# ASLPAC-PE-BrainAge analyses

This repository provides code/scripts for secondary data analyses as reported in the following (draft) manuscript: 

Constantinides, C., Caramaschi, D., Zammit, S., Freeman, T. P., &amp; Walton, E. (2024). Exploring associations between psychotic experiences and structural brain age: a population-based study in late adolescence [WORKING PAPER]

Here we used data from the ALSPAC-Psychotic Experiences (PE) imaging study as described previously (see data note by Sharp et al., 2020). 

### Step 1. Sample selection (including variables of interest)

The following R script was used to extract the original ALSPAC-PE imaging sample (and variables of interest) from the wider ALSPAC birth cohort: 

(1) '1_Extract_ALSPAC_PE_Imaging_Dataset_20240928'

A desicription of the ALSPAC variables used for the current analyses can be found in the following Excel spreadsheet: 'Variables_Info_ALSPAC_PE_CC_20240926.xlsx'. Note that column names for imaging-derived phenotype (IDPs) may differ in future data releases. See spreadsheet for column names as they currently appear in the ALSPAC data disctionary/variable search tool (https://variables.alspac.bris.ac.uk).

Imaging derived-phenotypes (IDPs) were previously derived from sMRI/T1-weighted scans (Sharp et al., 2020). 

### Step 2. Brain age prediction using the ENIGMA Brain Age model (Han et al., 2020) 
(NB. You may skip this step if you want to use the CentileBrain model only)

The following R scripts were used sequentially to prepare the model input files for the ALSPAC-PE imaging sample:

(2a) '2a_ENIGMA_Input_Prep_Step_2a_20240920.R' - formats column names based on the headers used by the ENIGMA model ('ENIGMA_headers.csv'). 

(2b) '2b_ENIGMA_Input_Prep_Step_2b_20240928.R' - takes the average of input features and splits dataset with respect to sex. 

Plus two additional scripts that are loaded by script 2b as functions:
'prepare.files.R',
'get.means.R'.

After running scripts 2a and 2b, the resulting model input files (males_raw.csv/females_raw.csv) should be uploaded on the PHOTONAI platform to get brain age predictions: 

https://photon-ai.com/enigma_brainage

Once you are on the platform, make sure that you click on the correct sex group (males/females) before uploading the corresponding input file. The two output files should be renamed to 'males_raw_out' and 'females_raw_out' for males and females respectively. 

Finally, the following script was run to generate model performance metrics/plots for the current sample and to prepare dataset for downstream statistical analyses (step 4):

(2c) '2c_ENIGMA_Output_Prep_Step_2c_20240928.R'

Plus two additional scripts that loaded by script 2c as functions: 'model.fits.brainAge.R', 
'model.fits.brainAge.PE.R'.

The two ouput datasets from named as 'ALSPAC_PE_BA_ENG.RData' and 'ALSPAC_PE_BA_ENG_FI.RData' should be used for step 4. 

### Step 3. Brain age prediction using the CentileBrain Brain Age model (Yu et al., 2024)

The following R script was used to prepare model input files for the ALSPAC-PE imaging sample:

(3a) '3a_CentileBrain_Input_prep_20240928.R' -  formats column names based on the headers used by CentileBrain model ('CentileBrain_headers.csv'). 

After running script 3a, the resulting model input files (CB_males_raw.xlsx/CB_females.raw.xlsx) should be uploaded on the CentileBrain platform to get brain age predictions: 

https://centilebrain.org/#/brainAge2

Once you are on the platform, make sure that you click on the correct age group (5≤age≤40 years) and sex group (males/females) before uploading the corresponding input file. You would just just need to download 'predicted age' for each sex group (top downlead button). The two output files should be renamed to 'CB_males_raw_out' and 'CB_females_raw_out' for males and females respectively. 

Finally, the following script was run to generate model performance metrics/plots for the current sample and to prepare the dataset for downstream statistical analyses (step 4):

(3b) '3b_CentileBrain_Output_prep_20240920.R'

Plus two additional scripts loaded by script 3b as functions: 'CB.model.fits.brainAge.R', 
'CB.model.fits.brainAge.PE.R'.

The two ouput datasets named as 'ALSPAC_PE_BA_CB.RData' and 'ALSPAC_PE_BA_CB_FI.RData' should be used for step 4. 

### Step 4. Statistical analyses

The R code used for statistical analyses (incl. sample discriptives and data visualisation) can be found in the relevant script: '3_ALSPAC_PE_BA_Stats_CC_04092024.R'. 

### References

Han, L. K. M., Dinga, R., Hahn, T., Ching, C. R. K., Eyler, L. T., Aftanas, L., Aghajani, M., Aleman, A., Baune, B. T., Berger, K., Brak, I., Filho, G. B., Carballedo, A., Connolly, C. G., Couvy-Duchesne, B., Cullen, K. R., Dannlowski, U., Davey, C. G., Dima, D., Duran, F. L. S., … Schmaal, L. (2021). Brain aging in major depressive disorder: results from the ENIGMA major depressive disorder working group. Molecular psychiatry, 26(9), 5124–5139. https://doi.org/10.1038/s41380-020-0754-0

Sharp, T. H., McBride, N. S., Howell, A. E., Evans, C. J., Jones, D. K., Perry, G., Dimitriadis, S. I., Lancaster, T. M., Zuccolo, L., Relton, C., Matthews, S. M., Breeze, T., David, A. S., Drakesmith, M., Linden, D. E. J., Paus, T., & Walton, E. (2020). Population neuroimaging: generation of a comprehensive data resource within the ALSPAC pregnancy and birth cohort. Wellcome open research, 5, 203. https://doi.org/10.12688/wellcomeopenres.16060.


