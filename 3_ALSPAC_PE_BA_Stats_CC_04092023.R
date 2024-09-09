##################### Statistical analyses of Psychotic Experiences and Brain Age in ALSPAC-PE imaging sub-study (Constantinides et al., 2024) #####################
# Script Author: Constantinos Constaninides
# Written/last update on: 09/09/2024
# Project:Exploring associations between psychotic experiences and structural brain age: a population-based study in late adolescence [WORKING PAPER]

# Load relevant packages
library (ggplot2)
library(dplyr)
library(pastecs)
library(psych)
library(haven)
library(labelled)
library(sensemakr)
library(olsrr)
library(smplot2)
library(effectsize)
library(gtsummary)
library(tidyverse)

options(max.print=999999)

# Load derived dataset (incl. brain age estimates)
# setwd to working directory
load("ALSPAC_PE_BA_Dataset.RData")
data <- ALSPAC_PE_BA_Dataset
rm(ALSPAC_PE_BA_Dataset)


#Inspect dataset
## View(data)
str(data)
## remove SPSS labels from column names
var_label(data) <- NULL
head(data)
summary(data)
colnames(data)

# Select variables needed for current analyses
data <- data %>% select(SubjID, Age, Sex, PE, david_plikscat, FJPL167_new, FJPL170, FJPL172, ff5263, kz030, f8ws112, c645a, c755, c765,  MRI_YP_D1157, MRI_YP_D1158, predAge_ENG, predAge_CB_V2)  
summary(data)

# Step 1. Fix and derive variables

# Rename variables 
data <- data %>% 
  rename(PE_3Level = david_plikscat, 
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
colnames(data)

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

# Derive new variables

## Brain-PAD (outcome)
### ENIGMA_derived brain-PAD
data$devAge_ENG <- (data$predAge_ENG - data$Age)
### Absolute error of ENIGMA brain-age model (for model performance)
data$AE_ENG <- abs(data$predAge_ENG - data$Age)

### CentileBrain-derived brain-PAD, Version 2
data$devAge_CB_V2 <- (data$predAge_CB_V2 - data$Age)
### Absolute error of CentileBrain brain-age model, Version 2
data$AE_CB_V2 <- abs(data$predAge_CB_V2 - data$Age)


# Convert 3-level PE ordinal variable to a 4-level variable regardless of sleep/fever 
#  (0=none, 1 = suspected, 2 = definite, 3 = definite, clinical)
data$PE_3Level[data$PE_4Level_NASF == 3] <- 3
data <- data %>% 
  rename(PE_4Level = PE_3Level)
# convert both ordinal variables to factors
## Ordinal PE
data$PE_4Level <- as.factor(data$PE_4Level)
## ordinal PE not attributable to sleep/fever
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

# Step. 2 Inspection and exclusion of scans that failed image QC

# Participants to be excluded 
# Select those who failed image QC for cortical/sub-cortical QC (for supplementary table 1)
data_exc <- data %>% filter(CorticalQC == 2 | SubCorticalQC == 2)
summary(data_exc)
# Tabulate basic characteristics of excluded participants for table 1
data_exc %>%  
  tbl_summary(
    include = c(Age, Sex, PE_4Level),
    by = PE,
    statistic = 
      all_continuous() ~ "{mean} ({sd})",
    digits = list(all_categorical() ~ c(0, 2),
                  all_continuous() ~ c(0,2))
    )

# Exclude those who failed image QC from the current sample

## Exclude those with  failed cortical QC
data <- data %>% filter(CorticalQC != 2)
## Exclude those with failed sub-cortical QC
data <- data %>% filter(SubCorticalQC != 2)

# Step 3. Get summary statistics for demographics/other characteristics of the current sample 

# By binary PE classification (Table 1)
data %>%  
  tbl_summary(
    include = c(Age, Sex, PE_NASF, PE_Sum_NASF, PsyDis, PE_age_12_Ascert, PE_age_12, IQ_8, parSC, ME, BW),
    by = PE,
    statistic = all_continuous() ~ "{mean} ({sd})",
    digits = list(all_categorical() ~ c(0, 2),
                  all_continuous() ~ c(2,2))
    ) %>% 
 add_n() %>% # add column with total number of non-missing observations
 add_p(pvalue_fun = label_style_pvalue(digits = 2)) %>% # test for a difference between groups
 bold_p

## For sum of PE per participant, get median (Q1, Q3) (table 1)
data_NoPE <- data[which(data$PE == 0),]
data_PE <- data[which(data$PE == 1),]
summary(data_PE$PE_Sum_NASF)

# By categorical/ordinal PE classification (Table 1)
data %>%  
  tbl_summary(
    include = c(Age, Sex, PE_4Level_NASF, PE_Sum_NASF, PsyDis, PE_age_12_Ascert, PE_age_12, IQ_8, parSC, ME, BW),
    by = PE_4Level,
    statistic = all_continuous() ~ "{mean} ({sd})",
    digits = list(all_categorical() ~ c(0, 2),
                  all_continuous() ~ c(2))
  ) %>% 
  add_n() %>% # add column with total number of non-missing observations
  add_p(pvalue_fun = label_style_pvalue(digits = 2)) %>% # test for a difference between groups
  bold_p

## For sum of PEs per participant, get median (Q1, Q3) and test-difference across subgroups (table 1)
## Suspected PE
data_SuspPE <- data[which(data$PE_4Level == 1),]
summary(data_SuspPE$PE_Sum_NASF)
## Definite PE, non-clinical
data_DefPE_NC <- data[which(data$PE_4Level == 2),]
summary(data_DefPE_NC$PE_Sum_NASF)
## Definite PE, clinical 
data_DefClinPE <- data[which(data$PE_4Level == 3),]
summary(data_DefClinPE$PE_Sum_NASF)
## test-difference across subgroups
kruskal.test(PE_Sum_NASF ~ PE_4Level, data = data_PE)


# Step 4. Assess model generalization performance and age-related bias in brain age prediction in the current sample

##  Calculate performance metrics (MAE, r, R2) for Table S2

### With respect to sex (regardless of PE status)
data_M <- data[which(data$Sex == 0),]
data_F <- data[which(data$Sex == 1),]
#### MAE/RSD (coef.var) - Males
stat.desc(data_M$AE_ENG)
stat.desc(data_M$AE_CB_V2)       
#### MAE/RSD - Females
stat.desc(data_F$AE_ENG)
stat.desc(data_F$AE_CB_V2)
#### Post-hoc test for group difference 
t.test(data$AE_ENG ~ data$Sex, alternative = "two.sided")
t.test(data$AE_CB_V2 ~ data$Sex, alternative = "two.sided")
#### Pearson's R and R2 - Males
cor(data_M$Age,data_M$predAge_ENG)
cor(data_M$Age,data_M$predAge_CB_V2)
caret::R2(data_M$Age,data_M$predAge_ENG)
caret::R2(data_M$Age,data_M$predAge_CB_V2)
#### Pearson's R and R2 - Females
cor(data_F$Age,data_F$predAge_ENG)
cor(data_F$Age,data_F$predAge_CB_V2)
caret::R2(data_F$Age,data_F$predAge_ENG)
caret::R2(data_F$Age,data_F$predAge_CB_V2)

### with respect to both sex and PEs
data_M_NoPE <- data_M[which(data_M$PE == 0),]
data_M_PE <- data_M[which(data_M$PE== 1),]
data_F_NoPE <- data_F[which(data_F$PE == 0),]
data_F_PE <- data_F[which(data_F$PE== 1),]
#### without PEs
##### MAE/RSD - Males / No PEs
stat.desc(data_M_NoPE$AE_ENG)
stat.desc(data_M_NoPE$AE_CB_V2)
##### MAE/RSD - Females / No PEs
stat.desc(data_F_NoPE$AE_ENG)
stat.desc(data_F_NoPE$AE_CB_V2)
#### Post-hoc test for group difference in MAE
t.test(data_NoPE$AE_ENG ~ data_NoPE$Sex, alternative = "two.sided")
t.test(data_NoPE$AE_CB_V2 ~ data_NoPE$Sex, alternative = "two.sided")
##### Pearson's R and R2 - Males / No PEs
cor(data_M_NoPE$Age,data_M_NoPE$predAge_ENG)
cor(data_M_NoPE$Age,data_M_NoPE$predAge_CB_V2)
caret::R2(data_M_NoPE$Age,data_M_NoPE$predAge_ENG)
caret::R2(data_M_NoPE$Age,data_M_NoPE$predAge_CB_V2)
##### Pearson's R and R2 - Females / No PEs
cor(data_F_NoPE$Age,data_F_NoPE$predAge_ENG)
cor(data_F_NoPE$Age,data_F_NoPE$predAge_CB_V2)
caret::R2(data_F_NoPE$Age,data_F_NoPE$predAge_ENG)
caret::R2(data_F_NoPE$Age,data_F_NoPE$predAge_CB_V2)
#### with PEs
##### MAE/RSD - Males / PE
stat.desc(data_M_PE$AE_ENG)
stat.desc(data_M_PE$AE_CB_V2)
##### MAE/RSD - Females / PE
stat.desc(data_F_PE$AE_ENG)
stat.desc(data_F_PE$AE_CB_V2)
#### Post-hoc test for group difference in MAE
t.test(data_PE$AE_ENG ~ data_PE$Sex, alternative = "two.sided")
t.test(data_PE$AE_CB_V2 ~ data_PE$Sex, alternative = "two.sided")
##### Pearson's R and R2 - Males / PE
cor(data_M_PE$Age,data_M_PE$predAge_ENG)
cor(data_M_PE$Age,data_M_PE$predAge_CB_V2)
caret::R2(data_M_PE$Age,data_M_PE$predAge_ENG)
caret::R2(data_M_PE$Age,data_M_PE$predAge_CB_V2)
##### Pearson's R and R2 - Females / low PRS
cor(data_F_PE$Age,data_F_PE$predAge_ENG)
cor(data_F_PE$Age,data_F_PE$predAge_CB_V2)
caret::R2(data_F_PE$Age,data_F_PE$predAge_ENG)
caret::R2(data_F_PE$Age,data_F_PE$predAge_CB_V2)


# Plot predicted age for each brain age model and with respect to sex (Figure 1A)
colnames(data)
data_2 <- data %>% select(SubjID, Sex, Age, predAge_ENG, predAge_CB_V2)
data_2$Age_type <- ""
data_2_M <- data_2[which(data_2$Sex == 0),]
data_2_F <- data_2[which(data_2$Sex == 1),]
colnames(data_2_M)
data_M_Age <- data_2_M %>% select(SubjID, Sex, Age, Age_type)
data_M_Age$Age_type[data_M_Age$Age_type==""] <- "Chronological age (Males)"
data_M_predAgeENG <- data_2_M %>% select(SubjID, Sex, predAge_ENG, Age_type)
names(data_M_predAgeENG)[names(data_M_predAgeENG) == "predAge_ENG"] <- "Age"
data_M_predAgeENG$Age_type[data_M_predAgeENG$Age_type==""] <- "ENIGMA-predicted age (Males)"
data_M_predAgeCB_V2 <- data_2_M %>% select(SubjID, Sex, predAge_CB_V2, Age_type)
names(data_M_predAgeCB_V2)[names(data_M_predAgeCB_V2) == "predAge_CB_V2"] <- "Age"
data_M_predAgeCB_V2$Age_type[data_M_predAgeCB_V2$Age_type==""] <- "CentileBrain-predicted age (Males)"
colnames(data_2_F)
data_F_Age <- data_2_F%>% select(SubjID, Sex, Age, Age_type)
data_F_Age$Age_type[data_F_Age$Age_type==""] <- "Chronological age (Females)"
data_F_predAgeENG <- data_2_F %>% select(SubjID, Sex, predAge_ENG, Age_type)
names(data_F_predAgeENG)[names(data_F_predAgeENG) == "predAge_ENG"] <- "Age"
data_F_predAgeENG$Age_type[data_F_predAgeENG$Age_type==""] <- "ENIGMA-predicted age (Females)"
data_F_predAgeCB_V2 <- data_2_F %>% select(SubjID, Sex, predAge_CB_V2, Age_type)
names(data_F_predAgeCB_V2)[names(data_F_predAgeCB_V2) == "predAge_CB_V2"] <- "Age"
data_F_predAgeCB_V2$Age_type[data_F_predAgeCB_V2$Age_type==""] <- "CentileBrain-predicted age (Females)"

data_plotB1 <- rbind(data_M_Age, data_M_predAgeENG)
data_plotB1 <- rbind(data_plotB1, data_M_predAgeCB_V2)
data_plotB1 <- rbind(data_plotB1, data_F_Age)
data_plotB1 <- rbind(data_plotB1, data_F_predAgeENG)
data_plotB1 <- rbind(data_plotB1, data_F_predAgeCB_V2)

ggplot(data_plotB1, aes(x = Age, color = Age_type, fill = Age_type)) + 
  geom_density(alpha = 0.2) +
  theme_bw() + theme(axis.text=element_text(size=9),
                     axis.title=element_text(size=11),legend.title=element_blank(),
                     legend.text = element_text(size=9),legend.position="bottom")


# Plot brain-predicted age versus chronological age 
## CentileBrain-V2
### with respect to sex (Figure 1B)
Sex_ <- factor(data$Sex, labels = c("Males", "Females"))
ggplot(data, aes(x=Age, y=predAge_CB_V2, color=Sex_, group=Sex_)) + 
  # geom_hline(yintercept=0 ,linetype=2) +
  geom_point(alpha=0.5, size=2) + 
  scale_y_continuous(limits=(c(7,42))) +
  geom_abline(a=0, b=1, color='black') +
  #geom_smooth(method=lm, colour="black", size=0.5, se=FALSE, fullrange=TRUE, linetype="dashed") +
  geom_smooth(aes(fill=Sex_), method=lm, size=0.5, se=TRUE, fullrange=FALSE, linetype="dashed") +
  # scale_color_manual(labels = c("Males", "Females"), values = c("blue", "red"), name = "Sex") +
  labs(x = "Chronological age", y = "CentileBrain-predicted age") +
  theme_bw()
cor.test(data$Age, data$predAge_CB_V2)
# Repeat with respect to PE status (S.Figure S1A)
PE_status <- factor(data$PE, labels = c("No PE", "PE"))
ggplot(data, aes(x=Age, y=predAge_CB_V2, color=PE_status, group=PE_status)) + 
  # geom_hline(yintercept=0 ,linetype=2) +
  geom_point(alpha=0.5, size=2) + 
  #scale_y_continuous(limits=(c(7,42))) +
  scale_y_continuous(limits=(c(7,42))) +
  geom_abline(a=0, b=1, color='black') +
  #geom_smooth(method=lm, colour="black", size=0.5, se=FALSE, fullrange=TRUE, linetype="dashed") +
  geom_smooth(aes(fill=PE_status), method=lm, size=0.5, se=TRUE, fullrange=FALSE, linetype="dashed") +
  scale_color_manual(values = c("#009E73", "#D55E00")) +
  scale_fill_manual(values = c("#009E73", "#D55E00")) +
  labs(x = "Chronological age", y = "CentileBrain-predicted age") +
  theme_bw()

## ENIGMA
### with respect to sex (Figure S1, C)
Sex_ <- factor(data$Sex, labels = c("Males", "Females"))
ggplot(data, aes(x=Age, y=predAge_ENG, color=Sex_, group=Sex_)) + 
  # geom_hline(yintercept=0 ,linetype=2) +
  geom_abline(a=0, b=1, color='black') +
  geom_point(alpha=0.5, size=2) + 
  scale_y_continuous(limits=(c(7,42))) +
  # geom_smooth(method=lm, colour="black", size=0.5, se=FALSE, fullrange=TRUE) +
  geom_smooth(aes(fill=Sex_), method=lm, size=0.5, se=TRUE, fullrange=FALSE, linetype="dashed") +
  # scale_color_manual(labels = c("Males", "Females"), values = c("blue", "red"), name = "Sex") +
  labs(x = "Chronological age", y = "ENIGMA-predicted age") +
  theme_bw()
cor.test(data$Age, data$predAge_ENG)

### Repeat with respect PE status (Figure S1, B)
PE_status <- factor(data$PE, labels = c("No PE", "PE"))
ggplot(data, aes(x=Age, y=predAge_ENG, color=PE_status, group=PE_status)) + 
  # geom_hline(yintercept=0 ,linetype=2) +
  geom_point(alpha=0.5, size=2) + 
  scale_y_continuous(limits=(c(7,42))) +
  geom_abline(a=0, b=1, color='black') +
  #geom_smooth(method=lm, colour="black", size=0.5, se=FALSE, fullrange=TRUE, linetype="dashed") +
  geom_smooth(aes(fill=PE_status), method=lm, size=0.5, se=TRUE, fullrange=FALSE, linetype="dashed") +
  scale_color_manual(values = c("#009E73", "#D55E00")) +
  scale_fill_manual(values = c("#009E73", "#D55E00")) +
  labs(x = "Chronological age", y = "ENIGMA-predicted age") +
  theme_bw()


# Assess presence of age-related bias in brain-PAD with respect to PEs (SFigure 2 A and B)

# CentileBrain-derived brain-PAD, Version 2 (Figure S2, A)
ggplot(data, aes(x=Age, y=devAge_CB_V2, color=as.factor(PE))) + 
  geom_point(alpha=0.5, size=2) + 
  geom_hline(yintercept=0 ,linetype=2) +
  scale_y_continuous(limits=(c(-10,10))) +
  geom_smooth(method=lm, colour="black", size=0.5, se=FALSE, fullrange=TRUE) +
  geom_smooth(aes(group=PE), method=lm, size=0.5, se=FALSE, fullrange=TRUE, linetype="dashed") +
  scale_color_manual(labels = c("No PE", "PE"), values = c("#009E73", "#D55E00"), name = "PE status") +
  labs(x = "Chronological age", y = "CentileBrain-derived brain-PAD") +
  theme_bw()
cor.test(data$devAge_CB_V2, data$Age)

# ENIGMA-derived brain-PAD (S.Figure S2, B)
ggplot(data, aes(x=Age, y=devAge_ENG, color=as.factor(PE))) + 
  geom_hline(yintercept=0 ,linetype=2) +
  geom_point(alpha=0.5, size=2) + 
  scale_y_continuous(limits=(c(-20,25))) +
  geom_smooth(method=lm, colour="black", size=0.5, se=FALSE, fullrange=TRUE) +
  geom_smooth(aes(group=PE), method=lm, size=0.5, se=FALSE, fullrange=TRUE, linetype="dashed") +
  scale_color_manual(labels = c("No PE", "PE"), values = c("#009E73", "#D55E00"), name = "PE status") +
  labs(x = "Chronological age", y = "ENIGMA-derived brain-PAD") +
  theme_bw()
cor.test(data$devAge_ENG, data$Age)


# Residualize Brain-PAD for age to illustrate age-bias correction (Suppl. Figure S2, C)

# CentileBrain-derived brain-PAD-V2 residualised for age (S.Figure S2, C)
devAge_CB_V2_Age = lm(devAge_CB_V2 ~ Age, data=data)
data$devAge_CB_V2_resAge=resid(devAge_CB_V2_Age)
ggplot(data, aes(x=Age, y=devAge_CB_V2_resAge, color=as.factor(PE))) + 
  geom_hline(yintercept=0 ,linetype=2) +
  geom_point(alpha=0.5, size=2) + 
  scale_y_continuous(limits=(c(-10,10))) +
  geom_smooth(method=lm, colour="black", size=0.5, se=FALSE, fullrange=TRUE) +
  geom_smooth(aes(group=PE), method=lm, size=0.5, se=FALSE, fullrange=TRUE, linetype="dashed") +
  scale_color_manual(labels = c("NoPE", "PE"), values = c("#009E73", "#D55E00"), name = "PE status") +
  labs(x = "Chronological age", y = "CentileBrain-derived brain-PAD resid. for age") +
  theme_bw() + theme(axis.text=element_text(size=8),
                     axis.title=element_text(size=8), legend.title = element_text(size = 10),
                     legend.text = element_text(size = 10))
cor.test(data$devAge_CB_V2_resAge, data$Age)

# ENIGMA-derived brain-PAD residualised for age (Suppl.Figure S2, D)
devAge_ENG_Age = lm(devAge_ENG ~ Age, data=data)
data$devAge_ENG_resAge = resid(devAge_ENG_Age)
ggplot(data, aes(x=Age, y=devAge_ENG_resAge, color=as.factor(PE))) + 
  geom_hline(yintercept=0 ,linetype=2) +
  geom_point(alpha=0.5, size=2) + 
  scale_y_continuous(limits=(c(-20,25))) +
  geom_smooth(method=lm, colour="black", size=0.5, se=FALSE, fullrange=TRUE) +
  geom_smooth(aes(group=PE), method=lm, size=0.5, se=FALSE, fullrange=TRUE, linetype="dashed") +
  scale_color_manual(labels = c("No PE", "PES"), values = c("#009E73", "#D55E00"), name = "PE status") +
  labs(x = "Chronological age", y = "ENIGMA-derived brain-PAD resid. for age") +
  theme_bw() + theme(axis.text=element_text(size=9),
                     axis.title=element_text(size=9), legend.title = element_text(size = 9),
                     legend.text = element_text(size = 9))
cor.test(data$devAge_ENG_resAge, data$Age)


# Assess correlation between ENIGMA-derived brain-PAD and Centile-derived brain-PAD-V2 (S.Figure S3, left)
ggplot(data, aes(x=devAge_CB_V2,y=devAge_ENG, color=as.factor(PE))) + 
  geom_point(alpha=0.5, size=2) + 
  geom_hline(yintercept=0, linetype=2) +
  geom_smooth(method=lm, colour="black", size=0.5, se=FALSE, fullrange=TRUE) +
  scale_color_manual(labels = c("None", "PE"), values = c("#009E73", "#D55E00"), name = "PE status") +
  labs(x = "CentileBrain-derived brain-PAD", y = "ENIGMA-derived brain-PAD") +
  theme_bw() + theme(axis.text=element_text(size=8),
                    axis.title=element_text(size=8), legend.title = element_text(size = 10),
                    legend.text = element_text(size = 10))
cor.test (data$devAge_ENG, data$devAge_CB_V2)
cor.test (data$devAge_ENG, data$devAge_CB_V2, method=c("spearman"))
##  repeated with age-residualised brain-PAD(S.Figure S3, right)
ggplot(data, aes(x=devAge_CB_V2_resAge, y=devAge_ENG_resAge, color=as.factor(PE))) + 
  geom_point(alpha=0.5, size=2) + 
  geom_hline(yintercept=0, linetype=2) +
  geom_smooth(method=lm, colour="black", size=0.5, se=FALSE, fullrange=TRUE) +
  scale_color_manual(labels = c("None", "PE"), values = c("#009E73", "#D55E00"), name = "PE status") +
  labs(x = "CentileBrain-derived brain-PAD resid. for age", y = "ENIGMA-derived brain-PAD resid. for age") +
  theme_bw() + theme(axis.text=element_text(size=8),
                     axis.title=element_text(size=8), legend.title = element_text(size = 10),
                     legend.text = element_text(size = 10))
cor.test (data$devAge_CB_V2_resAge, data$devAge_ENG_resAge)
cor.test (data$devAge_CB_V2_resAge, data$devAge_ENG_resAge, method=c("spearman"))



# Step 5. Multiple regression for the effect of PEs on brain-PAD

## Step 5a Run primary models for difference in brain-PAD between low and high SCZ-PRS (primary analyses / Table B2)

### CentileBrain-brain-PAD (outcome)
#### Binary PE classification (Table S2)
DevAge_PE_Age_Sex_CB_V2=lm(devAge_CB_V2 ~ as.factor(PE) + as.factor(Sex) + Age, data = data)
summary(DevAge_PE_Age_Sex_CB_V2)
confint(DevAge_PE_Age_Sex_CB_V2)
d_CB_V2 <- (-1.591)*(115+117)/(sqrt(115*117)*sqrt(228))
d.ci(d_CB_V2,n1=115,n2=117)
##### check assumptions of linear regression
ols_plot_resid_qq(DevAge_PE_Age_Sex_CB_V2)
ols_test_normality(DevAge_PE_Age_Sex_CB_V2)
ols_test_correlation(DevAge_PE_Age_Sex_CB_V2)
ols_plot_resid_fit(DevAge_PE_Age_Sex_CB_V2)
ols_plot_resid_hist(DevAge_PE_Age_Sex_CB_V2)


##### Raincloud plot for CentileBrain-derived brain_PAD in those with without and with PE, residualised for age and sex (Figure 2, A)
devAge_CB_V2_Age_Sex = lm(devAge_CB_V2 ~ Age + Sex, data=data)
data$devAge_CB_V2_resAgeSex = resid(devAge_CB_V2_Age_Sex)
Group <- factor(data$PE, labels = c("Without PE", "with PE"))
ggplot(data = data, mapping = aes(x = "", y = devAge_CB_V2_resAgeSex, fill = Group,
                                  color = Group)) +
  sm_raincloud(sep_level = 2, point.params = list(size = 3, shape = 21,
                                                  color = 'transparent', alpha = 0.4)) +
  scale_fill_manual(labels = c("No PE", "PE"), values = c("#009E73","#D55E00"), name = "PE status") +
  labs(x = "", y = "brain-PAD res. for age and sex") +
  geom_hline(yintercept=0 ,linetype=2) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(size=12),
                     axis.title=element_text(size=12), legend.title = element_text(size = 12),
                     legend.text = element_text(size = 12))

####  Repeat with ordinal PE classification (Table S2)
DevAge_PE_Ordinal_Age_Sex_CB_V2=lm(devAge_CB_V2 ~ as.numeric(PE_4Level) + as.factor(Sex) + Age, data = data)
summary(DevAge_PE_Ordinal_Age_Sex_CB_V2)
confint(DevAge_PE_Ordinal_Age_Sex_CB_V2)
partial_r2(DevAge_PE_Ordinal_Age_Sex_CB_V2)
##### check assumptions of linear regression
ols_plot_resid_qq(DevAge_PE_Ordinal_Age_Sex_CB_V2)
ols_test_normality(DevAge_PE_Ordinal_Age_Sex_CB_V2)
ols_test_correlation(DevAge_PE_Ordinal_Age_Sex_CB_V2)
ols_plot_resid_fit(DevAge_PE_Ordinal_Age_Sex_CB_V2)
ols_plot_resid_hist(DevAge_PE_Ordinal_Age_Sex_CB_V2)

#### Specify as nominal (for Supplemetary Figure S4)
DevAge_PE_Nom_Age_Sex_CB_V2=lm(devAge_CB_V2 ~ as.factor(PE_4Level) + as.factor(Sex) + Age, data = data)
summary(DevAge_PE_Nom_Age_Sex_CB_V2)
confint(DevAge_PE_Nom_Age_Sex_CB_V2)
partial_r2(DevAge_PE_Nom_Age_Sex_CB_V2)
d_CB_V2_S  <- (-0.840)*(115+41)/(sqrt(115*41)*sqrt(226))
d_CB_V2_S
d.ci(d_CB_V2_S,n1=115,n2=41)
d_CB_V2_NC <- (-1.553)*(115+46)/(sqrt(115*46)*sqrt(226))
d.ci(d_CB_V2_NC,n1=115,n2=46)
d_CB_V2_C  <- (-0.870)*(115+30)/(sqrt(115*30)*sqrt(226))
d.ci(d_CB_V2_C,n1=115,n2=30)

# Raincloud plot for ENIGMA-derived brain_PAD across PE groups, residualised for age and sex (Figure 2, B)
Group_ordinal <- factor(data$PE_4Level, labels = c("None", "Suspected", "Definite", "Definite, clinical"))
ggplot(data = data, mapping = aes(x=Group_ordinal, y=devAge_CB_V2_resAgeSex, fill= Group_ordinal)) +
  sm_raincloud(sep_level = 1, point.params = list(size = 3, shape = 21,
                                                  color = 'transparent', alpha = 0.4)) +
  geom_hline(yintercept=0 ,linetype=2) +
  labs(x = "", y = "Brain-PAD resid. for age and sex") +
  scale_fill_manual(labels = c("None", "Suspected", "Definite", "Definite, clinical"), values = c("#009E73","goldenrod1", "darkorange2", "firebrick3"), name = "PE status") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(size=12),
                     axis.title=element_text(size=12), legend.title = element_text(size = 12),
                     legend.text = element_text(size = 12), legend.position="none")

### Repeat with ENIGMA-derived brain-PAD (outcome)
#### Binary PE classification (Table S3)
DevAge_PE_Age_Sex=lm(devAge_ENG ~ as.factor(PE) + as.factor(Sex) + Age, data = data)
summary(DevAge_PE_Age_Sex)
confint(DevAge_PE_Age_Sex)
# d <-t.val*(n1+n2)/(sqrt(n1*n2)*sqrt(df))
d_ENG <- (0.625)*(115+117)/(sqrt(115*117)*sqrt(228))
d.ci(d_ENG,n1=115,n2=117)
##### check assumptions of linear regression
ols_plot_resid_qq(DevAge_PE_Age_Sex)
ols_test_normality(DevAge_PE_Age_Sex)
ols_test_correlation(DevAge_PE_Age_Sex)
ols_plot_resid_fit(DevAge_PE_Age_Sex)
ols_plot_resid_hist(DevAge_PE_Age_Sex)

##### Raincloud plot for ENIGMA-derived brain_PAD in PE vs. without PEs, residualised for age and sex (Suppl. Figure S5, A)
devAge_ENG_Age_Sex = lm(devAge_ENG ~ Age + Sex, data=data)
data$devAge_ENG_resAgeSex = resid(devAge_ENG_Age_Sex)
ggplot(data = data, mapping = aes(x = "", y = devAge_ENG_resAgeSex, fill = Group,
                                  color = Group)) +
  sm_raincloud(sep_level = 2, point.params = list(size = 3, shape = 21,
                                                  color = 'transparent', alpha = 0.4)) +
  scale_fill_manual(labels = c("No PE", "PE"), values = c("#009E73","#D55E00"), name = "PE status") +
  labs(x = "", y = "Brain-PAD res. for age and sex") +
  geom_hline(yintercept=0 ,linetype=2) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(size=12),
                     axis.title=element_text(size=12), legend.title = element_text(size = 12),
                     legend.text = element_text(size = 12))

#### Repeat with ordinal PE classification (Table S3)
DevAge_PE_Ordinal_Age_Sex=lm(devAge_ENG ~ as.numeric(PE_4Level) + as.factor(Sex) + Age, data = data)
summary(DevAge_PE_Ordinal_Age_Sex)
confint(DevAge_PE_Ordinal_Age_Sex)
partial_r2(DevAge_PE_Ordinal_Age_Sex)
# check assumptions of linear regression
ols_plot_resid_qq(DevAge_PE_Ordinal_Age_Sex)
ols_test_normality(DevAge_PE_Ordinal_Age_Sex)
ols_test_correlation(DevAge_PE_Ordinal_Age_Sex)
ols_plot_resid_fit(DevAge_PE_Ordinal_Age_Sex)
ols_plot_resid_hist(DevAge_PE_Ordinal_Age_Sex)

####  Re-run as nominal (For Supl. Figure S4)
DevAge_PE_Nom_Age_Sex=lm(devAge_ENG ~ as.factor(PE_4Level) + as.factor(Sex) + Age, data = data)
summary(DevAge_PE_Nom_Age_Sex)
confint(DevAge_PE_Nom_Age_Sex)
partial_r2(DevAge_PE_Nom_Age_Sex)
d_ENG_S <- (0.844)*(115+41)/(sqrt(115*41)*sqrt(226))
d.ci(d_ENG_S,n1=115,n2=41)
d_ENG_NC <- (0.200)*(115+46)/(sqrt(115*46)*sqrt(226))
d.ci(d_ENG_NC,n1=115,n2=46)
d_ENG_C <- (0.211)*(115+30)/(sqrt(115*30)*sqrt(226))
d.ci(d_ENG_NC,n1=115,n2=30)

# Raincloud plot for ENIGMA-derived brain_PAD across PE groups, residualised for age and sex (Figure S5, B)
ggplot(data = data, mapping = aes(x=Group_ordinal, y=devAge_ENG_resAgeSex, fill= Group_ordinal)) +
  sm_raincloud(sep_level = 1, point.params = list(size = 3, shape = 21,
                                                  color = 'transparent', alpha = 0.4)) +
  geom_hline(yintercept=0 ,linetype=2) +
  labs(x = "", y = " Brain-PAD resid. for age and sex") +
  scale_fill_manual(labels = c("None", "Suspected", "Definite", "Definite, Clinical"), values = c("#009E73","goldenrod1", "darkorange2", "firebrick3"), name = "PE status") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text=element_text(size=12),
                     axis.title=element_text(size=12), legend.title = element_text(size = 12),
                     legend.text = element_text(size = 12), legend.position="none")



## Step 5b. Sensitivity analyses 
### 1. Re-run analyses after accounting for attributed to sleep and fever 
#### CentileBrain-dervive brain-PAD-V2
##### Binary
DevAge_PE_Age_Sex_CB_V2_nsf=lm(devAge_CB_V2 ~ as.factor(PE_NASF) + as.factor(Sex) + Age, data = data)
summary(DevAge_PE_Age_Sex_CB_V2_nsf)
confint(DevAge_PE_Age_Sex_CB_V2_nsf)
partial_r2(DevAge_PE_Age_Sex_CB_V2_nsf)
d_CB_V2_nsf  <- (-1.139)*(131+101)/(sqrt(131*101)*sqrt(228))
d_CB_V2_nsf
d.ci(d_CB_V2_nsf,n1=131,n2=101)
#####Ordinal 
DevAge_PE_ordinal_Age_Sex_CB_V2_nsf=lm(devAge_CB_V2 ~ as.numeric(PE_4Level_NASF) + as.factor(Sex) + Age, data = data)
summary(DevAge_PE_ordinal_Age_Sex_CB_V2_nsf)
confint(DevAge_PE_ordinal_Age_Sex_CB_V2_nsf)
partial_r2(DevAge_PE_ordinal_Age_Sex_CB_V2_nsf)
# Nominal
DevAge_PE_Nominal_Age_Sex_CB_V2_nsf=lm(devAge_CB_V2 ~ as.factor(PE_4Level_NASF) + as.factor(Sex) + Age, data = data)
summary(DevAge_PE_Nominal_Age_Sex_CB_V2_nsf)
confint(DevAge_PE_Nominal_Age_Sex_CB_V2_nsf)
partial_r2(DevAge_PE_Nominal_Age_Sex_CB_V2_nsf)
d_CB_V2_S_nsf  <- (-0.249)*(131+38)/(sqrt(131*38)*sqrt(226))
d_CB_V2_S_nsf  
d_CB_V2_NC_nsf <- (-1.430)*(131+33)/(sqrt(131*33)*sqrt(226))
d_CB_V2_NC_nsf
d_CB_V2_C_nsf <- (-0.689)*(131+30)/(sqrt(131*30)*sqrt(226))
d_CB_V2_C_nsf

#### ENIGMA-derived brain-PAD
##### Binary
DevAge_PE_Age_Sex_nsf=lm(devAge_ENG ~ as.factor(PE_NASF) + as.factor(Sex) + Age, data = data)
summary(DevAge_PE_Age_Sex_nsf)
confint(DevAge_PE_Age_Sex_nsf)
partial_r2(DevAge_PE_Age_Sex_nsf)
d_ENG_nsf  <- (0.653)*(131+101)/(sqrt(131*101)*sqrt(228))
d.ci(d_ENG_nsf,n1=131,n2=101)
##### Ordinal 
DevAge_PE_ordinal_Age_Sex_nsf=lm(devAge_ENG ~ as.numeric(PE_4Level_NASF) + as.factor(Sex) + Age, data = data)
summary(DevAge_PE_ordinal_Age_Sex_nsf)
confint(DevAge_PE_ordinal_Age_Sex_nsf)
partial_r2(DevAge_PE_ordinal_Age_Sex_nsf)
##### Nominal
DevAge_PE_Nominal_Age_Sex_nsf=lm(devAge_ENG ~ as.factor(PE_4Level_NASF) + as.factor(Sex) + Age, data = data)
summary(DevAge_PE_Nominal_Age_Sex_nsf)
confint(DevAge_PE_Nominal_Age_Sex_nsf)
partial_r2(DevAge_PE_Nominal_Age_Sex_nsf)
d_ENG_S_nsf  <- (1.112)*(131+38)/(sqrt(131*38)*sqrt(226))
d_ENG_S_nsf
d_ENG_NC_nsf <- (-0.082)*(131+33)/(sqrt(131*33)*sqrt(226))
d_ENG_NC_nsf 
d_ENG_C_nsf  <- (0.190)*(131+30)/(sqrt(131*30)*sqrt(226))
d_ENG_C_nsf 


### 2. Exclusion of outliers 

#### Check for outliers in CentileBrain-derived brain-PAD-V2 (outcome)
##### Plot distribution of brian-PAD across the total sample
ggplot(data, aes(x=devAge_CB_V2)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="blue") +
  labs(x = "CentileBrain-derived brain-PAD-V2")
##### Now, Standardize brain-PAD across the the total sample
data$devAge_std_CB_V2 <- data$devAge_CB_V2
data <- data %>% mutate_at(c('devAge_std_CB_V2'), ~(scale(.) %>% as.vector))
##### Plot standardized brain-PAD across the the total sample
ggplot(data, aes(x=devAge_std_CB_V2)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="blue") +
  labs(x = "standardised CentileBrain-derived brain-PAD-V2")
##### Two positive outlier (> 3.00 SD) detected. Exclude: 
data_CB_V2_wout <- data %>% filter(devAge_std_CB_V2 < 3.00)
summary(data_CB_V2_wout)
##### Run model with binary PE classification
DevAge_PE_Age_Sex_CB_V2_wout=lm(devAge_CB_V2 ~ as.factor(PE) + as.factor(Sex) + Age, data = data_CB_V2_wout)
summary(DevAge_PE_Age_Sex_CB_V2_wout)
confint(DevAge_PE_Age_Sex_CB_V2_wout)
partial_r2(DevAge_PE_Age_Sex_CB_V2_wout)
d_CB_V2_wout <- (-1.606)*(114+116)/(sqrt(114*116)*sqrt(226))
d_CB_V2_wout
d.ci(-0.2136671,n1=114,n2=116)
##### Repeat with ordinal PE classification 
DevAge_PE_ordinal_Age_Sex_CB_V2_wout=lm(devAge_CB_V2 ~ as.numeric(PE_4Level) + as.factor(Sex) + Age, data = data_CB_V2_wout)
summary(DevAge_PE_ordinal_Age_Sex_CB_V2_wout)
confint(DevAge_PE_ordinal_Age_Sex_CB_V2_wout)
partial_r2(DevAge_PE_ordinal_Age_Sex_CB_V2_wout)
# ##### Repeat with nominal specification 
DevAge_PE_Nominal_Age_Sex_CB_V2_wout=lm(devAge_CB_V2 ~ as.factor(PE_4Level) + as.factor(Sex) + Age, data = data_CB_V2_wout)
summary(DevAge_PE_Nominal_Age_Sex_CB_V2_wout)
confint(DevAge_PE_Nominal_Age_Sex_CB_V2_wout)
partial_r2(DevAge_PE_Nominal_Age_Sex_CB_V2_wout)
d_CB_V2_S_wout  <- (-0.785)*(114+41)/(sqrt(114*41)*sqrt(224))
d.ci(d_CB_V2_S_wout,n1=114,n2=41)
d_CB_V2_NC_wout <- (-1.386)*(114+46)/(sqrt(114*46)*sqrt(224))
d.ci(d_CB_V2_NC_wout,n1=114,n2=46)
d_CB_V2_C_wout <- (-1.205)*(114+29)/(sqrt(114*29)*sqrt(224))
d.ci(d_CB_V2_C_wout,n1=114,n2=29)


#### Check for outliers in ENIGMA-Brain-PAD (i.e., above or below 3.00 SD from the mean across the total sample)
##### plot distribution of brian-PAD across the total sample
ggplot(data, aes(x=devAge_ENG)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="blue") +
  labs(x = "ENIGMA-derived brain-PAD")
##### Now, Standardize brain-PAD across the total sample
data$devAge_std_ENG <- data$devAge_ENG
data <- data %>% mutate_at(c('devAge_std_ENG'), ~(scale(.) %>% as.vector))
##### Plot standardized brain-PAD across the total sample
ggplot(data, aes(x=devAge_std_ENG)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="blue") +
  labs(x = "standardised ENIGMA-derived brain-PAD")
##### No outliers detected


## Step 5c. Exploratory analyses for effects of recurring/transient PEs (Table S5)
#####  CentileBrain-derived V2 brain-PAD
DevAge_PE_Re_Age_Sex_CB_V2=lm(devAge_CB_V2 ~ as.factor(PE_18_12_R) + as.factor(Sex) + Age, data = data)
summary(DevAge_PE_Re_Age_Sex_CB_V2)
confint(DevAge_PE_Re_Age_Sex_CB_V2)
partial_r2(DevAge_PE_Re_Age_Sex_CB_V2)
d_CB_V2_T <- (-1.273)*(93+73)/(sqrt(93*73)*sqrt(197))
d_CB_V2_T
d.ci(-0.1827261,n1=93,n2=73)
d_CB_V2_R<- (-1.102)*(93+36)/(sqrt(93*36)*sqrt(197))
d_CB_V2_R
d.ci(-0.1750433,n1=93,n2=36)
###### Repeat after excluding outliers
DevAge_PE_Re_Age_Sex_CB_V2=lm(devAge_CB_V2 ~ as.factor(PE_18_12_R) + as.factor(Sex) + Age, data = data_CB_V2_wout)
summary(DevAge_PE_Re_Age_Sex_CB_V2)
confint(DevAge_PE_Re_Age_Sex_CB_V2)
partial_r2(DevAge_PE_Re_Age_Sex_CB_V2)
d_CB_V2_T_wout <- (-1.062)*(92+73)/(sqrt(92*73)*sqrt(196))
d_CB_V2_T_wout
d.ci(-0.1527303,n1=92,n2=73)
d_CB_V2_R_wout <- (-0.932)*(92+36)/(sqrt(92*36)*sqrt(196))
d_CB_V2_R_wout
d.ci(-0.1480651,n1=92,n2=36)


##### ENIGMA-derived brain-PAD
DevAge_PE_Re_Age_Sex=lm(devAge_ENG ~ as.factor(PE_18_12_R) + as.factor(Sex) + Age, data = data)
summary(DevAge_PE_Re_Age_Sex)
confint(DevAge_PE_Re_Age_Sex)
partial_r2(DevAge_PE_Re_Age_Sex)
d_ENG_T <- (-0.621)*(93+73)/(sqrt(93*73)*sqrt(197))
d_ENG_T 
d.ci(-0.08913816,n1=93,n2=73)
d_ENG_R <- (-0.230)*(93+36)/(sqrt(93*36)*sqrt(197))
d_ENG_R
d.ci(-0.03653354,n1=93,n2=36)



## Step 5d - Additional sensitivity analyses co-varying for IQ, BW, ME or ParSC (Suppl. Table S7)

#### CentileBrain-derived brain-PAD - Version 2

##### Additional adjustment of primary analyses for IQ at age 8
###### binary PE classification
DevAge_CB_IQ=lm(devAge_CB_V2 ~  Age + as.factor(Sex) + IQ_8 + as.factor(PE), data = data)
summary(DevAge_CB_IQ)
###### Ordinal PE classification
DevAge_CB_IQ=lm(devAge_CB_V2 ~  Age + as.factor(Sex) + IQ_8 + as.numeric(PE_4Level), data = data)
summary(DevAge_CB_IQ)

##### Additional adjustment of primary analyses for birth weight
###### binary PE classification
DevAge_CB_BW=lm(devAge_CB_V2 ~  Age + as.factor(Sex) + BW + as.factor(PE), data = data)
summary(DevAge_CB_BW)
confint(DevAge_CB_BW)
d_CB_V2 <- (-2.089)*(107+111)/(sqrt(107*111)*sqrt(213))
d.ci(d_CB_V2,n1=107,n2=111)
###### Ordinal PE classification
DevAge_CB_BW=lm(devAge_CB_V2 ~  Age + as.factor(Sex) + BW + as.numeric(PE_4Level), data = data)
summary(DevAge_CB_BW)
confint(DevAge_CB_BW)
partial_r2(DevAge_CB_BW)

##### Additional adjustment of primary analyses for maternal education

###### binary PE classification
DevAge_CB_ME=lm(devAge_CB_V2 ~  Age + as.factor(Sex) +  as.numeric(ME) + as.factor (PE), data = data)
summary(DevAge_CB_ME)
###### Ordinal PE classification
DevAge_CB_ME=lm(devAge_CB_V2 ~  Age + as.factor(Sex) +  as.numeric(ME) + as.numeric (PE_4Level), data = data)
summary(DevAge_CB_ME)

##### Additional adjustment of primary analyses for parental social class
###### binary PE classification
DevAge_CB_ParSC=lm(devAge_CB_V2 ~  Age + as.factor(Sex) +  as.factor(parSC) + as.factor(PE), data = data)
summary(DevAge_CB_ParSC)
confint(DevAge_CB_ParSC)
d_CB_V2 <- (-2.168)*(104+102)/(sqrt(104*102)*sqrt(201))
d_CB_V2
d.ci(d_CB_V2,n1=104,n2=102)
###### Ordinal PE classification
DevAge_CB_ParSC=lm(devAge_CB_V2 ~  Age + as.factor(Sex) +  as.factor(parSC) + as.numeric(PE_4Level), data = data)
summary(DevAge_CB_ParSC)
confint(DevAge_CB_ParSC)
partial_r2(DevAge_CB_ParSC)


#### ENIGMA-derived brain-PAD (Table S7)

##### Additional adjustment of primary analyses for IQ at age 8
###### binary PE classification
DevAge_ENG_IQ=lm(devAge_ENG ~  Age + as.factor(Sex) + IQ_8 + as.factor(PE), data = data)
summary(DevAge_ENG_IQ)
###### Ordinal PE classification
DevAge_ENG_IQ=lm(devAge_ENG ~  Age + as.factor(Sex) + IQ_8 + as.numeric(PE_4Level), data = data)
summary(DevAge_ENG_IQ)

DevAge_ENG_BW=lm(devAge_ENG ~  Age + as.factor(Sex) + as.factor(PE) + BW, data = data)
summary(DevAge_ENG_BW)

DevAge_ENG_BW=lm(devAge_ENG ~  Age + as.factor(Sex) + as.numeric(PE_4) + BW, data = data)
summary(DevAge_ENG_BW)

##### Additional adjustment of primary analyses for birth weight
###### binary PE classification
DevAge_ENG_BW=lm(devAge_ENG ~  Age + as.factor(Sex) + as.factor(PE) + BW, data = data)
summary(DevAge_ENG_BW)
###### Ordinal PE classification
DevAge_ENG_BW=lm(devAge_ENG ~  Age + as.factor(Sex) + as.numeric(PE_4Level) + BW, data = data)
summary(DevAge_ENG_BW)

##### Additional adjustment of primary analyses for maternal education
###### binary PE classification
DevAge_ENG_ME=lm(devAge_ENG ~  Age + as.factor(Sex) + as.factor(PE) + as.numeric(ME), data = data)
summary(DevAge_ENG_ME)
###### Ordinal PE classification
DevAge_ENG_MEa=lm(devAge_ENG ~  Age + as.factor(Sex) +  as.numeric(ME) + as.numeric(PE_4Level), data = data)
summary(DevAge_ENG_ME)

##### Additional adjustment of primary analyses for parental social class
###### binary PE classification
DevAge_CB_ParSC=lm(devAge_ENG ~ Age + as.factor(Sex) +  as.factor(parSC) + as.factor(PE), data = data)
summary(DevAge_ENG_ParSC)
###### Ordinal PE classification
DevAge_CB_ParSC=lm(devAge_ENG ~  Age + as.factor(Sex) +  as.factor(parSC) + as.numeric(PE_4Level), data = data)
summary(DevAge_ENG_ParSC)


## END ## 
