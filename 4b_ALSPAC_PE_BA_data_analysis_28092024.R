# Project: Exploring associations between psychotic experiences and structural brain age: a population-based study in late adolescence [WORKING PAPER]
# Script 4b: Data visualization and statistical analyses - step 4b
# Description: Generates descriptives, plots, and results from multiple regression analyses to populate the manuscript
# Written by: Constantinos Constaninides
# Written/last update on: 28/09/2024

# Load relevant packages
library (ggplot2)
library(dplyr)
library(pastecs)
library(psych)
library(sensemakr)
library(olsrr)
library(smplot2)
library(effectsize)
library(gtsummary)
# library(tidyverse)

# Load final dataset (as prepared by script 4a) 
load ('ALSPAC_PE_BA_FINAL.RData')

# Step 3. Get summary statistics for demographics/other characteristics of the current sample 

# By binary PE classification (Table 1)
data %>%  
  tbl_summary(
    include = c(Age, Sex, PE_NASF, PsyDis, PE_age_12_Ascert, PE_age_12, IQ_8, parSC, ME, BW),
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
    include = c(Age, Sex, PE_4Level_NASF, PsyDis, PE_age_12_Ascert, PE_age_12, IQ_8, parSC, ME, BW),
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
## test difference across subgroups
kruskal.test(PE_Sum_NASF ~ PE_4Level, data = data_PE)


# Step 4. Assess model generalization performance and age-related bias in brain age prediction in the current sample
# Plot predicted age for each brain age model and with respect to sex (Figure 1A)
colnames(data)
data_2 <- data %>% select(SubjID, Sex, Age, predAge_ENG, predAge_CB)
data_2$Age_type <- ""
data_2_M <- data_2[which(data_2$Sex == 0),]
data_2_F <- data_2[which(data_2$Sex == 1),]
colnames(data_2_M)
data_M_Age <- data_2_M %>% select(SubjID, Sex, Age, Age_type)
data_M_Age$Age_type[data_M_Age$Age_type==""] <- "Chronological age (Males)"
data_M_predAgeENG <- data_2_M %>% select(SubjID, Sex, predAge_ENG, Age_type)
names(data_M_predAgeENG)[names(data_M_predAgeENG) == "predAge_ENG"] <- "Age"
data_M_predAgeENG$Age_type[data_M_predAgeENG$Age_type==""] <- "ENIGMA-predicted age (Males)"
data_M_predAgeCB <- data_2_M %>% select(SubjID, Sex, predAge_CB, Age_type)
names(data_M_predAgeCB)[names(data_M_predAgeCB) == "predAge_CB"] <- "Age"
data_M_predAgeCB$Age_type[data_M_predAgeCB$Age_type==""] <- "CentileBrain-predicted age (Males)"
colnames(data_2_F)
data_F_Age <- data_2_F%>% select(SubjID, Sex, Age, Age_type)
data_F_Age$Age_type[data_F_Age$Age_type==""] <- "Chronological age (Females)"
data_F_predAgeENG <- data_2_F %>% select(SubjID, Sex, predAge_ENG, Age_type)
names(data_F_predAgeENG)[names(data_F_predAgeENG) == "predAge_ENG"] <- "Age"
data_F_predAgeENG$Age_type[data_F_predAgeENG$Age_type==""] <- "ENIGMA-predicted age (Females)"
data_F_predAgeCB <- data_2_F %>% select(SubjID, Sex, predAge_CB, Age_type)
names(data_F_predAgeCB)[names(data_F_predAgeCB) == "predAge_CB"] <- "Age"
data_F_predAgeCB$Age_type[data_F_predAgeCB$Age_type==""] <- "CentileBrain-predicted age (Females)"

data_plotB1 <- rbind(data_M_Age, data_M_predAgeENG)
data_plotB1 <- rbind(data_plotB1, data_M_predAgeCB)
data_plotB1 <- rbind(data_plotB1, data_F_Age)
data_plotB1 <- rbind(data_plotB1, data_F_predAgeENG)
data_plotB1 <- rbind(data_plotB1, data_F_predAgeCB)

# Figure 1A
ggplot(data_plotB1, aes(x = Age, color = Age_type, fill = Age_type)) + 
  geom_density(alpha = 0.2) +
  theme_bw() + theme(axis.text=element_text(size=9),
                     axis.title=element_text(size=11),legend.title=element_blank(),
                     legend.text = element_text(size=9),legend.position="bottom")


##  Calculate performance metrics (MAE, r, R2) for Table S2

### With respect to sex (regardless of PE status)
data_M <- data[which(data$Sex == 0),]
data_F <- data[which(data$Sex == 1),]
#### MAE/RSD (coef.var) - Males
stat.desc(data_M$AE_ENG)
stat.desc(data_M$AE_CB)       
#### MAE/RSD - Females
stat.desc(data_F$AE_ENG)
stat.desc(data_F$AE_CB)
#### Post-hoc test for sex group difference 
t.test(data$AE_ENG ~ data$Sex, alternative = "two.sided")
t.test(data$AE_CB ~ data$Sex, alternative = "two.sided")
#### Pearson's R and R2 - Males
cor(data_M$Age,data_M$predAge_ENG)
cor(data_M$Age,data_M$predAge_CB)
caret::R2(data_M$Age,data_M$predAge_ENG)
caret::R2(data_M$Age,data_M$predAge_CB)
#### Pearson's R and R2 - Females
cor(data_F$Age,data_F$predAge_ENG)
cor(data_F$Age,data_F$predAge_CB)
caret::R2(data_F$Age,data_F$predAge_ENG)
caret::R2(data_F$Age,data_F$predAge_CB)

### with respect to both sex and PEs
data_M_NoPE <- data_M[which(data_M$PE == 0),]
data_M_PE <- data_M[which(data_M$PE== 1),]
data_F_NoPE <- data_F[which(data_F$PE == 0),]
data_F_PE <- data_F[which(data_F$PE== 1),]
#### without PEs
##### MAE/RSD - Males / No PEs
stat.desc(data_M_NoPE$AE_ENG)
stat.desc(data_M_NoPE$AE_CB)
##### MAE/RSD - Females / No PEs
stat.desc(data_F_NoPE$AE_ENG)
stat.desc(data_F_NoPE$AE_CB)
#### Post-hoc test for group difference in MAE
t.test(data_NoPE$AE_ENG ~ data_NoPE$Sex, alternative = "two.sided")
t.test(data_NoPE$AE_CB ~ data_NoPE$Sex, alternative = "two.sided")
##### Pearson's R and R2 - Males / No PEs
cor(data_M_NoPE$Age,data_M_NoPE$predAge_ENG)
cor(data_M_NoPE$Age,data_M_NoPE$predAge_CB)
caret::R2(data_M_NoPE$Age,data_M_NoPE$predAge_ENG)
caret::R2(data_M_NoPE$Age,data_M_NoPE$predAge_CB)
##### Pearson's R and R2 - Females / No PEs
cor(data_F_NoPE$Age,data_F_NoPE$predAge_ENG)
cor(data_F_NoPE$Age,data_F_NoPE$predAge_CB)
caret::R2(data_F_NoPE$Age,data_F_NoPE$predAge_ENG)
caret::R2(data_F_NoPE$Age,data_F_NoPE$predAge_CB)
#### with PEs
##### MAE/RSD - Males / PE
stat.desc(data_M_PE$AE_ENG)
stat.desc(data_M_PE$AE_CB)
##### MAE/RSD - Females / PE
stat.desc(data_F_PE$AE_ENG)
stat.desc(data_F_PE$AE_CB)
#### Post-hoc test for group difference in MAE
t.test(data_PE$AE_ENG ~ data_PE$Sex, alternative = "two.sided")
t.test(data_PE$AE_CB ~ data_PE$Sex, alternative = "two.sided")
##### Pearson's R and R2 - Males / PE
cor(data_M_PE$Age,data_M_PE$predAge_ENG)
cor(data_M_PE$Age,data_M_PE$predAge_CB)
caret::R2(data_M_PE$Age,data_M_PE$predAge_ENG)
caret::R2(data_M_PE$Age,data_M_PE$predAge_CB)
##### Pearson's R and R2 - Females / low PRS
cor(data_F_PE$Age,data_F_PE$predAge_ENG)
cor(data_F_PE$Age,data_F_PE$predAge_CB)
caret::R2(data_F_PE$Age,data_F_PE$predAge_ENG)
caret::R2(data_F_PE$Age,data_F_PE$predAge_CB)



# Plot brain-predicted age versus chronological age

## CentileBrain
### with respect to sex (Figure 1B)
Sex_ <- factor(data$Sex, labels = c("Males", "Females"))
ggplot(data, aes(x=Age, y=predAge_CB, color=Sex_, group=Sex_)) + 
  # geom_hline(yintercept=0 ,linetype=2) +
  geom_point(alpha=0.5, size=2) + 
  scale_y_continuous(limits=(c(7,42))) +
  geom_abline(a=0, b=1, color='black') +
  #geom_smooth(method=lm, colour="black", size=0.5, se=FALSE, fullrange=TRUE, linetype="dashed") +
  geom_smooth(aes(fill=Sex_), method=lm, size=0.5, se=TRUE, fullrange=FALSE, linetype="dashed") +
  # scale_color_manual(labels = c("Males", "Females"), values = c("blue", "red"), name = "Sex") +
  labs(x = "Chronological age", y = "CentileBrain-predicted age") +
  theme_bw()
cor.test(data$Age, data$predAge_CB)
# Repeat with respect to PE status (S.Figure S1A)
PE_status <- factor(data$PE, labels = c("No PE", "PE"))
ggplot(data, aes(x=Age, y=predAge_CB, color=PE_status, group=PE_status)) + 
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
ggplot(data, aes(x=Age, y=devAge_CB, color=as.factor(PE))) + 
  geom_point(alpha=0.5, size=2) + 
  geom_hline(yintercept=0 ,linetype=2) +
  scale_y_continuous(limits=(c(-10,10))) +
  geom_smooth(method=lm, colour="black", size=0.5, se=FALSE, fullrange=TRUE) +
  geom_smooth(aes(group=PE), method=lm, size=0.5, se=FALSE, fullrange=TRUE, linetype="dashed") +
  scale_color_manual(labels = c("No PE", "PE"), values = c("#009E73", "#D55E00"), name = "PE status") +
  labs(x = "Chronological age", y = "CentileBrain-derived brain-PAD") +
  theme_bw()
cor.test(data$devAge_CB, data$Age)

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

# CentileBrain-derived brain-PAD residualised for age (S.Figure S2, C)
devAge_CB_Age = lm(devAge_CB ~ Age, data=data)
data$devAge_CB_resAge=resid(devAge_CB_Age)
ggplot(data, aes(x=Age, y=devAge_CB_resAge, color=as.factor(PE))) + 
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
cor.test(data$devAge_CB_resAge, data$Age)

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


# Assess correlation between ENIGMA-derived brain-PAD and Centile-derived brain-PAD (S.Figure S3, left)
ggplot(data, aes(x=devAge_CB,y=devAge_ENG, color=as.factor(PE))) + 
  geom_point(alpha=0.5, size=2) + 
  geom_hline(yintercept=0, linetype=2) +
  geom_smooth(method=lm, colour="black", size=0.5, se=FALSE, fullrange=TRUE) +
  scale_color_manual(labels = c("None", "PE"), values = c("#009E73", "#D55E00"), name = "PE status") +
  labs(x = "CentileBrain-derived brain-PAD", y = "ENIGMA-derived brain-PAD") +
  theme_bw() + theme(axis.text=element_text(size=8),
                     axis.title=element_text(size=8), legend.title = element_text(size = 10),
                     legend.text = element_text(size = 10))
cor.test (data$devAge_ENG, data$devAge_CB)
cor.test (data$devAge_ENG, data$devAge_CB, method=c("spearman"))
##  repeated with age-residualised brain-PAD(S.Figure S3, right)
ggplot(data, aes(x=devAge_CB_resAge, y=devAge_ENG_resAge, color=as.factor(PE))) + 
  geom_point(alpha=0.5, size=2) + 
  geom_hline(yintercept=0, linetype=2) +
  geom_smooth(method=lm, colour="black", size=0.5, se=FALSE, fullrange=TRUE) +
  scale_color_manual(labels = c("None", "PE"), values = c("#009E73", "#D55E00"), name = "PE status") +
  labs(x = "CentileBrain-derived brain-PAD resid. for age", y = "ENIGMA-derived brain-PAD resid. for age") +
  theme_bw() + theme(axis.text=element_text(size=8),
                     axis.title=element_text(size=8), legend.title = element_text(size = 10),
                     legend.text = element_text(size = 10))
cor.test (data$devAge_CB_resAge, data$devAge_ENG_resAge)
cor.test (data$devAge_CB_resAge, data$devAge_ENG_resAge, method=c("spearman"))



# Step 5. Multiple regression for the effect of PEs on brain-PAD

## Step 5a Run primary models for difference in brain-PAD between low and high SCZ-PRS (primary analyses / Table B2)

### CentileBrain-brain-PAD (outcome)
#### Binary PE classification (Table S2)
DevAge_PE_Age_Sex_CB=lm(devAge_CB ~ as.factor(PE) + as.factor(Sex) + Age, data = data)
summary(DevAge_PE_Age_Sex_CB)
confint(DevAge_PE_Age_Sex_CB)
d_CB <- (-1.591)*(115+117)/(sqrt(115*117)*sqrt(228))
d.ci(d_CB,n1=115,n2=117)
##### check assumptions of linear regression
ols_plot_resid_qq(DevAge_PE_Age_Sex_CB)
ols_test_normality(DevAge_PE_Age_Sex_CB)
ols_test_correlation(DevAge_PE_Age_Sex_CB)
ols_plot_resid_fit(DevAge_PE_Age_Sex_CB)
ols_plot_resid_hist(DevAge_PE_Age_Sex_CB)


##### Raincloud plot for CentileBrain-derived brain_PAD in those with without and with PE, residualised for age and sex (Figure 2, A)
devAge_CB_Age_Sex = lm(devAge_CB ~ Age + Sex, data=data)
data$devAge_CB_resAgeSex = resid(devAge_CB_Age_Sex)
Group <- factor(data$PE, labels = c("Without PE", "with PE"))
ggplot(data = data, mapping = aes(x = "", y = devAge_CB_resAgeSex, fill = Group,
                                  color = Group)) +
  sm_raincloud(sep_level = 2, point.params = list(size = 3, shape = 21,
                                                  color = 'transparent', alpha = 0.4)) +
  scale_fill_manual(labels = c("No PE", "PE"), values = c("#009E73","#D55E00"), name = "PE status") +
  labs(x = "", y = "brain-PAD res. for age and sex") +
  geom_hline(yintercept=0 ,linetype=2) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                     axis.text=element_text(size=12),
                     axis.title=element_text(size=12), legend.title = element_text(size = 12),
                     legend.text = element_text(size = 12))

####  Repeat with ordinal PE classification (Table S2)
DevAge_PE_Ordinal_Age_Sex_CB=lm(devAge_CB ~ as.numeric(PE_4Level) + as.factor(Sex) + Age, data = data)
summary(DevAge_PE_Ordinal_Age_Sex_CB)
confint(DevAge_PE_Ordinal_Age_Sex_CB)
partial_r2(DevAge_PE_Ordinal_Age_Sex_CB)
##### check assumptions of linear regression
ols_plot_resid_qq(DevAge_PE_Ordinal_Age_Sex_CB)
ols_test_normality(DevAge_PE_Ordinal_Age_Sex_CB)
ols_test_correlation(DevAge_PE_Ordinal_Age_Sex_CB)
ols_plot_resid_fit(DevAge_PE_Ordinal_Age_Sex_CB)
ols_plot_resid_hist(DevAge_PE_Ordinal_Age_Sex_CB)

#### Specify as nominal (for Supplemetary Figure S4)
DevAge_PE_Nom_Age_Sex_CB=lm(devAge_CB ~ as.factor(PE_4Level) + as.factor(Sex) + Age, data = data)
summary(DevAge_PE_Nom_Age_Sex_CB)
confint(DevAge_PE_Nom_Age_Sex_CB)
partial_r2(DevAge_PE_Nom_Age_Sex_CB)
d_CB_S  <- (-0.840)*(115+41)/(sqrt(115*41)*sqrt(226))
d_CB_S
d.ci(d_CB_S,n1=115,n2=41)
d_CB_NC <- (-1.553)*(115+46)/(sqrt(115*46)*sqrt(226))
d.ci(d_CB_NC,n1=115,n2=46)
d_CB_C  <- (-0.870)*(115+30)/(sqrt(115*30)*sqrt(226))
d.ci(d_CB_C,n1=115,n2=30)

# Raincloud plot for ENIGMA-derived brain_PAD across PE groups, residualised for age and sex (Figure 2, B)
Group_ordinal <- factor(data$PE_4Level, labels = c("None", "Suspected", "Definite", "Definite, clinical"))
ggplot(data = data, mapping = aes(x=Group_ordinal, y=devAge_CB_resAgeSex, fill= Group_ordinal)) +
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
#### CentileBrain-deriived brain-PAD
##### Binary
DevAge_PE_Age_Sex_CB_nsf=lm(devAge_CB ~ as.factor(PE_NASF) + as.factor(Sex) + Age, data = data)
summary(DevAge_PE_Age_Sex_CB_nsf)
confint(DevAge_PE_Age_Sex_CB_nsf)
partial_r2(DevAge_PE_Age_Sex_CB_nsf)
d_CB_nsf  <- (-1.139)*(131+101)/(sqrt(131*101)*sqrt(228))
d_CB_nsf
d.ci(d_CB_nsf,n1=131,n2=101)
#####Ordinal 
DevAge_PE_ordinal_Age_Sex_CB_nsf=lm(devAge_CB ~ as.numeric(PE_4Level_NASF) + as.factor(Sex) + Age, data = data)
summary(DevAge_PE_ordinal_Age_Sex_CB_nsf)
confint(DevAge_PE_ordinal_Age_Sex_CB_nsf)
partial_r2(DevAge_PE_ordinal_Age_Sex_CB_nsf)
# Nominal
DevAge_PE_Nominal_Age_Sex_CB_nsf=lm(devAge_CB ~ as.factor(PE_4Level_NASF) + as.factor(Sex) + Age, data = data)
summary(DevAge_PE_Nominal_Age_Sex_CB_nsf)
confint(DevAge_PE_Nominal_Age_Sex_CB_nsf)
partial_r2(DevAge_PE_Nominal_Age_Sex_CB_nsf)
d_CB_S_nsf  <- (-0.249)*(131+38)/(sqrt(131*38)*sqrt(226))
d_CB_S_nsf  
d_CB_NC_nsf <- (-1.430)*(131+33)/(sqrt(131*33)*sqrt(226))
d_CB_NC_nsf
d_CB_C_nsf <- (-0.689)*(131+30)/(sqrt(131*30)*sqrt(226))
d_CB_C_nsf

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

#### Check for outliers in CentileBrain-derived brain-PAD (outcome)
##### Plot distribution of brian-PAD across the total sample
ggplot(data, aes(x=devAge_CB)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="blue") +
  labs(x = "CentileBrain-derived brain-PAD")
##### Now, Standardize brain-PAD across the the total sample
data$devAge_std_CB <- data$devAge_CB
data <- data %>% mutate_at(c('devAge_std_CB'), ~(scale(.) %>% as.vector))
##### Plot standardized brain-PAD across the the total sample
ggplot(data, aes(x=devAge_std_CB)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="blue") +
  labs(x = "standardised CentileBrain-derived brain-PAD")
##### Two positive outlier (> 3.00 SD) detected. Exclude: 
data_CB_wout <- data %>% filter(devAge_std_CB < 3.00)
summary(data_CB_wout)
##### Run model with binary PE classification
DevAge_PE_Age_Sex_CB_wout=lm(devAge_CB ~ as.factor(PE) + as.factor(Sex) + Age, data = data_CB_wout)
summary(DevAge_PE_Age_Sex_CB_wout)
confint(DevAge_PE_Age_Sex_CB_wout)
partial_r2(DevAge_PE_Age_Sex_CB_wout)
d_CB_wout <- (-1.606)*(114+116)/(sqrt(114*116)*sqrt(226))
d_CB_wout
d.ci(-0.2136671,n1=114,n2=116)
##### Repeat with ordinal PE classification 
DevAge_PE_ordinal_Age_Sex_CB_wout=lm(devAge_CB ~ as.numeric(PE_4Level) + as.factor(Sex) + Age, data = data_CB_wout)
summary(DevAge_PE_ordinal_Age_Sex_CB_wout)
confint(DevAge_PE_ordinal_Age_Sex_CB_wout)
partial_r2(DevAge_PE_ordinal_Age_Sex_CB_wout)
# ##### Repeat with nominal specification 
DevAge_PE_Nominal_Age_Sex_CB_wout=lm(devAge_CB ~ as.factor(PE_4Level) + as.factor(Sex) + Age, data = data_CB_wout)
summary(DevAge_PE_Nominal_Age_Sex_CB_wout)
confint(DevAge_PE_Nominal_Age_Sex_CB_wout)
partial_r2(DevAge_PE_Nominal_Age_Sex_CB_wout)
d_CB_S_wout  <- (-0.785)*(114+41)/(sqrt(114*41)*sqrt(224))
d.ci(d_CB_S_wout,n1=114,n2=41)
d_CB_NC_wout <- (-1.386)*(114+46)/(sqrt(114*46)*sqrt(224))
d.ci(d_CB_NC_wout,n1=114,n2=46)
d_CB_C_wout <- (-1.205)*(114+29)/(sqrt(114*29)*sqrt(224))
d.ci(d_CB_C_wout,n1=114,n2=29)


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
#####  CentileBrain-derived brain-PAD
DevAge_PE_Re_Age_Sex_CB=lm(devAge_CB ~ as.factor(PE_18_12_R) + as.factor(Sex) + Age, data = data)
summary(DevAge_PE_Re_Age_Sex_CB)
confint(DevAge_PE_Re_Age_Sex_CB)
partial_r2(DevAge_PE_Re_Age_Sex_CB)
d_CB_T <- (-1.273)*(93+73)/(sqrt(93*73)*sqrt(197))
d_CB_T
d.ci(-0.1827261,n1=93,n2=73)
d_CB_R<- (-1.102)*(93+36)/(sqrt(93*36)*sqrt(197))
d_CB_R
d.ci(-0.1750433,n1=93,n2=36)
###### Repeat after excluding outliers
DevAge_PE_Re_Age_Sex_CB=lm(devAge_CB ~ as.factor(PE_18_12_R) + as.factor(Sex) + Age, data = data_CB_wout)
summary(DevAge_PE_Re_Age_Sex_CB)
confint(DevAge_PE_Re_Age_Sex_CB)
partial_r2(DevAge_PE_Re_Age_Sex_CB)
d_CB_T_wout <- (-1.062)*(92+73)/(sqrt(92*73)*sqrt(196))
d_CB_T_wout
d.ci(-0.1527303,n1=92,n2=73)
d_CB_R_wout <- (-0.932)*(92+36)/(sqrt(92*36)*sqrt(196))
d_CB_R_wout
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

#### CentileBrain-derived brain-PAD

##### Additional adjustment of primary analyses for IQ at age 8
###### binary PE classification
DevAge_CB_IQ=lm(devAge_CB ~  Age + as.factor(Sex) + IQ_8 + as.factor(PE), data = data)
summary(DevAge_CB_IQ)
###### Ordinal PE classification
DevAge_CB_IQ=lm(devAge_CB ~  Age + as.factor(Sex) + IQ_8 + as.numeric(PE_4Level), data = data)
summary(DevAge_CB_IQ)

##### Additional adjustment of primary analyses for birth weight
###### binary PE classification
DevAge_CB_BW=lm(devAge_CB ~  Age + as.factor(Sex) + BW + as.factor(PE), data = data)
summary(DevAge_CB_BW)
confint(DevAge_CB_BW)
d_CB <- (-2.089)*(107+111)/(sqrt(107*111)*sqrt(213))
d.ci(d_CB,n1=107,n2=111)
###### Ordinal PE classification
DevAge_CB_BW=lm(devAge_CB ~  Age + as.factor(Sex) + BW + as.numeric(PE_4Level), data = data)
summary(DevAge_CB_BW)
confint(DevAge_CB_BW)
partial_r2(DevAge_CB_BW)

##### Additional adjustment of primary analyses for maternal education

###### binary PE classification
DevAge_CB_ME=lm(devAge_CB ~  Age + as.factor(Sex) +  as.numeric(ME) + as.factor (PE), data = data)
summary(DevAge_CB_ME)
###### Ordinal PE classification
DevAge_CB_ME=lm(devAge_CB ~  Age + as.factor(Sex) +  as.numeric(ME) + as.numeric (PE_4Level), data = data)
summary(DevAge_CB_ME)

##### Additional adjustment of primary analyses for parental social class
###### binary PE classification
DevAge_CB_ParSC=lm(devAge_CB ~  Age + as.factor(Sex) +  as.factor(parSC) + as.factor(PE), data = data)
summary(DevAge_CB_ParSC)
confint(DevAge_CB_ParSC)
d_CB <- (-2.168)*(104+102)/(sqrt(104*102)*sqrt(201))
d_CB
d.ci(d_CB,n1=104,n2=102)
###### Ordinal PE classification
DevAge_CB_ParSC=lm(devAge_CB ~  Age + as.factor(Sex) +  as.factor(parSC) + as.numeric(PE_4Level), data = data)
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

DevAge_ENG_BW=lm(devAge_ENG ~  Age + as.factor(Sex) + as.numeric(PE_4Level) + BW, data = data)
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
DevAge_ENG_ParSC=lm(devAge_ENG ~ Age + as.factor(Sex) +  as.factor(parSC) + as.factor(PE), data = data)
summary(DevAge_ENG_ParSC)
###### Ordinal PE classification
DevAge_ENG_ParSC=lm(devAge_ENG ~  Age + as.factor(Sex) +  as.factor(parSC) + as.numeric(PE_4Level), data = data)
summary(DevAge_ENG_ParSC)


## END ## 
## Move to script 4c