# Load Libraries ----------------------------------------------------------
library("gmodels")
library("caret")
library("InformationValue")
library("MASS")
library("car")
library("dplyr")
library("ggplot2")
library("tidyverse")
library("Hmisc")
library("caTools")
library("rbin")
library("smbinning")

# Update Column Names -----------------------------------------------------
names(give_credit) <- gsub("-","_",names(give_credit),fixed=TRUE) #convert dash to underscore
give_credit <- rename(give_credit, num_30_59_dpd=NumberOfTime30_59DaysPastDueNotWorse, num_60_89_dpd=NumberOfTime60_89DaysPastDueNotWorse) #update long names

# Histogram and Outliers --------------------------------------------------
CrossTable(give_credit$SeriousDlqin2yrs, prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE) #crosstab
hist(give_credit$RevolvingUtilizationOfUnsecuredLines)

#Identify Outliers thru Plots
plot(give_credit$DebtRatio) 
index_outlier <- which(give_credit$num_30_59_dpd>10)
View(index_outlier)
summary(give_credit$NumberOfOpenCreditLinesAndLoans)
describe(give_credit$rev_utl_loc) #from Hmisc pkg

#updating outliers to 999 for identification/bucketing ease
give_credit$rev_utl_loc <- ifelse(give_credit$RevolvingUtilizationOfUnsecuredLines>2.0,999,give_credit$RevolvingUtilizationOfUnsecuredLines)
give_credit$dpd_30_59_ct <- ifelse(give_credit$num_30_59_dpd>20,999,give_credit$num_30_59_dpd)
give_credit$dpd_60_89_ct <- ifelse(give_credit$num_60_89_dpd>20,999,give_credit$num_60_89_dpd)
give_credit$debt_ratio_pct <- ifelse(give_credit$DebtRatio>2.0,999,give_credit$DebtRatio)
give_credit$ever90dpd <- ifelse(give_credit$NumberOfTimes90DaysLate>=1,1,0) #quasi prev default
give_credit$ever_re_cl <- ifelse(give_credit$NumberRealEstateLoansOrLines>=1,1,0) #ever Real estate credit line 

# Sample Split ------------------------------------------------------------
#Stratified Sample by Delq2yr 
train_data <- give_credit %>%
  group_by (SeriousDlqin2yrs) %>%
  sample_frac(size=0.75)

train_data <- as.data.frame(train_data) #convert to df for use with smbinning pkg
is.data.frame(train_data) #returns TRUE

CrossTable(train_data$SeriousDlqin2yrs) #default prop=6.7%

# Binning -----------------------------------------------------------------
#Response as integer for smbinning 
train_data$response <- as.integer(train_data$SeriousDlqin2yrs)

#Age 
#smbinning pkg
result_bin <- smbinning(train_data,y="response",x="age",p=0.05)
result_bin$ivtable
result_bin$bands
result_bin$ctree
par(mfrow=c(2,2))
smbinning.plot(result_bin,option="dist")
smbinning.plot(result_bin,option="badrate")
smbinning.plot(result_bin,option="WoE")
train_data <- smbinning.gen(train_data,result_bin,chrname="age_bin_sm")

#Monthly Income
#Using smbinning pkg 
mth_inc_bin_sm <- smbinning(train_data,y="response",x="MonthlyIncome",p=0.05)
mth_inc_bin_sm$ivtable
par(mfrow=c(2,2))
smbinning.plot(mth_inc_bin_sm,option="dist")
smbinning.plot(mth_inc_bin_sm,option="badrate")
smbinning.plot(mth_inc_bin_sm,option="WoE")

# revolving loc usage
#smbinning RLOC
rloc_sm_bin <- smbinning(train_data,y="response",x="rev_utl_loc", p=0.05)
rloc_sm_bin$ivtable
rloc_sm_bin$iv
train_data <- smbinning.gen(train_data, rloc_sm_bin, chrname = "rloc_sm_bin")

#Debt Ratio
dr_sm_bin <- smbinning(train_data,y="response",x="debt_ratio_pct", p=0.05)
train_data <- smbinning.gen(train_data, dr_sm_bin, chrname = "debtratio_sm_bin")

#Notes:
#use smbinning as this categorizes 'missing' (null) into specific bin ('00') where logistic includes as category. 
## ie-doesnt remove the nulls, used as comparison category for explanatory variable

# Model -------------------------------------------------------------------
# M1 - 
m1 <- glm(SeriousDlqin2yrs ~ age_bin_sm + mth_inc_bin_sm, family="binomial", data=train_data)
summary(m1)

m2 <- glm(SeriousDlqin2yrs ~ age_bin_sm + rloc_sm_bin + debtratio_sm_bin, family="binomial", data=train_data)
summary(m2)

# Model Scoring -----------------------------------------------------------
coef(m1)
exp(coef(m1)) #odds ratio
exp(coef(m2))

#attached pd % to dataset
train_data$m1_pred <- round(fitted(m1),3)
train_data$m2_pred <- round(fitted(m2),3)

#ROC
plotROC(train_data$SeriousDlqin2yrs, train_data$m1_pred)
plotROC(train_data$SeriousDlqin2yrs, train_data$m2_pred)