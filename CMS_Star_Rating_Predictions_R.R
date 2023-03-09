library(tidyverse)
library(dplyr)
library(effects)
require(nnet)
library(corrplot)
library(rwa)
library(caret)
library(psych)
library(ResourceSelection)
library(ordinal)
setwd('~/UChicago/MSCA 32009 - Health Analytics/Project 4')
df16 <- read_csv('data/cms_hospital_patient_satisfaction_2016.csv')
df17 <- read_csv('data/cms_hospital_patient_satisfaction_2017.csv')
df18 <- read_csv('data/cms_hospital_patient_satisfaction_2018.csv')
df19 <- read_csv('data/cms_hospital_patient_satisfaction_2019.csv')
df20 <- read_csv('data/cms_hospital_patient_satisfaction_2020.csv')

# Stack the df's vertically

# Data already has year variables assigned

df <- rbind(df16, df17, df18, df19, df20)

################ DATA CLEANING AND RE-FORMATTING ########################

# Let's look at the 23 star ratings and linear mean scores from patient survey data

# Some hospitals reported too few cases and have Not Availables listed, we will
# discard those hospitals
star_ratings = filter(df, `Patient Survey Star Rating`!='Not Applicable')
star_ratings = filter(star_ratings, `Patient Survey Star Rating`!='Not Available')
LMVs = filter(df, `HCAHPS Linear Mean Value`!='Not Applicable')
LMVs = filter(LMVs, `HCAHPS Linear Mean Value`!='Not Available')

summary_star = filter(star_ratings, `HCAHPS Question`=='Summary star rating')
hospitals = select(summary_star, `Facility ID`, `Facility Name`, Address, City,
                      State, `ZIP Code`, `County Name`, `Phone Number`,
                      Year, `Hospital Type`, `Hospital Ownership`, `Emergency Services`,
                      `Meets criteria for promoting interoperability of EHRs`,
                      `Hospital overall rating`, `Mortality national comparison`,
                      `Safety of care national comparison`,
                      `Readmission national comparison`,
                      `Effectiveness of care national comparison`,
                      `Timeliness of care national comparison`,
                      `Efficient use of medical imaging national comparison`,
                      `Survey Response Rate Percent`, `Patient Survey Star Rating`)
colnames(hospitals)[22]  <- 'Summary_Star_Rating'
colnames(hospitals)[14] <- 'CMS_Star_Rating'
table(hospitals$CMS_Star_Rating, useNA = 'always')

# Let's drop rows in the data where our response is not available
hospitals = filter(hospitals, CMS_Star_Rating!='Not Available')
hospitals$CMS_Star_Rating <- as.numeric(hospitals$CMS_Star_Rating)

# Add in the other Star Ratings and LMVs
cleanliness_star = filter(star_ratings, `HCAHPS Question`=='Cleanliness - star rating')
cleanliness_star = select(cleanliness_star, `Facility ID`, Year, `Patient Survey Star Rating`)
colnames(cleanliness_star)[3] <- 'Cleanliness_Star_Rating'
hospitals = left_join(hospitals, cleanliness_star, by=c('Facility ID'='Facility ID',
                                                         'Year'='Year'))

nurse_comm_star = filter(star_ratings, `HCAHPS Question`=='Nurse communication - star rating')
nurse_comm_star = select(nurse_comm_star, `Facility ID`, Year, `Patient Survey Star Rating`)
colnames(nurse_comm_star)[3] <- 'Nurse_Communication_Star_Rating'
hospitals = left_join(hospitals, nurse_comm_star, by=c('Facility ID'='Facility ID',
                                                        'Year'='Year'))

doc_comm_star = filter(star_ratings, `HCAHPS Question`=='Doctor communication - star rating')
doc_comm_star = select(doc_comm_star, `Facility ID`, Year, `Patient Survey Star Rating`)
colnames(doc_comm_star)[3] <- 'Doctor_Communication_Star_Rating'
hospitals = left_join(hospitals, doc_comm_star, by=c('Facility ID'='Facility ID',
                                                       'Year'='Year'))

staff_response_star = filter(star_ratings, `HCAHPS Question`=='Staff responsiveness - star rating')
staff_response_star = select(staff_response_star, `Facility ID`, Year, `Patient Survey Star Rating`)
colnames(staff_response_star)[3] <- 'Staff_Responsiveness_Star_Rating'
hospitals = left_join(hospitals, staff_response_star, by=c('Facility ID'='Facility ID',
                                                       'Year'='Year'))

pain_star = filter(star_ratings, `HCAHPS Question`=='Pain management - star rating')
pain_star = select(pain_star, `Facility ID`, Year, `Patient Survey Star Rating`)
colnames(pain_star)[3] <- 'Pain_Management_Star_Rating'
hospitals = left_join(hospitals, pain_star, by=c('Facility ID'='Facility ID',
                                                           'Year'='Year'))

med_comm_star = filter(star_ratings, `HCAHPS Question`=='Communication about medicines - star rating')
med_comm_star = select(med_comm_star, `Facility ID`, Year, `Patient Survey Star Rating`)
colnames(med_comm_star)[3] <- 'Communication_About_Medicine_Star_Rating'
hospitals = left_join(hospitals, med_comm_star, by=c('Facility ID'='Facility ID',
                                                           'Year'='Year'))

discharge_star = filter(star_ratings, `HCAHPS Question`=='Discharge information - star rating')
discharge_star = select(discharge_star, `Facility ID`, Year, `Patient Survey Star Rating`)
colnames(discharge_star)[3] <- 'Discharge_Info_Star_Rating'
hospitals = left_join(hospitals, discharge_star, by=c('Facility ID'='Facility ID',
                                                           'Year'='Year'))

care_trans_star = filter(star_ratings, `HCAHPS Question`=='Care transition - star rating')
care_trans_star = select(care_trans_star, `Facility ID`, Year, `Patient Survey Star Rating`)
colnames(care_trans_star)[3] <- 'Care_Transition_Star_Rating'
hospitals = left_join(hospitals, care_trans_star, by=c('Facility ID'='Facility ID',
                                                           'Year'='Year'))

hosp_rate_star = filter(star_ratings, `HCAHPS Question`=='Overall hospital rating - star rating')
hosp_rate_star = select(hosp_rate_star, `Facility ID`, Year, `Patient Survey Star Rating`)
colnames(hosp_rate_star)[3] <- 'Hospital_Rating_Star_Rating'
hospitals = left_join(hospitals, hosp_rate_star, by=c('Facility ID'='Facility ID',
                                                           'Year'='Year'))

quietness_star = filter(star_ratings, `HCAHPS Question`=='Quietness - star rating')
quietness_star = select(quietness_star, `Facility ID`, Year, `Patient Survey Star Rating`)
colnames(quietness_star)[3] <- 'Quietness_Star_Rating'
hospitals = left_join(hospitals, quietness_star, by=c('Facility ID'='Facility ID',
                                                           'Year'='Year'))

recommend_star = filter(star_ratings, `HCAHPS Question`=='Recommend hospital - star rating')
recommend_star = select(recommend_star, `Facility ID`, Year, `Patient Survey Star Rating`)
colnames(recommend_star)[3] <- 'Recommendation_Star_Rating'
hospitals = left_join(hospitals, recommend_star, by=c('Facility ID'='Facility ID',
                                                           'Year'='Year'))

cleanliness_LMV = filter(LMVs, `HCAHPS Question`=='Cleanliness - linear mean score')
cleanliness_LMV = select(cleanliness_LMV, `Facility ID`, Year, `HCAHPS Linear Mean Value`)
colnames(cleanliness_LMV)[3] <- 'Cleanliness_LMV'
hospitals = left_join(hospitals, cleanliness_LMV, by=c('Facility ID'='Facility ID',
                                                           'Year'='Year'))

nurse_comm_LMV = filter(LMVs, `HCAHPS Question`=='Nurse communication - linear mean score')
nurse_comm_LMV = select(nurse_comm_LMV, `Facility ID`, Year, `HCAHPS Linear Mean Value`)
colnames(nurse_comm_LMV)[3] <- 'Nurse_Communication_LMV'
hospitals = left_join(hospitals, nurse_comm_LMV, by=c('Facility ID'='Facility ID',
                                                       'Year'='Year'))

doc_comm_LMV = filter(LMVs, `HCAHPS Question`=='Doctor communication - linear mean score')
doc_comm_LMV = select(doc_comm_LMV, `Facility ID`, Year, `HCAHPS Linear Mean Value`)
colnames(doc_comm_LMV)[3] <- 'Doctor_Communication_LMV'
hospitals = left_join(hospitals, doc_comm_LMV, by=c('Facility ID'='Facility ID',
                                                       'Year'='Year'))

staff_response_LMV = filter(LMVs, `HCAHPS Question`=='Staff responsiveness - linear mean score')
staff_response_LMV = select(staff_response_LMV, `Facility ID`, Year, `HCAHPS Linear Mean Value`)
colnames(staff_response_LMV)[3] <- 'Staff_Responsiveness_LMV'
hospitals = left_join(hospitals, staff_response_LMV, by=c('Facility ID'='Facility ID',
                                                       'Year'='Year'))

pain_LMV = filter(LMVs, `HCAHPS Question`=='Pain management - linear mean score')
pain_LMV = select(pain_LMV, `Facility ID`, Year, `HCAHPS Linear Mean Value`)
colnames(pain_LMV)[3] <- 'Pain_LMV'
hospitals = left_join(hospitals, pain_LMV, by=c('Facility ID'='Facility ID',
                                                       'Year'='Year'))

med_comm_LMV = filter(LMVs, `HCAHPS Question`=='Communication about medicines - linear mean score')
med_comm_LMV = select(med_comm_LMV, `Facility ID`, Year, `HCAHPS Linear Mean Value`)
colnames(med_comm_LMV)[3] <- 'Communication_About_Medicine_LMV'
hospitals = left_join(hospitals, med_comm_LMV, by=c('Facility ID'='Facility ID',
                                                       'Year'='Year'))

discharge_LMV = filter(LMVs, `HCAHPS Question`=='Discharge information - linear mean score')
discharge_LMV = select(discharge_LMV, `Facility ID`, Year, `HCAHPS Linear Mean Value`)
colnames(discharge_LMV)[3] <- 'Discharge_Info_LMV'
hospitals = left_join(hospitals, discharge_LMV, by=c('Facility ID'='Facility ID',
                                                       'Year'='Year'))

care_trans_LMV = filter(LMVs, `HCAHPS Question`=='Care transition - linear mean score')
care_trans_LMV = select(care_trans_LMV, `Facility ID`, Year, `HCAHPS Linear Mean Value`)
colnames(care_trans_LMV)[3] <- 'Care_Transition_LMV'
hospitals = left_join(hospitals, care_trans_LMV, by=c('Facility ID'='Facility ID',
                                                       'Year'='Year'))

hosp_rate_LMV = filter(LMVs, `HCAHPS Question`=='Overall hospital rating - linear mean score')
hosp_rate_LMV = select(hosp_rate_LMV, `Facility ID`, Year, `HCAHPS Linear Mean Value`)
colnames(hosp_rate_LMV)[3] <- 'Hospital_Rating_LMV'
hospitals = left_join(hospitals, hosp_rate_LMV, by=c('Facility ID'='Facility ID',
                                                       'Year'='Year'))

quietness_LMV = filter(LMVs, `HCAHPS Question`=='Quietness - linear mean score')
quietness_LMV = select(quietness_LMV, `Facility ID`, Year, `HCAHPS Linear Mean Value`)
colnames(quietness_LMV)[3] <- 'Quietness_LMV'
hospitals = left_join(hospitals, quietness_LMV, by=c('Facility ID'='Facility ID',
                                                       'Year'='Year'))

recommend_LMV = filter(LMVs, `HCAHPS Question`=='Recommend hospital - linear mean score')
recommend_LMV = select(recommend_LMV, `Facility ID`, Year, `HCAHPS Linear Mean Value`)
colnames(recommend_LMV)[3] <- 'Recommendation_LMV'
hospitals = left_join(hospitals, recommend_LMV, by=c('Facility ID'='Facility ID',
                                                       'Year'='Year'))
# Change some columns to numeric
for (i in 21:44) {
  hospitals[,i] <- as.numeric(unlist(hospitals[,i]))
}

################ EDA, FEATURE ENGINEERING, FEATURE SELECTION ########################

# Create a binary versions of the response to indicate hospital rating groups
# Groups are the cumulative binning of hospitals that are at least the number of stars
# associated with each group. i.e. hospitals$is_3_star groups all hospitals of AT LEAST
# 3 stars.
hospitals$is_5_star <- ifelse(hospitals$CMS_Star_Rating < 5,0,1)
hospitals$is_4_star <- ifelse(hospitals$CMS_Star_Rating < 4,0,1)
hospitals$is_3_star <- ifelse(hospitals$CMS_Star_Rating < 3,0,1)
hospitals$is_2_star <- ifelse(hospitals$CMS_Star_Rating < 2,0,1)
hospitals$is_1_star <- 1
hospitals$is_5_star <- as.factor(hospitals$is_5_star)
hospitals$is_4_star <- as.factor(hospitals$is_4_star)
hospitals$is_3_star <- as.factor(hospitals$is_3_star)
hospitals$is_2_star <- as.factor(hospitals$is_2_star)
hospitals$is_1_star <- as.factor(hospitals$is_1_star)

# Create a version of the df where CMS_Star_Rating is a factor
hospitals_fac <- hospitals
hospitals_fac$CMS_Star_Rating <- as.factor(hospitals_fac$CMS_Star_Rating)

colnames(hospitals)[1] <- 'FacilityID'
colnames(hospitals)[2] <- 'FacilityName'

# Aggregate the data by hospital over the years
hospitals$n = 1

# Pain Management is only recorded in 2016 and 2017. It is not of use for any
# forward-looking analysis, so we will ignore it moving forward.

hospitals_agg <- hospitals %>% group_by(FacilityName, FacilityID) %>%
  summarize(CMS_Star_rating=mean(CMS_Star_Rating),
            Summary_Star_Rating=mean(Summary_Star_Rating),
            Cleanliness_Star_Rating=mean(Cleanliness_Star_Rating),
            Nurse_Communication_Star_Rating=mean(Nurse_Communication_Star_Rating),
            Doctor_Communication_Star_Rating=mean(Doctor_Communication_Star_Rating),
            Staff_Responsiveness_Star_Rating=mean(Staff_Responsiveness_Star_Rating),
            Communication_About_Medicine_Star_Rating=mean(Communication_About_Medicine_Star_Rating),
            Discharge_Info_Star_Rating=mean(Discharge_Info_Star_Rating),
            Care_Transition_Star_Rating=mean(Care_Transition_Star_Rating),
            Hospital_Rating_Star_Rating=mean(Hospital_Rating_Star_Rating),
            Quietness_Star_Rating=mean(Quietness_Star_Rating),
            Recommendation_Star_Rating=mean(Recommendation_Star_Rating),
            Cleanliness_LMV=mean(Cleanliness_LMV),
            Nurse_Communication_LMV=mean(Nurse_Communication_LMV),
            Doctor_Communication_LMV=mean(Doctor_Communication_LMV),
            Staff_Responsiveness_LMV=mean(Staff_Responsiveness_LMV),
            Communication_About_Medicine_LMV=mean(Communication_About_Medicine_LMV),
            Discharge_Info_LMV=mean(Discharge_Info_LMV),
            Care_Transition_LMV=mean(Care_Transition_LMV),
            Hospital_Rating_LMV=mean(Hospital_Rating_LMV),
            Quietness_LMV=mean(Quietness_LMV),
            Recommendation_LMV=mean(Recommendation_LMV),
            n=sum(n))

# Descriptive statistics and counts for predictors and response
table(hospitals$CMS_Star_Rating)
table(hospitals$Summary_Star_Rating)
table(hospitals$Cleanliness_Star_Rating)
table(hospitals$Nurse_Communication_Star_Rating)
table(hospitals$Doctor_Communication_Star_Rating)
table(hospitals$Staff_Responsiveness_Star_Rating)
table(hospitals$Communication_About_Medicine_Star_Rating)
table(hospitals$Discharge_Info_Star_Rating)
table(hospitals$Care_Transition_Star_Rating)
table(hospitals$Hospital_Rating_Star_Rating)
table(hospitals$Quietness_Star_Rating)
table(hospitals$Recommendation_Star_Rating)

summary(hospitals$Cleanliness_LMV)
summary(hospitals$Nurse_Communication_LMV)
summary(hospitals$Doctor_Communication_LMV)
summary(hospitals$Staff_Responsiveness_LMV)
summary(hospitals$Communication_About_Medicine_LMV)
summary(hospitals$Discharge_Info_LMV)
summary(hospitals$Care_Transition_LMV)
summary(hospitals$Hospital_Rating_LMV)
summary(hospitals$Quietness_LMV)
summary(hospitals$Recommendation_LMV)

# Visualize the response distribution
hist(hospitals$CMS_Star_Rating, xlab='CMS Star Rating',
     main='Distribution of CMS Star Ratings')
table(hospitals$CMS_Star_Rating)

# Visualize the distributions of the star ratings and LMVs
hist(hospitals$Summary_Star_Rating)
hist(hospitals$Cleanliness_Star_Rating)
hist(hospitals$Nurse_Communication_Star_Rating)
hist(hospitals$Doctor_Communication_Star_Rating)
hist(hospitals$Staff_Responsiveness_Star_Rating)
hist(hospitals$Communication_About_Medicine_Star_Rating)
hist(hospitals$Discharge_Info_Star_Rating)
hist(hospitals$Care_Transition_Star_Rating)
hist(hospitals$Hospital_Rating_Star_Rating)
hist(hospitals$Quietness_Star_Rating)
hist(hospitals$Recommendation_Star_Rating)

hist(hospitals$Cleanliness_LMV)
hist(hospitals$Nurse_Communication_LMV)
hist(hospitals$Doctor_Communication_LMV)
hist(hospitals$Staff_Responsiveness_LMV)
hist(hospitals$Communication_About_Medicine_LMV)
hist(hospitals$Discharge_Info_LMV)
hist(hospitals$Care_Transition_LMV)
hist(hospitals$Hospital_Rating_LMV, xlab='Overall Hospital Rating (LMV)', 
     main='Distribution of Survey Response: Overall Hospital Rating (LMV)')
hist(hospitals$Quietness_LMV)
hist(hospitals$Recommendation_LMV)
# The LMV scores are nice and normally distributed, as was the intention of the 
# conversion of survey responses into these ratings

plot(hospitals$Cleanliness_LMV, hospitals$CMS_Star_Rating)
plot(hospitals$Nurse_Communication_LMV, hospitals$CMS_Star_Rating)
plot(hospitals$Doctor_Communication_LMV, hospitals$CMS_Star_Rating)
plot(hospitals$Staff_Responsiveness_LMV, hospitals$CMS_Star_Rating)
plot(hospitals$Communication_About_Medicine_LMV, hospitals$CMS_Star_Rating)
plot(hospitals$Discharge_Info_LMV, hospitals$CMS_Star_Rating)
plot(hospitals$Care_Transition_LMV, hospitals$CMS_Star_Rating)
plot(hospitals$Hospital_Rating_LMV, hospitals$CMS_Star_Rating)
plot(hospitals$Quietness_LMV, hospitals$CMS_Star_Rating)
plot(hospitals$Recommendation_LMV, hospitals$CMS_Star_Rating)
# All of the survey question LMVs appear somewhat correlated with the overall
# CMS_Star_Rating. Care_Transition_LMV is subject to some oddities and outliers
# among 2, 3, and 4-star hospitals.

# Make the star ratings factors again in the hospitals_fac dataframe
for (i in 22:33) {
  hospitals_fac[,i] <- as.numeric(unlist(hospitals_fac[,i]))
}

plot(hospitals_fac$Cleanliness_Star_Rating, hospitals_fac$CMS_Star_Rating)
plot(hospitals_fac$Nurse_Communication_Star_Rating, hospitals_fac$CMS_Star_Rating)
plot(hospitals_fac$Doctor_Communication_Star_Rating, hospitals_fac$CMS_Star_Rating)
plot(hospitals_fac$Staff_Responsiveness_Star_Rating, hospitals_fac$CMS_Star_Rating)
plot(hospitals_fac$Communication_About_Medicine_Star_Rating, hospitals_fac$CMS_Star_Rating)
plot(hospitals_fac$Discharge_Info_Star_Rating, hospitals_fac$CMS_Star_Rating)
plot(hospitals_fac$Care_Transition_Star_Rating, hospitals_fac$CMS_Star_Rating)
plot(hospitals_fac$Hospital_Rating_Star_Rating, hospitals_fac$CMS_Star_Rating)
plot(hospitals_fac$Quietness_Star_Rating, hospitals_fac$CMS_Star_Rating)
plot(hospitals_fac$Recommendation_Star_Rating, hospitals_fac$CMS_Star_Rating)
# The relationships look more difficult to distinguish with the star ratings

predictors <- hospitals[,34:44]
predictors <- predictors[,-5] # we drop pain management because it was not recorded after 2017
summary(predictors)

survey.cor = cor(predictors, method = c("pearson"))
corrplot(survey.cor, addCoef.col = 1, tl.cex = 0.5, number.cex = 0.35)
# Recommendation and Hospital Rating LMV scores are highly correlated (93%)
# Let's drop both of these from our group of predictors. These are general
# survey questions regarding how the patient would rate the hospital overall and
# whether or not they would recommend the hospital to someone else. Neither 
# help answer our question of what specifically in the patient experience
# should be targeted to improve these survey responses and thereby increase a
# hospital's star rating.

predictors <- predictors[,-10]
predictors <- predictors[,-8]

############### Relative Weights Analysis and Shapley Values ###############

# Relative weights analysis
rwa <- hospitals %>%
  rwa(outcome = "CMS_Star_Rating",
      predictors = c("Cleanliness_LMV", "Nurse_Communication_LMV",
                     "Doctor_Communication_LMV", "Staff_Responsiveness_LMV", 
                     "Communication_About_Medicine_LMV", "Discharge_Info_LMV",
                     "Care_Transition_LMV", "Quietness_LMV"),
      applysigns = TRUE)
print(rwa)

# Shapley Regression
install.packages('ShapleyValue')
library(ShapleyValue)
y <- hospitals$CMS_Star_Rating
x <- as.data.frame(predictors)
value <- shapleyvalue(y,x)
value

# Relative Weights Analysis and Shapley values paint a similar picture in terms
# of predictor importance. Care Transition is the most important feature, and then
# Staff Responsiveness and Nurse Communication are the next three
# most important features according to both.

################ MODELING, FEATURE SELECTION ########################

# Can we predict a hospital overall rating from its patient satisfaction scores?

# Splitting the data for model validation later
set.seed(030623)
sample_size = floor(0.7*nrow(hospitals))
picked = sample(seq_len(nrow(hospitals)),size = sample_size)
train =hospitals[picked,]
test =hospitals[-picked,]

# Linear Regression
lm_all <- lm(CMS_Star_Rating ~ Cleanliness_LMV + Nurse_Communication_LMV +
               Doctor_Communication_LMV + Staff_Responsiveness_LMV +
               Communication_About_Medicine_LMV + Discharge_Info_LMV +
               Care_Transition_LMV + Quietness_LMV, data = hospitals)
summary(lm_all)

# Feature selection
fwd1 <- step(lm(CMS_Star_Rating~1,data=hospitals),
             scope=formula(lm_all),direction='forward')
bkwd1 <- step(lm_all,direction='backward')

# Forwards and backwards selection methods both suggest dropping the communication
# about medicine variable. This could be due to high co-linearity with other
# communicative variables like Doctor/Nurse communication, staff responsiveness, and
# discharge information.

# Create optimized linear model:
lm_final <- lm(lm(CMS_Star_Rating ~ Cleanliness_LMV + Nurse_Communication_LMV +
                    Doctor_Communication_LMV + Staff_Responsiveness_LMV +
                    Discharge_Info_LMV +Care_Transition_LMV + Quietness_LMV,
                  data = hospitals))
summary(lm_final)

# Create optimized linear training model:
lm_train <- lm(lm(CMS_Star_Rating ~ Cleanliness_LMV + Nurse_Communication_LMV +
                    Doctor_Communication_LMV + Staff_Responsiveness_LMV +
                    Discharge_Info_LMV +Care_Transition_LMV + Quietness_LMV,
                  data = train))
# Copy the testing data
test_lm <- test
test_lm$pred_star_rating <- predict(lm_train, newdata=test_lm)
MSE <- mean((test_lm$pred_star_rating - test_lm$CMS_Star_Rating)^2)
RMSE <- sqrt(MSE)
RMSE

# Let's use the predictions from the linear regression to create classified predictions
test_lm$pred_star_bin <- ifelse(test_lm$pred_star_rating<1.5,1,0)
test_lm$pred_star_bin <- ifelse(test_lm$pred_star_rating >=1.5 &
                                  test_lm$pred_star_rating<2.5,2,test_lm$pred_star_bin)
test_lm$pred_star_bin <- ifelse(test_lm$pred_star_rating >=2.5 &
                                  test_lm$pred_star_rating<3.5,3,test_lm$pred_star_bin)
test_lm$pred_star_bin <- ifelse(test_lm$pred_star_rating >=3.5 &
                                  test_lm$pred_star_rating<4.5,4,test_lm$pred_star_bin)
test_lm$pred_star_bin <- ifelse(test_lm$pred_star_rating >=4.5,5,test_lm$pred_star_bin)

library(MASS) # don't bring in until now because it masks dplyr
test_lm$pred_star_bin <- as.factor(test_lm$pred_star_bin)
test_lm$CMS_Star_Rating_Fac <- as.factor(test_lm$CMS_Star_Rating)
a <- confusionMatrix(test_lm$CMS_Star_Rating_Fac, test_lm$pred_star_bin)
print(a)

# Logistic regression
# 5-stars
log1 <- glm(is_5_star ~ Cleanliness_LMV + Nurse_Communication_LMV +
              Doctor_Communication_LMV + Staff_Responsiveness_LMV +
              Communication_About_Medicine_LMV + Discharge_Info_LMV +
              Care_Transition_LMV+ Quietness_LMV, data = hospitals,
            family='binomial'(link='logit'))
summary(log1)
fwd2 <- step(glm(formula=is_5_star~1,data=hospitals,family='binomial'(link='logit')),
             scope=formula(log1),direction='forward')
bkwd2 <- step(log1,direction='backward')
# Forward and backwards selection algorithms indicate cleanliness and quietness
# are not important indicators of 5-star hospitals
# Create training model for 5-star group: 
log_5_star <- glm(is_5_star ~ Nurse_Communication_LMV +
                    Doctor_Communication_LMV + Staff_Responsiveness_LMV +
                    Communication_About_Medicine_LMV + Discharge_Info_LMV +
                    Care_Transition_LMV, data = train,
                  family='binomial'(link='logit'))

# 4-stars
log2 <- glm(is_4_star ~ Cleanliness_LMV + Nurse_Communication_LMV +
              Doctor_Communication_LMV + Staff_Responsiveness_LMV +
              Communication_About_Medicine_LMV + Discharge_Info_LMV +
              Care_Transition_LMV+ Quietness_LMV, data = hospitals,
            family='binomial'(link='logit'))
summary(log2)
fwd3 <- step(glm(formula=is_4_star~1,data=hospitals,family='binomial'(link='logit')),
             scope=formula(log2),direction='forward')
bkwd3 <- step(log2,direction='backward')
# For predicting hospitals to be at least 4-star rated, model is optimized without
# Quietness, Doctor Communication, and Medicine Communication variables
# Create training model for 4-star group: 
log_4_star <- glm(is_4_star ~ Cleanliness_LMV + Nurse_Communication_LMV + 
                    Staff_Responsiveness_LMV + Discharge_Info_LMV +
                    Care_Transition_LMV, data = train,
                  family='binomial'(link='logit'))

# 3-stars:
log3 <- glm(is_3_star ~ Cleanliness_LMV + Nurse_Communication_LMV +
              Doctor_Communication_LMV + Staff_Responsiveness_LMV +
              Communication_About_Medicine_LMV + Discharge_Info_LMV +
              Care_Transition_LMV+ Quietness_LMV, data = train,
            family='binomial'(link='logit'))
summary(log3)
fwd4 <- step(glm(formula=is_3_star~1,data=hospitals,family='binomial'(link='logit')),
             scope=formula(log3),direction='forward')
bkwd4 <- step(log3,direction='backward')
# Communication about medicine is dropped from this model based on stepwise selections
# Create training model for 3-star group:
log_3_star <- glm(is_3_star ~ Cleanliness_LMV + Nurse_Communication_LMV +
                    Doctor_Communication_LMV + Staff_Responsiveness_LMV +
                    Discharge_Info_LMV + Care_Transition_LMV+ Quietness_LMV,
                  data = train, family='binomial'(link='logit'))

# 2-stars:
log4 <- glm(is_2_star ~ Cleanliness_LMV + Nurse_Communication_LMV +
              Doctor_Communication_LMV + Staff_Responsiveness_LMV +
              Communication_About_Medicine_LMV + Discharge_Info_LMV +
              Care_Transition_LMV+ Quietness_LMV, data = hospitals,
            family='binomial'(link='logit'))
summary(log4)
fwd5 <- step(glm(formula=is_2_star~1,data=hospitals,family='binomial'(link='logit')),
             scope=formula(log4),direction='forward')
bkwd5 <- step(log4,direction='backward')
# Nurse Communication, Medicine Communication, and Care Transition variables are
# dropped from this model
# Create a training model for 2-star hospitals:
log_2_star <- glm(is_2_star ~ Cleanliness_LMV + Doctor_Communication_LMV +
                    Staff_Responsiveness_LMV + Discharge_Info_LMV + Quietness_LMV,
                  data = train, family='binomial'(link='logit'))

# All rated hospitals are given at least 1-star, so we don't need to model this.
# The probability of a hospital being rated 1-star will simply be
# 1 - (cumulative probability of the hospital being given other ratings)

# Predict on the testing data with logistic regression models
test$pred_5_star <- predict(log_5_star, newdata=test, type='response')
test$pred_4_star <- predict(log_4_star, newdata=test, type='response') -
  test$pred_5_star
test$pred_3_star <- predict(log_3_star, newdata=test, type='response') -
  (test$pred_4_star + test$pred_5_star)
test$pred_2_star <- predict(log_2_star, newdata=test, type='response') -
  (test$pred_3_star + test$pred_4_star + test$pred_5_star)
test$pred_1_star <- 1 -
  (test$pred_2_star + test$pred_3_star + test$pred_4_star + test$pred_5_star) 

# Create classification predictions based on predicted class probabilities from
# the different logistic regressions
test$pred_star_rating <- ifelse(test$pred_5_star > test$pred_4_star &
                                  test$pred_5_star > test$pred_3_star &
                                  test$pred_5_star > test$pred_2_star &
                                  test$pred_5_star > test$pred_1_star,5,0)
test$pred_star_rating <- ifelse(test$pred_4_star > test$pred_5_star &
                                  test$pred_4_star > test$pred_3_star &
                                  test$pred_4_star > test$pred_2_star &
                                  test$pred_4_star > test$pred_1_star,4,
                                test$pred_star_rating)
test$pred_star_rating <- ifelse(test$pred_3_star > test$pred_5_star &
                                  test$pred_3_star > test$pred_4_star &
                                  test$pred_3_star > test$pred_2_star &
                                  test$pred_3_star > test$pred_1_star,3,
                                test$pred_star_rating)
test$pred_star_rating <- ifelse(test$pred_2_star > test$pred_5_star &
                                  test$pred_2_star > test$pred_4_star &
                                  test$pred_2_star > test$pred_3_star &
                                  test$pred_2_star > test$pred_1_star,2,
                                test$pred_star_rating)
test$pred_star_rating <- ifelse(test$pred_1_star > test$pred_5_star &
                                  test$pred_1_star > test$pred_4_star &
                                  test$pred_1_star > test$pred_3_star &
                                  test$pred_1_star > test$pred_2_star,1,
                                test$pred_star_rating)
test$CMS_Star_Rating <- as.factor(test$CMS_Star_Rating)
test$pred_star_rating <- as.factor(test$pred_star_rating)

# Evaluate predictions
b <- confusionMatrix(test$CMS_Star_Rating, test$pred_star_rating)
print(b)

# Visualize the distribution of the predicted response
test$pred_star_rating_num <- as.numeric(test$pred_star_rating)
hist(test$pred_star_rating_num)

# Hosmer-Lemeshow test to see the goodness of fit
#library(ResourceSelection)
#hoslem.test(hospitals$is_3_star, fitted(log1))

# Plot some effects plots
#library(effects)
#cleanliness_log <- glm(is_5_star~Cleanliness_LMV,data=hospitals,family='binomial'(link='logit'))
#summary(cleanliness_log)
#lot(allEffects(cleanliness_log))
#a.out <- allEffects(cleanliness_log)
#a.out$Cleanliness_LMV$model.matrix
#invLogit(a.out$cleanliness_LMV$model.matrix %*% coef(cleanliness_log))

# Ordinal Logistic Regression
# Let's copy the df and standardize the covariates
hospitals_polr <- hospitals_fac
polr1 <- polr(CMS_Star_Rating ~ Cleanliness_LMV + Nurse_Communication_LMV +
              Doctor_Communication_LMV + Staff_Responsiveness_LMV +
              Communication_About_Medicine_LMV + Discharge_Info_LMV +
              Care_Transition_LMV + Quietness_LMV,
            data = hospitals_polr)
summary(polr1)

# POLR fit doesn't give us p-values, but clm does. However, clm requires standardization
# of covariates which makes them less interpretable. Let's use a clm fit to see the
# p-values and validate the coefficients are the same as a POLR fit with standarized
# data
covariates <- hospitals_polr[,34:44] # this time we won't drop pain management
standardized_preds <- scale(covariates)
hospitals_clm <- hospitals_fac
hospitals_clm[,34:44] <- standardized_preds
clm1 <- clm(CMS_Star_Rating ~ Cleanliness_LMV + Nurse_Communication_LMV +
                Doctor_Communication_LMV + Staff_Responsiveness_LMV +
                Communication_About_Medicine_LMV + Discharge_Info_LMV +
                Care_Transition_LMV + Quietness_LMV,
              data = hospitals_clm)
summary(clm1)
polr2 <- polr(CMS_Star_Rating ~ Cleanliness_LMV + Nurse_Communication_LMV +
                Doctor_Communication_LMV + Staff_Responsiveness_LMV +
                Communication_About_Medicine_LMV + Discharge_Info_LMV +
                Care_Transition_LMV + Quietness_LMV,
              data = hospitals_clm)
summary(polr2)
# Coefficients on the intercept and predictors are the same across POLR and clm
# fits with standardized data. P-values on the clm fit suggest Doctor Communication
# and Medicine Communication are not of use

fwd6 <- step(clm(formula=CMS_Star_Rating~1,data=hospitals_clm),
             scope=formula(clm1),direction='forward')
bkwd6 <- step(clm1,direction='backward')
fwd7 <- step(polr(formula=CMS_Star_Rating~1,data=hospitals_clm),
             scope=formula(polr2),direction='forward')
bkwd7 <- step(polr2,direction='backward')
# Forwards and backwards selection are suggesting to drop Doctor Communication
# and Medicine Communication variables for both fits
# Create a full optimized POLR model on the unstandardized data for interpretation
polr_full <- polr(CMS_Star_Rating ~ Cleanliness_LMV + Nurse_Communication_LMV +
                    Staff_Responsiveness_LMV + Discharge_Info_LMV +
                    Care_Transition_LMV + Quietness_LMV,
                  data = hospitals_polr)
summary(polr_full)

# Copy the train/test data
train_fac <- train
train_fac$CMS_Star_Rating <- as.factor(train_fac$CMS_Star_Rating)
test_fac <- test
test_fac$CMS_Star_Rating <- as.factor(test_fac$CMS_Star_Rating)

# Drop previous predictions from these datasets

# Create an optimized ordinal logistic regression training model:
polr_train <- polr(CMS_Star_Rating ~ Cleanliness_LMV + Nurse_Communication_LMV +
                     Staff_Responsiveness_LMV + Discharge_Info_LMV +
                     Care_Transition_LMV + Quietness_LMV,
                   data = train_fac)
ord_predictions <- predict(polr_train, newdata = test_fac, type = "probs")
ord_predictions <- as.data.frame(ord_predictions)
test_fac[,50:54] <- ord_predictions # replace previous on the copied dataset
test_fac <- test_fac[,-56] # drop previous predictions
test_fac <- test_fac[,-55] # drop previous predictions

# Rename the prediction columns. ord_predictions is flipped from how I previously
# setup the test dataframe.
colnames(test_fac)[50] <- 'pred_1_star'
colnames(test_fac)[51] <- 'pred_2_star'
colnames(test_fac)[52] <- 'pred_3_star'
colnames(test_fac)[53] <- 'pred_4_star'
colnames(test_fac)[54] <- 'pred_5_star'

# Create the classified predictions
test_fac$pred_star_rating <- ifelse(test_fac$pred_5_star > test_fac$pred_4_star &
                                  test_fac$pred_5_star > test_fac$pred_3_star &
                                  test_fac$pred_5_star > test_fac$pred_2_star &
                                  test_fac$pred_5_star > test_fac$pred_1_star,5,0)
test_fac$pred_star_rating <- ifelse(test_fac$pred_4_star > test_fac$pred_5_star &
                                  test_fac$pred_4_star > test_fac$pred_3_star &
                                  test_fac$pred_4_star > test_fac$pred_2_star &
                                  test_fac$pred_4_star > test_fac$pred_1_star,4,
                                test_fac$pred_star_rating)
test_fac$pred_star_rating <- ifelse(test_fac$pred_3_star > test_fac$pred_5_star &
                                  test_fac$pred_3_star > test_fac$pred_4_star &
                                  test_fac$pred_3_star > test_fac$pred_2_star &
                                  test_fac$pred_3_star > test_fac$pred_1_star,3,
                                test_fac$pred_star_rating)
test_fac$pred_star_rating <- ifelse(test_fac$pred_2_star > test_fac$pred_5_star &
                                  test_fac$pred_2_star > test_fac$pred_4_star &
                                  test_fac$pred_2_star > test_fac$pred_3_star &
                                  test_fac$pred_2_star > test_fac$pred_1_star,2,
                                test_fac$pred_star_rating)
test_fac$pred_star_rating <- ifelse(test_fac$pred_1_star > test_fac$pred_5_star &
                                  test_fac$pred_1_star > test_fac$pred_4_star &
                                  test_fac$pred_1_star > test_fac$pred_3_star &
                                  test_fac$pred_1_star > test_fac$pred_2_star,1,
                                test_fac$pred_star_rating)
test_fac$pred_star_rating <- as.factor(test_fac$pred_star_rating)

# Evaluate predictions
c <- confusionMatrix(test_fac$CMS_Star_Rating, test_fac$pred_star_rating)
print(c)

# Visualize distribution of the predicted response
test$pred_star_rating_num <- as.numeric(test$pred_star_rating)
hist(test$pred_star_rating_num)

# Multinomial regression
hospitals_fac$CMS_Star_Rating2 <- relevel(hospitals_fac$CMS_Star_Rating, ref = 1)
mnr1 <- multinom(CMS_Star_Rating2 ~ Cleanliness_LMV + Nurse_Communication_LMV +
                   Doctor_Communication_LMV + Staff_Responsiveness_LMV +
                   Communication_About_Medicine_LMV + Discharge_Info_LMV +
                   Care_Transition_LMV + Quietness_LMV, data = hospitals_fac)
summary(mnr1)

z <- summary(mnr1)$coefficients/summary(mnr1)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# AIC on the multinomial regression is higher than the ordinal regression. The
# p-values on the covariates paint a similar story. The communication predictors
# show somehwat weak statistical significance, likely due to co-linearity.

# Poisson Regression (did not fit the data well)
#pois1 <- glm(Summary_Star_Rating ~ Cleanliness_LMV + Nurse_Communication_LMV +
#               Doctor_Communication_LMV + Staff_Responsiveness_LMV +
#               Communication_About_Medicine_LMV + Discharge_Info_LMV +
#               Care_Transition_LMV + Hospital_Rating_LMV + Quietness_LMV +
#               Recommendation_LMV, data = hospitals, family = poisson(link ='log'))
#summary(pois1)
#mean(hospitals$Summary_Star_Rating)
#var(hospitals$Summary_Star_Rating)

# Underdispersion indicates a lot of the LMVs are likely correlated

#pois2 <- glm(Summary_Star_Rating ~ Hospital_Rating_LMV, data = hospitals,
#             family = poisson(link='log'))
#summary(pois2)
#test$pred <- predict(pois2, newdata=test)

# Let's try some alternative predictions based on manually changing the classification
# thresholds. We noticed from the confusionMatrices on logistic and ordinal logistic
# predictions that the models tend to underestimate the chance a hospital is rated 3-stars,
# and also overestimate the chance of hospitals being 1-star or 5-star
test$pred_3_star_inflated = test$pred_3_star + .2
test$pred_1_star_deflated = test$pred_1_star - .2
test$pred_5_star_deflated = test$pred_5_star - .2
test$pred_star_rating_alt <- ifelse(test$pred_5_star_deflated > test$pred_4_star &
                                  test$pred_5_star_deflated > test$pred_3_star &
                                  test$pred_5_star_deflated > test$pred_2_star &
                                  test$pred_5_star_deflated > test$pred_1_star_deflated,5,0)
test$pred_star_rating_alt <- ifelse(test$pred_4_star > test$pred_5_star_deflated &
                                  test$pred_4_star > test$pred_3_star &
                                  test$pred_4_star > test$pred_2_star &
                                  test$pred_4_star > test$pred_1_star_deflated,4,
                                test$pred_star_rating_alt)
test$pred_star_rating_alt <- ifelse(test$pred_3_star > test$pred_5_star_deflated &
                                  test$pred_3_star > test$pred_4_star &
                                  test$pred_3_star > test$pred_2_star &
                                  test$pred_3_star > test$pred_1_star_deflated,3,
                                test$pred_star_rating_alt)
test$pred_star_rating_alt <- ifelse(test$pred_2_star > test$pred_5_star_deflated &
                                  test$pred_2_star > test$pred_4_star &
                                  test$pred_2_star > test$pred_3_star &
                                  test$pred_2_star > test$pred_1_star_deflated,2,
                                test$pred_star_rating_alt)
test$pred_star_rating_alt <- ifelse(test$pred_1_star_deflated > test$pred_5_star_deflated &
                                  test$pred_1_star_deflated > test$pred_4_star &
                                  test$pred_1_star_deflated > test$pred_3_star &
                                  test$pred_1_star_deflated > test$pred_2_star,1,
                                test$pred_star_rating_alt)
test$CMS_Star_Rating <- as.factor(test$CMS_Star_Rating)
test$pred_star_rating_alt <- as.factor(test$pred_star_rating_alt)

# Evaluate predictions
d <- confusionMatrix(test$CMS_Star_Rating, test$pred_star_rating_alt)
print(d)

# Visualize the distribution of the predicted response
test$pred_star_rating_num <- as.numeric(test$pred_star_rating)
hist(test$pred_star_rating_num)

# Ensemble logistic regression proves to be the best approach. Let's look at the full models
# for interpretability.

# Create a full optimized 5-star hospital model for interpretation
log_mod1 <- glm(is_5_star ~ Nurse_Communication_LMV +
                  Doctor_Communication_LMV + Staff_Responsiveness_LMV +
                  Communication_About_Medicine_LMV + Discharge_Info_LMV +
                  Care_Transition_LMV, data = hospitals,
                family='binomial'(link='logit'))
summary(log_mod1)

# Create an optimized full model for the 4-star group:
log_mod2 <- glm(is_4_star ~ Cleanliness_LMV + Nurse_Communication_LMV + 
                   Staff_Responsiveness_LMV + Discharge_Info_LMV +
                   Care_Transition_LMV, data = hospitals,
                 family='binomial'(link='logit'))
summary(log_mod2)

# Create a full optimized model for the 3-star group:
log_mod3 <- glm(is_3_star ~ Cleanliness_LMV + Nurse_Communication_LMV +
                   Doctor_Communication_LMV + Staff_Responsiveness_LMV +
                   Discharge_Info_LMV + Care_Transition_LMV+ Quietness_LMV,
                 data = hospitals, family='binomial'(link='logit'))
summary(log_mod3)

# Create a full optimized model for the 3-star group:
log_mod4 <- glm(is_2_star ~ Cleanliness_LMV + Doctor_Communication_LMV +
                   Staff_Responsiveness_LMV + Discharge_Info_LMV + Quietness_LMV,
                 data = hospitals, family='binomial'(link='logit'))
summary(log_mod4)

hoslem.test(hospitals$is_5_star, fitted(log_mod1))

