# To center a variable at some specific value (e.g., 5), simply change
# the mean function to a specific value in the code
dat$NewVar <- dat$OldVar – 5
# For example we can do this for the variables sex and time below
GPA_long$sex <- unclass(GPA_long$sex) – 1
# center the time at 1 --> 0 = starting point of the time
GPA_long$time <- as.numeric(GPA_long$time) - 1

## grand mean centering at level 1 predictor
exam$lrt_gmc <- exam$LRTscore - mean(exam$LRTscore)

## group mean centering at level 1 predictor
exam$lrt_groupmean <-  ave(exam$LRTscore, exam$School) #compute the group mean (per shcool) for LRTscore 
exam$lrt_wcc <- exam$LRTscore - exam$lrt_groupmean 

################################
###### Contextual effects ######
################################
library(lme4)
library(lmerTest)
### book page 51, 52 also explains this.

setwd("~/Desktop/MLM/Lab2/lab2a_contextual effects")

# load data
exam <- read.csv("exam.csv", header = TRUE)
head(exam)

## 1. Start with model3: the model that includes a random intercept, the predictors at level 1 and the predictors at level 2. Include the predictor LRT using grand mean centering.
exam$lrt_gmc <- exam$LRTscore - mean(exam$LRTscore)

model3 <- lmer(Examscore ~ lrt_gmc + AvsLRT + (1|School), REML = FALSE, data=exam)
summary(model3)

## 1a) What does the regression coefficient for LRT represent in terms of contextual effects?
# 0.5595: meaning that with every point higher on LRT score, the child exam score is expected to increase by 0.56.
# Beth answer: The regression coefficient for LRT represents the within school effect of individual reading ability on the exam score. So with every point higher a child has on individual reading scores compared to her/his peers, the child scores 0.56 points higher on the exam score.

## 1b) What does the regression coefficient for AvsLRT represent in terms of contextual effects?
# 0.3583: As LRTscore is grand-mean centered, the regression coefficient for AvsLRT represents the "DIFFERENCE" between within school effect of individual reading ability on exam score and between school effect of average reading ability on the exam score. So on top of the within effect of individual reading ability on exam score, if a child is within a school that on average 1 point higher on reading score, then the child on average gains 0.36 points more on the exam score.


## 2. Suppose we want to calculate the predicted exam score for 2 children, child X and Y. We want to predict the exam score for these children under 2 scenarios: when the children would go to school A, and when the children would go to school B. We have the following information:

# Child X has score -2, so he/she has a score below average
# Child Y has score 2, so he/she has a score above average
# School A has an average of -1, so the school is below average
# School B has an average of 1, so the school is above average
## What are the predicted exam scores for the two children under the two scenarios?
childX_schoolA = 0.01 + (-2)*0.56 + (-1)*0.36
childX_schoolB = 0.01 + (-2)*0.56 + (1)*0.36
childY_schoolA = 0.01 + (2)*0.56 + (-1)*0.36
childY_schoolB = 0.01 + (2)*0.56 + (1)*0.36


## 3. Again, fit model 3 but this time include the predictor LRT using cluster mean centering (i.e., within cluster centering).

# cluster mean centering
exam$lrt_groupmean <-  ave(exam$LRTscore, exam$School)
exam$lrt_wcc <- exam$LRTscore - exam$lrt_groupmean

model3_wcc <- lmer(Examscore ~ 1 + lrt_wcc + AvsLRT + (1|School), REML = FALSE, data=exam )
summary(model3_wcc)

## 3a) What does the regression coefficient for LRT represent in terms of contextual effects? (WCC)

# 0.5595 : LRT regression coefficient represents the absolute within effects of the model: how the individual relative reading score relates to the exam score within the class. So with every point higher on individual reading scores compared to her/his peers, the child scores 0.56 higher on the exam score.

# Beth answer: The LRT regression coefficient represents the within effects of the model, i.e. the effect that the LRT reading score of an individual student has on their exam score. One point above the class average results in a higher predicted exam score. In this case, for every point a student scores higher on LRT, on average the predicted exam score of this student increases by 0.56 points.

## 3b) What does the regression coefficient for AvsLRT represent in terms of contextual effects? (WCC)

# 0.917: The AvsLRT regression coefficient represents the absolute between effects of the model, i.e., the effect that belonging to different schools has on the exam score of students; how is the average school reading score relates to the average school exam score. For every point a school score higher on the AvsLRT, the average exam score of the students in that school increases by 0.92.

## 4. Does the within effect differ significantly from the between effect? Interpret.
# YES, given that b_AvsLRT = 0.36, t(65) = 3.250, p = .0018  (see AvsLRT reg.coefficient Wald test result in the grand-mean centered model)

summary(model3)
