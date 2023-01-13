## always libraries are ready!
library(lme4)
library(lmerTest)
library(tidyr) #for pivot_longer
library(dplyr)
library(ggplot2)
library(jtools)

setwd("~/Desktop/MLM/Lab2/lab2b_longitudinal MLM")

# load data
dat <- read.csv("gpa2.csv")
head(dat)

## 1. At which level is each variable?
# sex, highgpa --> person/subject level
# job --> occasion level
# gpa --> DV

## 2. Which variable is the level 2 identification variable?
# "student"

## 3. Convert the wide data file to long-format
longdat <- dat %>% 
  pivot_longer(c(4:15), names_to = c(".value", "time"),
               names_pattern = "(gpa|job)(.)")  %>% 
  
  # recode the data: MAKE SURE "TIME" IS CODED SUCH THAT 0 IS INCLUDED MEANINFULLY
  # here "sex" was also needed to be recoed as it was 1:male, 2:female, 0 is not available
  mutate(sex =  sex -1, time = as.numeric(time) - 1)

## alternatively, to recode sex and time:
GPA_long$sex <- unclass(GPA_long$sex) - 1
GPA_long$time <- as.numeric(GPA_long$time) - 1

# check the longdata
head(longdat)

## 5. Check the linearity assumption: normally you would check for both level1 and level2 predictors (Check Assignment2)

# • Make a scatterplot of GPA and Time (add a linear and quadratic fit line).
# • Make a scatterplot of GPA and Job (add a linear and quadratic fit line).
## 6. Check for outliers (just look in the scatterplots)

# gpa ~ time
ggplot(longdat,
             aes(x = time, y = gpa)) +
  geom_point() +
  geom_smooth(method = "lm",
              aes(color = "linear"),
              se = FALSE) +
  geom_smooth(method = "lm",
              formula = y ~ x + I(x^2),
              aes(color = "quadratic"),
              se = FALSE) +
  theme_minimal() 

# gpa ~ job
ggplot(longdat,
       aes(x = job, y = gpa)) +
  geom_point() +
  geom_smooth(method = "lm",
              aes(color = "linear"),
              se = FALSE) +
  geom_smooth(method = "lm",
              formula = y ~ x + I(x^2),
              aes(color = "quadratic"),
              se = FALSE) +
  theme_minimal() 

## 7. Specify the intercept only model.

## m0: single-level model
m0 <- lm(gpa ~1, data=longdat)

## m1: intercept only model --> compute ICC!
m1 <- lmer(gpa ~ 1 + (1|student), REML = FALSE, data= longdat)
summary(m1)

ICC <- 0.0567 / (0.0567 + 0.09759); ICC
summ(m1) #can double-check the ICC value 

## check if ML analysis is warranted: is intercept variance significant?
## DIVIDE P-VAL / 2 !! as you're testing random intercept
anova(m1, m0)

## 8. Specify the model with Time as a linear predictor of the trend over time.

## m2: add Time --> BENCHMARK MODEL FOR R2
m2 <- lmer(gpa ~ 1 + time + (1|student), REML = FALSE, data= longdat)
summary(m2)

# check if time is a significant predictor
anova(m2, m1)

## 9. Set up the model with the time-varying predictor Job.
#• Calculate the explained variance at level 1 and level 2.

## m3: add time-varying predictor (occasion level)
## when they are continuous variable, think of grand-mean centering them!

dat$job <- dat$job - mean(dat$job) # job: grand-mean centered
m3 <- lmer(gpa ~ 1 + time + job + (1|student), REML = FALSE, data= longdat)
summary(m3)

# compute R2: Benchmark model = m2 with only time
R2.lv1 <- (0.058 - 0.055) / 0.058
R2.lv2 <-  (0.063 - 0.052) / 0.063 

# to check if job is a significant predictor
anova(m3, m2)

## 10. Set up the model with the time-invariant predictors Gender and HighGPA.
#• Calculate and interpret the explained variance at level 2

# add time-invariant gender + highgpa (gender is binary so no centering)
## when they are continuous variable, think of grand-mean centering them!
dat$highgpa <- dat$highgpa - mean(dat$highgpa)

m4 <- lmer(gpa ~ 1 + time + job + sex + highgpa + (1|student), REML = FALSE, data= longdat)
summary(m4)

# check if sex and highgpa are significant predictors
anova(m4, m3)

# compute R2 ONLY at level 2
# R2.lv1 <- (0.058 - 0.055) / 0.058
R2.lv2 <-  (0.063 - 0.045) / 0.063 


## 11. For the time-varying predictors (level1), check if the slopes are fixed or random.
#• Start with the slope of Job: check if the variance of the slope for Job is significant. I
#• Now turn to the variable Time: check if the variance of the slope for Time is significant. 

## m5: add random slopes

# check "job" slope variance significance
m5a <- lmer(gpa ~ 1 + time + job + sex + highgpa + (1 + job|student), REML = FALSE, data= longdat) #### boundary (singular) fit: see help('isSingular')****** model does not converge!!! make sure you see this message in the output!!
summary(m5a) 

# check "time" slope variance significance
m5b <- lmer(gpa ~ 1 + time + job + sex + highgpa + (1 + time|student), REML = FALSE, data= longdat)
summary(m5b)

# variance of slope for time is significant
## DIVIDE P-VAL / 2 AS YOU ARE TESTING RANDOM SLOPE VARIANCE 
anova(m5b, m4)  ## MAKE SURE YOU ARE COMPARING WITH THE CORRECT MODEL: compare with m4

## 12. Check if Gender can (partly) explain why the trajectory over time differs between students. That is, we include Gender as a predictor of the slope for Time.
#• Calculate and interpret the explained slope variance.

## m6: add cross-level interaction 
## MAKE SURE TO INCLUDE ALL LEVEL2 PREDICTORS, INCLUDING THE NON-SIGNIFICANT ONES!

m6a <- lmer(gpa ~ 1 + time + job + sex + highgpa + sex*time + highgpa*time + (1 + time|student), REML = FALSE, data= longdat) #highgpa*time not significant
summary(m6a)

m6b <- lmer(gpa ~ 1 + time + job + sex + highgpa + sex*time + (1 + time|student), REML = FALSE, data= longdat)
summary(m6b)

# compare with m5b***: make sure you are comparing with the correct model
anova(m6b, m5b)

# compute R2 for random slope
R2.slope <- (0.003837-0.003614)/0.003837;R2.slope

######################
## interaction plot ##
######################

time <- 0:5
plot(x = time, ylim = c(1,4), xlim = c(0,5), type = "n", xlab = "time", ylab = "gpa")
col.sex <- c("cadetblue", "coral")
abline(a = m6b@beta[1], b = m6b@beta[2], col = col.sex[1], lwd = 2) # intercept= overall intercept, slope = time coef

# girls start out higher and proceed faster over time:  
abline(a = m6b@beta[1] + m6b@beta[4], b = m6b@beta[2] + m6b@beta[6],
       col = col.sex[2], lwd = 2)  # intercept= overall intercept + sex coef, slope = time coef + interaction coef

legend("topleft", bty = "n", lwd = 2, col = col.sex,
       legend = c("boys", "girls"))


## 13. Check the normality assumption for the level1 residuals and level2 intercept & slope errors

# level1
qqnorm(residuals(m6b))
qqline(residuals(m6b), col="coral")

# level2
resid.lv2 <- ranef(m6b)$student

par(mfrow=c(1,2))
# intercept residuals
qqnorm(resid.lv2[,1], main="lv2_intercept")
qqline(resid.lv2[,1], col = "coral")

# slope residuals
qqnorm(resid.lv2[,2], main="lv2_slope")
qqline(resid.lv2[,2], col="coral")



### Final check 
library(texreg)
screenreg(list(m0, m1, m2, m3, m4, m5b, m6b)) ##### lastly, double check whether everything is correct !!
