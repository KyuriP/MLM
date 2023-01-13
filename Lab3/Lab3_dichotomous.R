library(lme4)
library(lmerTest)
library(jtools) ## odds-ratio double check --> convenient

setwd("~/Desktop/MLM/Lab3")

# load the data
Thai <- read.csv("UTHAI2.csv")
head(Thai)
summary(Thai)

## At which level is each variable?
# rep1 --> DV (dichotomous***)
# male, pped --> level1 (pupil level)
# msesc --> level2 predictor (school level)

## Which variable is the level 2 identification variable?
# "schoolid"

# # specify the null model : we don't do this not accurate.
# M0 <- glm(rep1 ~ 1, data = Thai, family=binomial)
# summary(M0)

## M1 : random intercept model (use "glmer")
M1 <- glmer(rep1 ~ 1 + (1|schoolid), family=binomial, nAGQ = 10, data=Thai)
summary(M1)
# anova(M0, M1)

# ICC 
ICC = 1.726 / (1.726 + 3.29);ICC   ## residual variance at the lowest level is fixed to 3.29 !!!

## M2: model with the level 1 predictors
M2 <- glmer(rep1 ~ 1  + male + pped + (1|schoolid), family=binomial, nAGQ = 10, data=Thai)
summary(M2)
anova(M2, M1)

## jtools give you the odds ratio: so it will take the exponent (specify "exp=T")
summ(M2, exp=T)

## calculating the variance of linear predictor of fixed part: "sigma^2_F" using the regression equation for M2
LP <- (-2.24) + (0.54*Thai$male) + (-0.64*Thai$pped)
sig2_F <- var(LP)

sig2_u0 <- 1.697 # from the output
sig2_R <- 3.29  # fixed

## explained variance = sigma2_F / (sigma2_F + sigma2_u0 + sigma2_R)
## tells us how much variance in the model we can explain with the fixed part of the model; what we are explaining with the linear predictor of fixed part our of the total variance we actually have in our data.
Exp.var <- sig2_F / (sig2_F + sig2_u0 + sig2_R)

## unexplained variance
Unexp.var.lv1 <- sig2_R / (sig2_F + sig2_u0 + sig2_R) # unexplained variance at level1
Unexp.var.lv2 <- sig2_u0 / (sig2_F + sig2_u0 + sig2_R) # unexplained variance at level2


## M3: model with the level 2 predictors
Thai$msesc <- Thai$msesc - mean(Thai$msesc) # grand-mean center the continuous predictor

M3 <- glmer(rep1 ~ 1  + male + pped + msesc + (1|schoolid), family=binomial, nAGQ = 10, data=Thai)
summary(M3)

summ(M3, exp=T)
anova(M3, M2)

## calculating the variance of sigma^2_F using the regression equation for M2
## normally we wouldn't compute the explained variance, as "mses" is not significant. so below is just for the sake of practice.
LP2 <- (-2.24164) + (0.53525 *Thai$male) + (-0.62708*Thai$pped) + (-0.29565*Thai$msesc)
sig2_F <- var(LP2)
sig2_u0 <- 1.686
sig2_R <- 3.29
# explained variance = sigma2_F / (sigma2_F + sigma2_u0 + sigma2_R)
Exp.var <- sig2_F / (sig2_F + sig2_u0 + sig2_R)

# (un)explained variance
Unexp.var.lv1 <- sig2_R / (sig2_F + sig2_u0 + sig2_R)
Unexp.var.lv2 <- sig2_u0 / (sig2_F + sig2_u0 + sig2_R)


## M4: add random slopes
M4a <- glmer(rep1 ~ 1  + male + pped +  (1 + male|schoolid), family=binomial, nAGQ = 1, data=Thai)
summary(M4a)
summ(M4a, exp=TRUE)
anova(M4a, M2) # not significant

# not converging
#M4b <- glmer(rep1 ~ 1  + male + pped + msesc + (1 + pped|schoolid), family=binomial, nAGQ = 10, data=Thai)


#### you don't calculate R2 when adding random slopes!!!!!! ***********
# LP3 <- (-2.19) + (0.44*Thai$male) + (-0.64*Thai$pped) 
# sig2_F <- var(LP3)
# sig2_u0 <- 1.474
# sig2_R <- 3.29

# # explained variance = sigma2_F / (sigma2_F + sigma2_u0 + sigma2_R)
# Exp.var <- sig2_F / (sig2_F + sig2_u0 + sig2_R)
# 
# # (un)explained variance
# Unexp.var.lv1 <- sig2_R / (sig2_F + sig2_u0 + sig2_R)
# Unexp.var.lv2 <- sig2_u0 / (sig2_F + sig2_u0 + sig2_R)



#################################
########## BETH says: ###########
#################################

#### Interpret and give an overall results --> meaning that you interpret everything that's going in on in that model. It includes any correlation, your fixed effects and random coefficients.

#### When you are asked to report ICC, please interpret ICC as well!  What ICC means. ICC definition changes a bit when you look at cross-sectional or longitudinal data. Also there are two ways of interpretations of ICC.

#### qualitative + quantitative interpretation by APA styling report! including degrees of freedom for parameters, p-values and whatnots. 


# ***Tips: 
# 1. color code which one is not significant 
# 2. if they are grand mean centered, mention they are in the name
# 3. make sure you compare with the CORRECT model!! Which model is nested in what! 
# 4. In longitudinal model, you want to keep the TIME variable!!
# 5. Make sure you point out what is your final model!!  Highlight your final model and AIC.
# 6. In the exam, make sure you include all the models you tested.
# 7. the model that did not converge, mention that the model did not converge (and if time is allowed, fill out the estimate).
# 8. make sure the column is correctly specified. Ex) covariance correlation, etc. etc.
