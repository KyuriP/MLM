######################################
### Cross-sectional data - popular ###
######################################

library(lme4)
library(lattice)
library(lmerTest)
library(ggplot2)
library(jtools)
library(reghelper)

setwd("~/Desktop/MLM/Lab1")

# load the data
popular <- read.csv(file = "popular.csv")
# always nice to look at the data to start with       
head(popular)
## which variables are in which level?
# - level1: extrav, gender
# - level2: texp
# - DV : popular
# - grouping variable : class

# We can view some descriptive statistics with summary ()
summary(popular)


# we can also create scatterplots to view linear relations and outliers using ggplot2
## level1
ggplot(popular,
       aes(x = extrav, y = popular)) +
  geom_point() +
  geom_smooth(method = "lm",
              aes(color = "linear"),
              se = FALSE) +
  geom_smooth(method = "lm",
              formula = y ~ x + I(x^2),
              aes(color = "quadratic"),
              se = FALSE) +
  theme_minimal()

## level2
popular %>% 
  # aggregate the exam scores and store'em as : Examscore_aggr
  group_by(class) %>% 
  mutate(pop_aggr = mean(popular)) %>% 
  ggplot(aes(x = texp, y = pop_aggr)) +
  geom_point() +
  geom_smooth(method = "lm",
              aes(color = "linear"),
              se = FALSE) +
  geom_smooth(method = "lm",
              formula = y ~ x + I(x^2),
              aes(color = "quadratic"),
              se = FALSE) +
  theme_minimal()

## alternatively, using aggreagate() 
## • For level 2 predictors, this means the dependent variable needs to be aggregated to the cluster means
popular_aggr <- aggregate(popular, list(popular$class), mean)

ggplot(popular_aggr,
       aes(x = texp, y = popular)) +
  geom_point() +
  geom_smooth(method = "lm",
              aes(color = "linear"),
              se = FALSE) +
  geom_smooth(method = "lm",
              formula = y ~ x + I(x^2),
              aes(color = "quadratic"),
              se = FALSE) +
  theme_minimal()

# for whole classes
#xyplot(popular~extrav | class, groups=class, data=popular, type=c('p','r'), auto.key=F)

# for the first 25 classes:
xyplot(popular~extrav | class, groups=class, data=popular[popular$class %in% 1:25,],
       type=c('p','r'), auto.key=F)


############################
### Specify the models   ###
############################

## Model0: single-levle model (using lm())
M0 <- lm(popular ~ 1, popular)
summary(M0)

## Model1: check if MLM is necessary 
M1 <- lmer(popular ~ 1 + (1|class), REML = FALSE, data=popular) # dont forget "REML = FALSE"
summary(M1) ## DONT FORGET TO DIVIDE P-VALUE BY 2!! as you are testing the random intercept

# compute ICC with Model1
ICC =  0.6945 /  (0.6945 + 1.2218); ICC

# jtools::summ gives ICC: you can double check if you got it correctly.
jtools::summ(M1)  

# compare models
anova(M1, M0)  # (null model is on the right)

# BENCHMARK model(M1): determine the variances on level1 and level2
M1var.lv1 <- as.data.frame(VarCorr(M1))[2,4]
M1var.lv2 <- as.data.frame(VarCorr(M1))[1,4]


options(scipen = 999) #scientific notation off
## Model2: add level1 predictors
M2 <- lmer(popular ~ gender + extrav + (1|class), REML=FALSE, data=popular)
summary(M2)
summ(M2)   # summ() gives round up result, sometimes easier to read.

# compare the models
anova(M2, M1)

# determine the variances on level1 and level2
M2var.lv1 <- as.data.frame(VarCorr(M2))[2,4]
M2var.lv2 <- as.data.frame(VarCorr(M2))[1,4]

# R2 on level1
R2.lv1.M2 <- (M1var.lv1 - M2var.lv1) / M1var.lv1

# R2 on level2
R2.lv2.M2 <- (M1var.lv2 - M2var.lv2) / M1var.lv2

## Model3: add level2 predictor
M3 <- lmer(popular ~ 1 + gender + extrav + texp + (1|class), REML=FALSE, data=popular)
summary(M3)

# compare the models
anova(M3, M2)

# determine the variances on level1 and level2
M3var.lv1 <- as.data.frame(VarCorr(M3))[2,4]
M3var.lv2 <- as.data.frame(VarCorr(M3))[1,4]

## R2 on level1 (NOPE!! WE don't calculate R2 on level1 when adding level2 predictors. THEY SHOULD BE THE SAME ESSEENTIALLY)
# R2.lv1.M3 <- (M1var.lv1 - M3var.lv1) / M1var.lv1

# ONLY R2 AT level2 !!
R2.lv2.M3 <- (M1var.lv2 - M3var.lv2) / M1var.lv2


# Model4: add random slope (&covariance between intercept and slope)
## ESSENTIALLY CHECKING ALL LEVEL1 PREDICTORS FOR RANDOM SLOPE
M4 <- lmer(popular ~ 1 + extrav + gender + texp + (extrav|class), REML = FALSE, data=popular)
summary(M4)

# compare the models: DIVIDE P-VALUE/2 as you're testing random slope!!
anova(M4, M3)

# determine the variances on random slopes: later to compute the R2 for random slope!!
M4var.rs <- as.data.frame(VarCorr(M4))[2,4]


## Model 5: add cross-level interaction
## CHECK ALL LEVEL2 PREDICTORS FOR CROSS-LEVEL INTERATCION
M5 <- lmer(popular ~1 + extrav + gender + texp + extrav*texp + (extrav|class), REML = FALSE, data = popular)
summary(M5)

# compare the models
anova(M5, M4)

# determine the variances on level1 and level2
M5var.rs <- as.data.frame(VarCorr(M5))[2,4]

# R2 for random slope of extraversion
R2.rs <- (M4var.rs - M5var.rs) / M4var.rs; R2.rs


##################################################
### Examine normality assumptions on residuals ### 
##################################################
# level 1 residual error: qqnorm() on the final ML model residuals
qqnorm(residuals(M5)) 

# intercept variance: extract cluster-specific deviances to the intercept
qqnorm(ranef(M5)$class[[1]])
qqline(ranef(M5)$class[[1]], col = "coral")

# slope variance: extract cluster-specific deviances to the slope
qqnorm(ranef(M5)$class[[2]])
qqline(ranef(M5)$class[[2]], col = "coral")


######################################
# spit out nice output of all models #
######################################
library(texreg)

screenreg(list(M0, M1, M2, M3, M4, M5)) ##### lastly, double check whether everything is correct !!
