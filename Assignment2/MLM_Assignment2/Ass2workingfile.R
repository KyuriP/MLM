library(dplyr)
library(tidyr)

curran_wide <- read.csv("curran_wide.csv")
head(curran_wide)
glimpse(curran_wide)
# not using homeo and children's gender

summary(curran_long)


curran_long <- curran_wide %>% 
  # not using homeo and childs' gender
  select(-c(homeemo,sex)) %>% 
  # convert it to a long format
  pivot_longer(!c(id, momage, homecog), names_to = c(".value", "time"),
               names_pattern = "(anti|read)(.)")  %>% 
  # re-code time : from 1:4 to 0:3
  mutate(time = as.numeric(time) - 1) %>% 
  # re-order the columns
  relocate(time, anti, read, .after=id)




## check for linearity and outliers
# level1 
ggplot(curran_long,
       aes(x = time, y = anti)) +
  geom_point() +
  geom_smooth(method = "lm",
              aes(color = "linear"),
              se = FALSE) +
  geom_smooth(method = "lm",
              formula = y ~ x + I(x^2),
              aes(color = "quadratic"),
              se = FALSE) +
  theme_minimal() +
  #xlab("Time") +
  #ylab("Antisocial behavior")+
  ggtitle("Level 1 (occasion level)")+
  theme(plot.title = element_text(size = 10,hjust=0.5))

ggplot(curran_long,
       aes(x = time, y = anti)) +
  geom_jitter() +
  geom_smooth(method = "lm",
              aes(color = "linear"),
              se = FALSE) +
  geom_smooth(method = "lm",
              formula = y ~ x + I(x^2),
              aes(color = "quadratic"),
              se = FALSE) +
  theme_minimal() +
  #xlab("Time") +
  #ylab("Antisocial behavior")+
  ggtitle("Level 1 (occasion level)")+
  theme(plot.title = element_text(size = 10,hjust=0.5))

ggplot(curran_long,
       aes(x = read, y = anti)) +
  geom_jitter(shape=1, cex=0.5) +
  geom_smooth(method = "lm",
              aes(color = "linear"),
              se = FALSE) +
  geom_smooth(method = "lm",
              formula = y ~ x + I(x^2),
              aes(color = "quadratic"),
              se = FALSE) +
  theme_minimal() +
  #xlab("Time") +
  #ylab("Antisocial behavior")+
  ggtitle("Level 1 (occasion level)")+
  theme(plot.title = element_text(size = 10,hjust=0.5))

# level2 
curran_long %>% 
  group_by(id) %>% 
  # aggregate the exam scores and store'em as : Examscore_aggr
  mutate(aggregated_anti = mean(anti)) %>% 
  ggplot(aes(x = momage, y = aggregated_anti)) +
  geom_point() +
  geom_smooth(method = "lm",
              aes(color = "linear"),
              se = FALSE) +
  geom_smooth(method = "lm",
              formula = y ~ x + I(x^2),
              aes(color = "quadratic"),
              se = FALSE) +
  theme_minimal()+
  #xlab("School Reading ability") +
  #ylab("School avg. exam score")+
  ggtitle("Level 2 (person level)")+
  theme(plot.title = element_text(size = 10, hjust=0.5))

curran_long %>% 
  group_by(id) %>% 
  # aggregate the exam scores and store'em as : Examscore_aggr
  mutate(aggregated_anti = mean(anti)) %>% 
  ggplot(aes(x = homecog, y = aggregated_anti)) +
  geom_point() +
  geom_smooth(method = "lm",
              aes(color = "linear"),
              se = FALSE) +
  geom_smooth(method = "lm",
              formula = y ~ x + I(x^2),
              aes(color = "quadratic"),
              se = FALSE) +
  theme_minimal()+
  xlab("momage") +
  ylab("anti")+
  ggtitle("Level 2 (person level)")+
  theme(plot.title = element_text(size = 10, hjust=0.5))


# model 1: random intercept model
model1 <- lmer(anti ~ 1 + (1|id), REML = FALSE, data= curran_long)
summary(model1)

# model2: add time predictor ((benchmark model for R2))
model2 <- lmer(anti ~ 1 + time + (1|id), REML = FALSE, data= curran_long)
summary(model2)
anova(model2, model1)

# model3: add time-varying predictor, read
model3 <- lmer(anti ~ 1 + time + read + (1|id), REML = FALSE, data= curran_long)
summary(model3)
anova(model3, model2)


library(texreg)
screenreg(list(model2, model3))
library(jtools)
summ(model3)


# model4: add time-invariant predictors, momage & homecog
model4 <- lmer(anti ~ 1 + time + read + momage + homecog + (1|id), REML = FALSE, data= curran_long)
summary(model4)
anova(model4, model3)

## DO I REMOVE read since it is not sig?
model4a <- lmer(anti ~ 1 + time + momage + homecog + (1|id), REML = FALSE, data= curran_long)
summary(model4a)
anova(model4a, model3)

# model5: add random slopes
model5a <- lmer(anti ~ 1 + time + momage + homecog + (1+time|id), REML = FALSE, data= curran_long)
summary(model5a)
anova(model5a, model4a)

model5b <- lmer(anti ~ 1 + time + momage + homecog + read + (1+read|id), REML = FALSE, data= curran_long)
summary(model5b)
anova(model5b, model4a)

model5c <-  lmer(anti ~ 1 + time + momage + homecog + read + (1+time +read|id), REML = FALSE, data= curran_long)
summary(model5c)
anova(model5c, model4a)

# model6: add cross-level interaction
# check if gender can explain the time variance
model6a <- lmer(anti ~ 1 + time + momage + homecog + time*momage + (1+time|id), REML = FALSE, data= curran_long)
summary(model6a)

# check if gender can explain the time variance
model6b <- lmer(anti ~ 1 + time + momage + homecog + time*homecog + (1+time|id), REML = FALSE, data= curran_long)
summary(model6b)

