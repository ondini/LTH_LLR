# Lecture 11: Multinomial logistic regression####
# 10/5-23: Example 1: program application

# plotting
library(ggplot2)
# Necessary for multinomial regression:
library(nnet)

#Data####
load("Data/hsbdemo.rda")
head(hsbdemo)
# prog: type of program student applies to
# ses: socio-economic status
# read, write, math, science, socst: grades
summary(hsbdemo)

##Set reference categories####
### vocation as reference y-category####
hsbdemo$prog <- relevel(hsbdemo$prog, ref = "vocation")

### Middle class as reference####
hsbdemo$ses <- relevel(hsbdemo$ses, ref = "middle")

#Fit some models####
##Null model for comparisons####
(model.null <- multinom(prog ~ 1, data = hsbdemo))
(sum.null <- summary(model.null))

##Math model for comparisons####
(model.math <- multinom(prog ~ math, data = hsbdemo))
(sum.math <- summary(model.math))

##Full model for upper limit in stepwise####
(model.full <- multinom(prog ~ female + ses + schtyp + read +
                          write + math + science + socst +
                          honors + awards, data = hsbdemo))
(sum.full <- summary(model.full))

#Stepwise variable selection####
##using AIC####
model.final <- step(model.null,
                  scope = list(upper = model.full),
                  direction = "both")
(sum.final <- summary(model.final))

##using BIC####
model.grades <- step(model.null,
                     scope = list(upper = model.full, lower = model.null),
                     direction = "both",
                  k = log(nrow(hsbdemo)))
model.grades

#Parameters####
## Beta, se, z-value and Wald test P-value####
(beta <- sum.final$coefficients)
(se.beta <- sum.final$standard.errors)
(z.value <- beta/se.beta)
(P.value <- pnorm(abs(z.value), lower.tail = FALSE))

##odds ratios####
(OR <- exp(beta))
OR["general", ]
OR["academic", ]

## Confidence intervals for OR####
ci <- exp(confint(model.final))
# a 3-dimensional matrix!
ci

## Intervals for specific parameter####
ci["(Intercept)", , ]

### lower or upper limits####
ci[, "2.5 %", ]
ci[, "97.5 %", ]

### by program####
ci[, , "general"]
ci[, , "academic"]

cbind(
  beta = round(beta["general", ], digits = 2),
  P.value = round(P.value["general", ], digits = 3),
  OR = round(OR["general", ], digits = 2),
  round(ci[, , "general"], digits = 2))

cbind(
  beta = round(beta["academic", ], digits = 2),
  P.value = round(P.value["academic", ], digits = 3),
  OR = round(OR["academic", ], digits = 2), 
  round(ci[, , "academic"], digits = 2))

# LR tests####
# For each of the variables
# Note that each variable has 2 beta-parameters,
# one for each non-reference y-category.
anova(update(model.final, . ~ . - math), model.final)
anova(update(model.final, . ~ . - socst), model.final)
anova(update(model.final, . ~ . - science), model.final)
anova(update(model.final, . ~ . - schtyp), model.final)
anova(update(model.final, . ~ . - ses), model.final)

#Plot####
##Create plot data####
# range of math grades, repeated twice, 
# for public and private school
x0 <- data.frame(math = rep(seq(33, 75), 2))
# the same ses, socst and science for all:
x0$ses <- "middle"
x0$socst <- mean(hsbdemo$socst)
x0$science <- mean(hsbdemo$science)
# public and private school:
x0$schtyp <- c(rep("public", 75 - 33 + 1), 
               rep("private", 75 - 33 + 1))

##Predict probabilities####
pred.x0 <- cbind(
  x0,
  predict(model.final, newdata = x0, type = "probs"))
head(pred.x0)

##Plot probabilities####
ggplot(pred.x0, aes(x = math)) +
  geom_line(aes(y = academic, color = "Academic"), linewidth = 2) +
  geom_line(aes(y = general, color = "General"), linewidth = 2) +
  geom_line(aes(y = vocation, color = "Vocation"), linewidth = 2) +
  expand_limits(y = c(0, 1)) +
  facet_grid(~ schtyp) +
  labs(title = "Multinomial: average student with varying math grades and school type",
       y = "probability",
       fill = "Program") +
  theme(text = element_text(size = 14))

##Plot stacked probabilities####
ggplot(pred.x0, aes(x = math)) +
  geom_ribbon(aes(ymin = 0, ymax = academic, fill = "Academic")) +
  geom_ribbon(aes(ymin = academic, ymax = academic + general,
                  fill = "General")) +
  geom_ribbon(aes(ymin = academic + general, ymax = 1,
                  fill = "Vocation")) +
  facet_grid(~ schtyp) +
  labs(title = "Multinomial: average student with varying math grades and school type",
       y = "probability",
       fill = "Program") +
  theme(text = element_text(size = 14))

# LR (deviance) tests####
## against null model####
anova(model.null, model.final)
## against math model####
anova(model.math, model.final)
## against grade model####
anova(model.grades, model.final)
## against full model####
anova(model.final, model.full)

# AIC etc####
info <- cbind(aic = AIC(model.null, model.math, model.grades, model.final, model.full),
              bic = BIC(model.null, model.math, model.grades, model.final, model.full))
info$r2 <- round(100*c(
  0, 
  1 - model.math$deviance/model.null$deviance,
  1 - model.grades$deviance/model.null$deviance,
  1 - model.final$deviance/model.null$deviance,
  1 - model.full$deviance/model.null$deviance), digits = 1)
info$r2.adj <- round(100*c(
  0,
  1 - (model.math$deviance + (model.math$edf - model.null$edf))/model.null$deviance,
  1 - (model.grades$deviance + (model.grades$edf - model.null$edf))/model.null$deviance,
  1 - (model.final$deviance + (model.final$edf - model.null$edf))/model.null$deviance,
  1 - (model.full$deviance + (model.full$edf - model.null$edf))/model.null$deviance),
  digits = 1)
info

#Goodness-of-fit####
##Estimated prob and category####
predict(model.final, type = "prob")
# predicted category
predict(model.final)
predict(model.final, type = "class")

pred.final <- cbind(
  hsbdemo,
  predict(model.final, type = "probs"),
  yhat = predict(model.final))
head(pred.final)

##Confusion matrix####
table(pred.final$prog)
table(pred.final$yhat)

(conf.final <- table(pred.final$prog, pred.final$yhat))

## Sensitivities####
round(100*diag(conf.final)/table(pred.final$prog), digits = 1)
##Precisions####
round(100*diag(conf.final)/table(pred.final$yhat), digits = 1)
##Accuracy####
100*sum(diag(conf.final))/sum(conf.final)
