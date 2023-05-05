# Lecture 7: Example 1,
# PM10-particles in Oslo, 24/4-23:####
# Logistic regression####
# estimates and confidence intervals

    library(ggplot2)
#Requires library MASS installed but not necessarily activated.
#library(MASS)

##pm10-data####
load("lectures/data/pm10.rda")
head(pm10)
# Some extra variables used in lecture 8.

## highpm10 against number of cars####
ggplot(pm10, aes(cars, highpm10)) +
  geom_point() +
  xlab("number of cars") +
  ylab("High PM10") +
  labs(title = "High PM10 (=1) or Not high PM10 (=0) vs number of cars") +
  theme(text = element_text(size = 14))

## add moving average####
ggplot(pm10, aes(cars, highpm10)) +
  geom_point() +
  geom_smooth() +
  xlab("number of cars") +
  ylab("High PM10") +
  labs(title = "High PM10 (=1) or Not high PM10 (=0) vs number of cars") +
  theme(text = element_text(size = 14))

#Estimate the model####
glm(highpm10 ~ cars, family = "binomial", data = pm10)
# Note the small beta1 = effect of 1 car.
# I will use cars/1000 as x-variable instead of cars.
# This is both for numerical reasons and for interpretation 
# of parameters.
(model.1 <- glm(highpm10 ~ I(cars/1000), family = "binomial", data = pm10))
# Note that beta for I(cars/1000) = 1000*beta for cars

summary(model.1)
# Look! our friend AIC.
# But no R2, residual standard error or F-test.
# Instead we have something called "deviance".
# Deviance = -2*loglikelihood. More on this next lecture.

## beta: log-odds(ratio)####
model.1$coefficients
## ci(beta): profile likelihood####
(ci.beta <- confint(model.1))

##ci(beta): Wald####
# Wald based c.i. "by hand":
bhat <- model.1$coefficients
se.b <- summary(model.1)$coefficients[, "Std. Error"]
(ci.wald <- cbind(lo = bhat - 1.96*se.b, hi = bhat + 1.96*se.b))
#Small difference between the versions due to large data set.
summary(model.1)
# Odds and Odds Ratio####
# exp(beta0), exp(beta1)
exp(model.1$coefficients)
(ci.or <- exp(ci.beta))
#Wald version:
exp(ci.wald)
#Small difference between the versions due to large data set.

# Predict for plotting####
## phat####
# = estimated probabilities p
pm10.pred <- cbind(
  pm10,
  phat = predict(model.1, type = "response"))

ggplot(pm10.pred, aes(cars, highpm10)) +
  geom_point() +
  geom_smooth(se = FALSE, linetype = "dashed") +
  geom_line(aes(y = phat), color = "red", size = 1) +
  xlab("number of cars") +
  ylab("High PM10") +
  labs(title = "High PM10 (=1) or Not high PM10 (=0) vs number of cars",
       caption = "red = fitted line, blue dashed = moving average") +
  theme(text = element_text(size = 14))

## logit####
# = logodds with s.e. for constructing C.I.
pm10.pred <- cbind(
  pm10.pred,
  logit = predict(model.1, se.fit = TRUE))
head(pm10.pred)
# An unnecessary variable:
pm10.pred$logit.residual.scale <- NULL

## CI for logit (Wald)####
# Wald ok here since we do not want to test 
# and there is no feasible alternative method!
# Standard normal quantile:
(lambda <- qnorm(1 - 0.05/2))
pm10.pred$logit.lwr <- pm10.pred$logit.fit - lambda*pm10.pred$logit.se.fit
pm10.pred$logit.upr <- pm10.pred$logit.fit + lambda*pm10.pred$logit.se.fit
head(pm10.pred)

## CI for odds####
pm10.pred$odds.lwr <- exp(pm10.pred$logit.lwr)
pm10.pred$odds.upr <- exp(pm10.pred$logit.upr)
head(pm10.pred)

## CI for p####
pm10.pred$p.lwr <- pm10.pred$odds.lwr/(1 + pm10.pred$odds.lwr)
pm10.pred$p.upr <- pm10.pred$odds.upr/(1 + pm10.pred$odds.upr)
head(pm10.pred)

## plot intervals####
ggplot(pm10.pred, aes(cars, highpm10)) +
  geom_point() +
  geom_line(aes(y = phat), color = "red", size = 1) +
  geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.2) +
  xlab("number of cars") +
  ylab("High PM10") +
  labs(title = "High PM10 (=1) or Not high PM10 (=0) vs number of cars",
       caption = "red = fitted line, with 95% confidence interval") +
  theme(text = element_text(size = 14))

# Wald test for betas####
summary(model.1)$coefficients
# Since |4.92| > lambda_0.025 = 1.96 we can reject
# H0: beta_1 = 0
# Alt. Since
# P(|N(0,1)| > 4.92) = 2*P(N(0,1) > 4.92) = 8.7*10^(-7) < 0.05
# we can reject H0.
# The number of cars (or, rather, the number of thousands of cars)
# has a significant impact on the probability of a high
# concentration of PM10-particles.

# LR-test for beta for cars####
#See Lecture 8.
# update(model, . ~ . - x) means 
# update model by using original y and x variables, but remove variable x.
# We could have used glm(highpm10 ~ 1, family = "binomial", data = pm10) here.
(lrcars <- anova(update(model.1, . ~ . - I(cars/1000)), model.1))
pchisq(lrcars[2, "Deviance"], lrcars[2, "Df"], lower.tail = FALSE)
# Slightly different P-value but same conclusion as the Wald test.
