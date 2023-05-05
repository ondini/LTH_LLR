# Lecture 8, Example 1
# PM10-particles in Oslo, 25/4-23:
# Logistic regression: LR-test, residuals, pseudo R2####

library(ggplot2)
#library(MASS)

load("lectures/data/pm10.rda")
head(pm10)
summary(pm10)

#Fit models####
## Null model####
(model.0 <- glm(highpm10 ~ 1, family = "binomial", data = pm10))
## Model 1: cars####
(model.1 <- glm(highpm10 ~ I(cars/1000), family = "binomial", data = pm10))
## Model.oslo: cars*windspeed + tempdiff####
(model.oslo <- glm(highpm10 ~ I(cars/1000)*windspeed + tempdiff,
                   family = "binomial", data = pm10))
(sum.oslo <- summary(model.oslo))
(beta.oslo <- cbind(model.oslo$coefficients, ci = confint(model.oslo)))
round(exp(beta.oslo), digits = 2)

#Compare with null model####
## Average Y####
mean(pm10$highpm10)
## or prediction in the null model####
predict(model.0, data.frame(x = NA), type = "response")

## compare Oslo model with Null model using the summary output####
(D_diff <- sum.oslo$null.deviance - sum.oslo$deviance)
(df_diff <- sum.oslo$df.null - sum.oslo$df.residual)

## compare with Null model using the anova funktion####
(anova.0.oslo <- anova(model.0, model.oslo))
(D_diff <- anova.0.oslo$Deviance[2])
(df_diff <- anova.0.oslo$Df[2])

###chi2-quantile to compare D_diff with####
qchisq(1 - 0.05, df_diff)
### or P-value####
pchisq(D_diff, df_diff, lower.tail = FALSE)

# Compare with model with only tempdiff####
(model.red <- glm(highpm10 ~ tempdiff, family = "binomial", data = pm10))
(sum.red <- summary(model.red))

(D_diff <- sum.red$deviance - sum.oslo$deviance)
(df_diff <- sum.red$df.residual - sum.oslo$df.residual)
# or
(anova.red.oslo <- anova(model.red, model.oslo))
(D_diff <- anova.red.oslo$Deviance[2])
(df_diff <- anova.red.oslo$Df[2])

qchisq(1 - 0.05, df_diff)
pchisq(D_diff, df_diff, lower.tail = FALSE)

# Leverage####
##calculate####
pm10.pred <- cbind(pm10,
                   xb = predict(model.oslo),
                   v = influence(model.oslo)$hat)
head(pm10.pred)

##plot####
(plot.v <- ggplot(pm10.pred, aes(xb, v)) + 
    geom_point() +
    geom_hline(yintercept = 2*length(model.oslo$coefficients)/nrow(pm10), 
               color = "red", size = 1) +
    facet_wrap(~ highpm10) +
    labs(title = "Leverage vs linear predictor, by Y=0 or Y=1",
         caption = "2(p+1)/n in red") +
    theme(text = element_text(size = 14)))

##highlight unusually large ones (arbitrary choice)####
I.highv <- which(pm10.pred$v > 0.045)
plot.v +
  geom_point(data = pm10.pred[I.highv, ], size = 3, 
             color = "red", shape = 24) +
  geom_hline(yintercept = 0.045, linetype = "dotted", size = 1) +
  labs(title = "Leverage vs linear predictor, by Y=0 or Y=1",
       caption = "red = 2(p+1)/n, black = 0.045")

ggplot(pm10.pred, aes(cars, v)) + 
  geom_point() +
  geom_point(data = pm10.pred[I.highv, ], size = 3, 
             color = "red", shape = 24) +
  facet_wrap(~ tempdiff) +
  labs(title = "Leverage vs cars, by temp diff",
       caption = "red = 2(p+1)/n, black = 0.045") +
  geom_hline(yintercept = 2*6/500, color = "red", size = 1) +
  geom_hline(yintercept = 0.045, linetype = "dotted", size = 1) +
  theme(text = element_text(size = 14))

ggplot(pm10.pred, aes(windspeed, v)) +
  geom_point() + 
  geom_point(data = pm10.pred[I.highv, ], size = 3, 
             color = "red", shape = 24) +
  facet_wrap(~ tempdiff) +
  geom_hline(yintercept = 2*6/500, color = "red", size = 1) +
  geom_hline(yintercept = 0.045, linetype = "dotted", size = 1) +
  labs(title = "Leverage vs wind speed, by temp diff",
       caption = "red = 2(p+1)/n, black = 0.045") +
  theme(text = element_text(size = 14))

# Use facet_grid to split rows and columns by different variables:
ggplot(pm10.pred, aes(cars, windspeed)) +
  geom_point() +
  geom_point(data = pm10.pred[I.highv, ], color = "red",
             shape = 24, size = 3) +
  facet_grid(rows = vars(highpm10), cols = vars(tempdiff)) +
  labs(title = "wind speed vs cars, by Y=0 or Y=1 and by temp diff",
       caption = "rows: Y = 0 or 1, columns: tempdiff") +
  theme(text = element_text(size = 14))

# Residuals####
## Pearson residuals####
pm10.pred$pearson <- influence(model.oslo)$pear.res
## Standardizes residuals####
pm10.pred$stdres <- pm10.pred$pearson/sqrt(1 - pm10.pred$v)
head(pm10.pred)

ggplot(pm10.pred, aes(sample = stdres)) +
  geom_qq() + geom_qq_line() +
  labs(title = "Q-Q-plot standardized residuals") +
  theme(text = element_text(size = 14))

# The as.factor(highpm10) prevents ggplot from using a 
# colour spectrum and instead use default color number 1 and 2.
ggplot(pm10.pred, aes(xb, stdres,
                      color = as.factor(highpm10))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed",
             size = 1) +
  geom_hline(yintercept = c(-3, 3), linetype = "dotted",
             size = 1) +
  labs(title = "Standardized residuals vs linear predictor",
       color = "Y") +
  theme(text = element_text(size = 14))

ggplot(pm10.pred, aes(xb, stdres^2, color = as.factor(highpm10))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 4, linetype = "dashed", size = 1) +
  geom_hline(yintercept = 9, linetype = "dotted", size = 1) +
  labs(title = "Squared standardized residuals vs linear predictor",
       color = "Y") +
  theme(text = element_text(size = 14))

## Deviance residuals, standardised####
pm10.pred$devres <- influence(model.oslo)$dev.res
pm10.pred$devstd <- pm10.pred$devres/sqrt(1 - pm10.pred$v)
head(pm10.pred)

ggplot(pm10.pred, aes(xb, devstd, color = as.factor(highpm10))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), linetype = "dotted", size = 1) +
  labs(title = "Standardized deviance residuals vs linear predictor",
       color = "Y") +
  theme(text = element_text(size = 14))

ggplot(pm10.pred, aes(cars, devstd, color = as.factor(highpm10))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), linetype = "dotted", size = 1) +
  labs(title = "Standardized deviance residuals vs cars, by temp diff",
       color = "Y") +
  theme(text = element_text(size = 14)) +
  facet_wrap(~ tempdiff)

ggplot(pm10.pred, aes(windspeed, devstd, color = as.factor(highpm10))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), linetype = "dotted", size = 1) +
  labs(title = "Standardized deviance residuals vs wind speed, by temp diff",
       color = "Y") +
  theme(text = element_text(size = 14)) +
  facet_wrap(~ tempdiff)

ggplot(pm10.pred, aes(sample = devstd)) +
  geom_qq() + geom_qq_line()

# Cook's distance####
pm10.pred$Dcook <- cooks.distance(model.oslo)
head(pm10.pred)

ggplot(pm10.pred, aes(xb, Dcook, color = as.factor(highpm10))) +
  geom_point() +
  geom_point(data = pm10.pred[I.highv, ], color = "black",
             shape = 24, size = 3) +
  #  geom_hline(yintercept = 1, color = "red", linetype = "dashed",
  #             size = 1) +
  geom_hline(yintercept = 4/nrow(pm10), linetype = "dotted",
             size = 1) +
  labs(title = "Cook's distance vs linear predictor, by temp diff",
       color = "Y", 
       caption = "4/n in black, high leverage highlighted") +
  theme(text = element_text(size = 14)) +
  facet_grid(rows = vars(highpm10), cols = vars(tempdiff))

#Other models####
# Since tempdiff = pos and neg are small groups, both with
# beta-parameters larger than zero, we might want to put them
# together into one group. This is done in the variable zerodiff:
table(pm10$tempdiff, pm10$zerodiff)
# We will try using this version in some models.
# It might reduce the influence problem.
#
# p = 0:
# Null = model.0
# p = 1:
# cars = model.1
# p = 2: 
## 2.cars + wind####
model.2 <- glm(highpm10 ~ I(cars/1000) + windspeed, family = "binomial", data = pm10)
# red: tempdiff = model.red
## 3.cars + zerodiff####
model.3 <- glm(highpm10 ~ I(cars/100) + zerodiff, family = "binomial", data = pm10)
# p = 3:
## 4.cars*wind####
model.4 <- glm(highpm10 ~ I(cars/1000)*windspeed, family = "binomial", data = pm10)
## 5.backward elimination with BIC gives cars+tempdiff####
model.5 <- step(model.oslo, k = log(nrow(pm10)))
# p = 4:
## 6.cars*wind+zerodiff####
model.6 <- glm(highpm10 ~ I(cars/100)*windspeed + zerodiff, family = "binomial", data = pm10)
# oslo.cars*wind+tempdiff = model.oslo

# AIC and BIC####
aic <- AIC(model.0, model.1, model.2, model.red, model.3, 
           model.4, model.5, model.6, model.oslo)
bic <- BIC(model.0, model.1, model.2, model.red, model.3, 
           model.4, model.5, model.6, model.oslo)
(collect.AIC <- data.frame(aic, bic))

# model 3: with cars and zerodiff is the best (BIC)

# Pseudo R2####
# Null model: ln L(b0)
logLik(model.0)
(lnL0 <- logLik(model.0)[1])

(R2CS.max <- 1 - (exp(lnL0))^(2/nrow(pm10)))
# Collect the log likelihoods L(betahat)
collect.AIC$loglik <- 
  c(logLik(model.0)[1],
    logLik(model.1)[1],
    logLik(model.2)[1],
    logLik(model.red)[1],
    logLik(model.3)[1],
    logLik(model.4)[1],
    logLik(model.5)[1],
    logLik(model.6)[1],
    logLik(model.oslo)[1])
##R2_McF####
collect.AIC$R2McF <- 1 - collect.AIC$loglik/lnL0
##R2_McF,adj.####
# Note that p+1 = df (and df.1):
collect.AIC$R2McF.adj <- 1 - (collect.AIC$loglik - (collect.AIC$df - 1)/2)/lnL0
##R2_Cox-Snell####
collect.AIC$R2CS <- 1 - (exp(lnL0 - collect.AIC$loglik))^(2/nrow(pm10))
##R2_Nagelkerke####
collect.AIC$R2N <- collect.AIC$R2CS/R2CS.max

# Show them as % with one decimal value:
round(100*collect.AIC[, c("R2McF", "R2McF.adj", "R2CS", "R2N")], digits = 1)
