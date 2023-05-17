# Lecture 11, Example 3: Quantile regression####
# 10/5-23: simulated data.

library(ggplot2)
# Necessary for quantile regresssion:
library(quantreg)

#Skewed example data####
load("Data/skewdata.rda")

##plot data####
ggplot(skewdata, aes(x, y)) + geom_point() +
  labs(title = "A skewed distribution") +
  theme(text = element_text(size = 14))

#Fit some models####
##null model####
# tau = 1 - alpha:
(model.null <- rq(y ~ 1, data = skewdata, tau = c(0.1, 0.5, 0.9)))
# one AIC for each tau:
(aic.null <- AIC(model.null))

##Final model####
(model.final <- rq(y ~ x, data = skewdata, tau = c(0.1, 0.5, 0.9)))

##Confidence intervals for beta####
(sum.final <- summary(model.final))
###ci for specific tau####
sum.final[[1]]
sum.final[[2]]
sum.final[[3]]

#Test parallel lines####
(anova.final <- anova(model.final))
anova.final$table
qf(0.05, 2, 601, lower.tail = FALSE)
pf(anova.final$table[1, "Tn"], 
   anova.final$table[1, "ndf"],
   anova.final$table[1, "ddf"], lower.tail = FALSE)

#AIC####
(aic.final <- AIC(model.final))

#Fitted lines####
# one line for each tau
yrq <- predict(model.final)
head(yrq)
skew.pred <- cbind(skewdata, quant = yrq)
head(skew.pred)

ggplot(skew.pred, aes(x, y)) + 
  geom_point() +
  geom_line(aes(y = quant.1, color = "tau = 10%"), size = 1) +
  geom_line(aes(y = quant.2, color = "tau = 50%"), size = 1) +
  geom_line(aes(y = quant.3, color = "tau = 90%"), size = 1) +
  labs(title = "A skewed distribution",
  caption = "tau = 1 - alpha",
  color = "Quantile") +
  theme(text = element_text(size = 18))

# LR-test####
## Comparing models for specific tau####
# one loglikelihood value for each tau:
logLik(model.null)
logLik(model.final)

# chi2-quantile to compare with
qchisq(0.05, 2 - 1, lower.tail = FALSE)

-2*(logLik(model.null)[1] - logLik(model.final)[1])
-2*(logLik(model.null)[2] - logLik(model.final)[2])
-2*(logLik(model.null)[3] - logLik(model.final)[3])

# plot beta-estimates####
# for the different alpha-values
# Red line is slope in a linear regression
plot(model.final)
## plot with c.i.####
plot(summary(model.final))

##For several quantiles####
#fit 10, 20, ..., 90% quantiles:
plot(summary(rq(y ~ x, data = skewdata, tau = seq(.1, .9, .1))))
