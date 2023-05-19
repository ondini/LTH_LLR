
library(ggplot2)
source("lectures/R/boot_poisson.R")

# Project 3 ####

## Datawork ####
### Load the data ####
weather <- read.csv("project3/data/weather.csv")
summary(weather)

### Add variables to the df #### 
weather$rain_integer <- round(weather$rain)
weather$monthnr <- as.factor(substr(weather$month, 6, 7))

### Frequency table of the amount of rain ####
(rain.table <- table(weather$rain_integer))

## Fit the distributions ####
### Save table as data frame and add proportions####
(rain.df <- data.frame(
  rain = as.numeric(names(rain.table)),
  rain.table))
rain.df$prob <- rain.df$Freq / sum(rain.df$Freq)
rain.df$Var1 <- NULL
rain.df$model <- "Observed"
rain.df

###Average amount of rain####
(mu <- mean(rain.df$rain))
var(rain.df$rain)

###Expected frequencies for Poisson ####
rain.dfp <- rain.df
rain.dfp$prob <- dpois(rain.dfp$rain, mu-40 )
rain.dfp$model <- "Poisson"
rain.dfp

rain.dfpf <- rbind(rain.df, rain.dfp)

###Plot observed and expected for Poisson ####
ggplot(rain.dfpf, aes(rain, prob, fill = model)) +
  geom_col(position = "dodge") +
  labs(title = "Distribution of rain amount vs poisson",
       x = "rain amount",
       y = "relative frequency") +
  theme(text = element_text(size = 18))


###Expected frequencies for NegBin ####
rain.dfnb <- rain.df
rain.dfnb$prob <- dnbinom(rain.dfnb$rain, mu = mu-40, size = 4.923)
rain.dfnb$model <- "Negative binomial"
rain.dfnb

rain.dfnbf <- rbind(rain.df, rain.dfnb)

### Plot observed and expected for Negbin ####
ggplot(rain.dfnbf, aes(rain, prob, fill = model)) +
  geom_col(position = "dodge") +
  labs(title = "Distribution of rain amount vs negbin",
       x = "rain amount",
       y = "relative frequency") +
  theme(text = element_text(size = 18))


## Models for Poisson ####

### Stepwise selection ####
fullmodp <- glm(rain_integer ~ pressure*location*speed*temp*monthnr, family = "poisson", data = weather)
(sum_fullp <- summary(fullmodp))

nullmodp <- glm(rain_integer ~1, family = "poisson", data = weather)

fwmodelp <- step(nullmodp, scope = list(lower=nullmodp,upper=fullmodp),
                direction="both", criterion = "BIC",  k = log(nrow(weather)))
(fwmodelp.sum <- summary(fwmodelp))
BIC(fwmodelp)
(1 - (fwmodelp.sum$deviance + 288)/fwmodelp.sum$null.deviance)


bwmodelp <- step(fullmodp, scope = list(lower=nullmodp,upper=fullmodp),
                direction="both", criterion = "BIC",  k = log(nrow(weather)))
(bwmodelp.sum <- summary(bwmodelp))
BIC(bwmodelp)
(1 - (bwmodelp.sum$deviance + 288)/bwmodelp.sum$null.deviance)



### Poisson regression simple models ####
(simpmodel <- glm(rain_integer ~ location + speed, family = "poisson", data = weather))
(simpmodel.sum <- summary(simpmodel))
BIC(simpmodel)
(1 - (simpmodel.sum$deviance + 5)/simpmodel.sum$null.deviance)

(simpmodel2 <- glm(rain_integer ~ location + pressure, family = "poisson", data = weather))
(simpmodel2.sum <- summary(simpmodel2))
BIC(simpmodel2)
(1 - (simpmodel2.sum$deviance + 5)/simpmodel2.sum$null.deviance)

(compmodel <- glm(rain_integer ~ location + pressure*speed + monthnr, family = "poisson", data = weather))
(compmodel.sum <- summary(compmodel))
BIC(compmodel)
(1 - (compmodel.sum$deviance + 17)/compmodel.sum$null.deviance)


## Show fit quality for selected Poisson model ####

model.weather <- bwmodelp

# cbind(summary(model.weather)$coefficients, ci = confint(model.weather))
# cbind(exp(model.weather$coefficients), exp(confint(model.weather)))


## Estimated mu with confidence interval using xbeta####
pred.weather <- cbind(
  weather,
  muhat = predict(model.weather, type = "response"),
  xb = predict(model.weather, se.fit = TRUE))
pred.weather$xb.residual.scale <- NULL

# x0p <- seq(33, 75)
# award.predint <- data.frame(
#   speed = rep(x0p, 3),
#   location = c(rep("Lund", length(x0)),
#                rep("Uppsala", length(x0)),
#                rep("Katterjåkk", length(x0))))
# 
# # Use the boot.pois function on the example####
# boot.predint <- boot.pois(model.weather, award.predint, 5000, 0.95)
# 
# ##Extract the prediction limits####
# award.predint$pred.lwr <- boot.predint$lower
# award.predint$pred.upr <- boot.predint$upper

pred.weather$xb.lwr <- pred.weather$xb.fit - 1.96*pred.weather$xb.se.fit
pred.weather$xb.upr <- pred.weather$xb.fit + 1.96*pred.weather$xb.se.fit
pred.weather$mu.lwr <- exp(pred.weather$xb.lwr)
pred.weather$mu.upr <- exp(pred.weather$xb.upr)
head(pred.weather)

##Plot data, mu and ci by location ####
ggplot(pred.weather, aes(speed, rain_integer, color = location)) +
  geom_jitter(height = 0, width = 0) +
  geom_line(aes(y = muhat), linewidth = 1) +
  geom_ribbon(aes(ymin = mu.lwr, ymax = mu.upr), alpha = 0.1) +
  labs(title = "Expected rain amount",
       caption = "95% confidence interval",
       color = "location") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ location, scales="free_x")

#Deviance tests####
##Against null model####
summary(model.weather)
(D_diff <- model.weather$null.deviance - model.weather$deviance)
(df_diff <- model.weather$df.null - model.weather$df.residual)
#quantile:
qchisq(1 - 0.05, df_diff)
#p-value:
pchisq(D_diff, df_diff, lower.tail = FALSE)

##Against removing prog####
#Update the model by removing prog:
model.noloc <- update(model.weather, . ~ . - location)
summary(model.noloc)
model.noloc$deviance - model.weather$deviance
qchisq(1 - 0.05, model.noloc$df.residual - model.weather$df.residual)

anova(model.noloc, model.weather)
qchisq(1 - 0.05, 2)
pchisq(model.noloc$deviance - model.weather$deviance,
       model.noloc$df.residual - model.weather$df.residual,
       lower.tail = FALSE)

#Influence measures####
infl.weather <- influence(model.weather)
pred.weather$v <- infl.weather$hat
pred.weather$devres <- infl.weather$dev.res
pred.weather$pears <- infl.weather$pear.res
pred.weather$std.devres <- pred.weather$devres/sqrt(1 - pred.weather$v)
pred.weather$std.pearres <- pred.weather$devres/sqrt(1 - pred.weather$v)
pred.weather$D <- cooks.distance(model.weather)

##Leverage####
ggplot(pred.weather, aes(speed, v, color = location)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 2*(length(model.weather$coefficients))/nrow(weather), 
             color = "red") +
  labs(title = "Leverage",
       color = "location", caption = "horizontal line = 2(p+1)/n") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ location)

##Standardised deviance residuals####
ggplot(pred.weather, aes(speed, std.devres, color = location)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), linetype = "dotted", size = 1) +
  labs(title = "Standardized deviance residuals",
       color = "program") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ location)

##Cook's D####
ggplot(pred.weather, aes(speed, D, color = location)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 4/nrow(weather), color = "red") +
  labs(title = "Cook's distance",
       color = "program", caption = "horizontal line = 4/n") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ location)

###Residual plot for poiss####
ggplot(pred.weather, aes(x = xb.fit, color = location)) +
  geom_point(aes(y = std.devres), size = 2) +
  geom_hline(yintercept = c(-3, -2, 0, 2, 3), 
             linetype = 2, size = 1) +
  expand_limits(y = c(-4.5, 7.5)) +
  labs(y = "std dev.res", x = "xb", color = "location",
       title = "Rain: Poiss model") +
  theme(text = element_text(size = 18))




## Fit negBin models #### 

library(MASS)

### Stepwise selection ####
fullmodn <- glm.nb(rain_integer ~ pressure*location*speed*temp*monthnr, data = weather)
(sum_fulln <- summary(fullmodn))

nullmodn <- glm.nb(rain_integer ~1,  data = weather)

## Choose best models using BIC and Stepwise selection ####
fwmodeln <- step(nullmodn, scope = list(lower=nullmodn,upper=fullmodn),
                direction="both", criterion = "BIC",  k = log(nrow(weather)))
(fwmodeln.sum <- summary(fwmodeln))
BIC(fwmodeln)
(1 - (fwmodeln.sum$deviance + 21)/fwmodeln.sum$null.deviance)


bwmodeln <- step(fullmodn, scope = list(lower=nullmodn,upper=fullmodn),
                direction="both", criterion = "BIC",  k = log(nrow(weather)))
(bwmodeln.sum <- summary(bwmodeln))
BIC(bwmodeln)
(1 - (bwmodeln.sum$deviance + 27)/bwmodeln.sum$null.deviance)


# Fit a negbin model####
(simplemodn1 <- glm.nb(rain_integer ~ speed + location, data = weather))
(simplemodn1.sum <- summary(simplemodn1))
BIC(simplemodn1)
(1 - (simplemodn1.sum$deviance + 5)/simplemodn1.sum$null.deviance)

(simpmodeln2 <-  glm.nb(rain_integer ~ pressure + location, data = weather))
(simpmodeln2.sum <- summary(simpmodeln2))
BIC(simpmodeln2)
(1 - (simpmodeln2.sum$deviance + 5)/simpmodeln2.sum$null.deviance)

(compmodeln <- glm.nb(rain_integer ~ location + pressure*speed + monthnr, data = weather))
(compmodeln.sum <- summary(compmodeln))
BIC(compmodeln)
(1 - (compmodeln.sum$deviance + 17)/compmodeln.sum$null.deviance)


## Show fit quality for selected Negbin model ####

model.nb <- bwmodeln
cbind(model.nb$coefficients, confint(model.nb))
exp(cbind(model.nb$coefficients, confint(model.nb)))

###Estimate means with confidence intervals####
pred.nb <- cbind(
  weather,
  mu = predict(model.nb, type = "response"),
  xb = predict(model.nb, se.fit = TRUE))
pred.nb$xb.residual.scale <- NULL

pred.nb$xb.lwr <- pred.nb$xb.fit - 1.96 * pred.nb$xb.se.fit
pred.nb$xb.upr <- pred.nb$xb.fit + 1.96 * pred.nb$xb.se.fit
pred.nb$mu.lwr <- exp(pred.nb$xb.lwr)
pred.nb$mu.upr <- exp(pred.nb$xb.upr)

###Add to plot####
ggplot(pred.nb, aes(pressure, rain_integer, color = location)) +
  geom_point() +
  geom_line(aes(y = mu), linewidth = 1) +
  geom_ribbon(aes(ymin = mu.lwr, ymax = mu.upr), alpha = 0.1) +
  facet_wrap(~ location)

### Bootstrap prediction intervals####

source("lectures/R/boot_negbin.R")
x0 <- seq(985, 1030, 5)
nb.predint <- data.frame(
  pressure = rep(x0, 3),
  location = c(rep("Lund", length(x0)),
           rep("Uppsala", length(x0)),
           rep("Katterjåkk", length(x0))))

table(weather$location)

# Use the boot.nb function on the example####
boot.predint.nb <- boot.nb(model.nb, weather, nb.predint, 5000, 0.95)

##Extract the prediction limits####
nb.predint$pred.lwr <- boot.predint.nb$lower
nb.predint$pred.upr <- boot.predint.nb$upper
head(nb.predint)

#Plotting predints with geom_smooth to avoid the raggedness.
ggplot(pred.nb, aes(pressure, rain_integer, color = location)) +
  geom_point() +
  geom_line(aes(y = mu)) +
  geom_ribbon(aes(ymin = mu.lwr, ymax = mu.upr), alpha = 0.1) +
  geom_smooth(data = nb.predint, aes(y = pred.lwr), size = 1, se = FALSE) +
  geom_smooth(data = nb.predint, aes(y = pred.upr), size = 1, se = FALSE) +
  labs(title = "Expected number of days absent",
       caption = "95% conf.int. and 95% bootstrap pred.int.") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ location)

##With standardized deviance residuals####
# for the two models.
pred.nb$devres <- influence(model.nb)$dev.res/sqrt(1 - influence(model.nb)$hat)
pred.nb$pearres <- influence(model.nb)$pear.res/sqrt(1 - influence(model.nb)$hat)

###Residual plot for negbin####
ggplot(pred.nb, aes(x = xb.fit, color = location)) +
  geom_point(aes(y = devres), size = 2) +
  geom_hline(yintercept = c(-3, -2, 0, 2, 3), 
             linetype = 2, size = 1) +
  expand_limits(y = c(-4.5, 7.5)) +
  labs(y = "std dev.res", x = "xb", color = "location",
       title = "Rain: Negbin model") +
  theme(text = element_text(size = 18))


##Against removing prog####
#Update the model by removing prog:
model.noloc <- update(model.nb, . ~ . - location)
summary(model.noloc)
model.noloc$deviance - model.nb$deviance
qchisq(1 - 0.05, model.noloc$df.residual - model.nb$df.residual)

anova(model.noloc, model.nb)
qchisq(1 - 0.05, 2)
pchisq(model.noloc$deviance - model.nb$deviance,
       model.noloc$df.residual - model.nb$df.residual,
       lower.tail = FALSE)

#Influence measures####
infl.weather <- influence(model.nb)
pred.weather$v <- infl.weather$hat
pred.weather$devres <- infl.weather$dev.res
pred.weather$std.devres <- pred.weather$devres/sqrt(1 - pred.weather$v)
pred.weather$D <- cooks.distance(model.nb)

##Leverage####
ggplot(pred.weather, aes(pressure, v, color = location)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 2*(length(model.nb$coefficients))/nrow(weather), 
             color = "red") +
  labs(title = "Leverage",
       color = "location", caption = "horizontal line = 2(p+1)/n") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ location)

##Standardised deviance residuals####
ggplot(pred.weather, aes(speed, std.devres, color = location)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), linetype = "dotted", size = 1) +
  labs(title = "Standardized deviance residuals",
       color = "program") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ location)

##Cook's D####
ggplot(pred.weather, aes(speed, D, color = location)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 4/nrow(weather), color = "red") +
  labs(title = "Cook's distance",
       color = "program", caption = "horizontal line = 4/n") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ location)


## Compare the models ####

### Likelihood test ####
-2*logLik(model.weather)
-2*logLik(model.nb)

(D_diff <- -2*logLik(model.weather)[1] + 2*logLik(model.nb)[1])
qchisq(1 - 0.05, 1)
pchisq(D_diff, 1, lower.tail = FALSE)

### Deviance residuals ####
data <- data.frame(
  Residuals = c(pred.nb$devres, pred.weather$devres),
  Model = c(rep("NegBin", length(pred.nb$devres)), rep("Poiss", length(pred.weather$devres)))
)

ggplot(data, aes(x = Residuals, fill = Model)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 20) +
  labs(title = "Standardized Devres Comparison", x = "Devres", y = "Frequency") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red"))

### Pearson residuals ####
data <- data.frame(
  Residuals = c(pred.nb$devres, pred.weather$devres),
  Model = c(rep("NegBin", length(pred.nb$devres)), rep("Poiss", length(pred.weather$devres)))
)

ggplot(data, aes(x = Residuals, fill = Model)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 20) +
  labs(title = "Standardized Pearson Comparison", x = "Pearres", y = "Frequency") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red"))

