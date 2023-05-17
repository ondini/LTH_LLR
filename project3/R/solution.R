
library(ggplot2)

# Part 3 of project 2 ####

## Load the data ####
weather <- read.csv("project2/data/weather.csv")
summary(weather)

## Add variables to the df #### 
weather$rain_integer <- round(weather$rain)
weather$monthnr <- as.factor(substr(weather$month, 6, 7))


## Frequency table of the number of awards####
(rain.table <- table(weather$rain_integer))
## Save table as data frame and add proportions####
(rain.df <- data.frame(
  rain = as.numeric(names(rain.table)),
  rain.table))
rain.df$prob <- rain.df$Freq / sum(rain.df$Freq)
rain.df$Var1 <- NULL
rain.df$model <- "Observed"
rain.df

##Average number of awards####
(mu <- mean(rain.df$rain))
## and a variance estimate####
# s2 = sum(x_i - mu)^2/(n-1)
var(rain.df$rain)

##Expected frequencies####
#according to a Poisson distribution with mean mu:
#New version of the table were the observed prob are
#replaced by the expected according to the Poisson.
#The model variable is used for colorcoding the plot:
rain.df2 <- rain.df
rain.df2$prob <- dpois(rain.df2$rain, mu)
rain.df2$model <- "Poisson"
rain.df2

#Add expected as rows:
rain.df <- rbind(rain.df, rain.df2)
rain.df

##Plot observed and expected####
ggplot(rain.df, aes(rain, prob, fill = model)) +
  geom_col(position = "dodge") +
  labs(title = "Distribution of awards",
       x = "number of awards",
       y = "relative frequency") +
  theme(text = element_text(size = 18))


#Table by location####
table(weather$location, weather$rain_integer)
aggregate(rain_integer ~ location, data = weather, FUN = mean)
aggregate(rain_integer ~ location, data = weather, FUN = var)

# large variance in Academic. Need maths?
# variance larger than the mean. Might be explained by covariates?

## Fit full model ####
fullmod <- glm(rain_integer ~ pressure*location*speed*temp*monthnr, family = "poisson", data = weather)
(sum_full <- summary(fullmod))

nullmod <- glm(rain_integer ~1, family = "poisson", data = weather)

## Choose best models using BIC and Stepwise selection ####
fwmodel <- step(nullmod, scope = list(lower=nullmod,upper=fullmod),
                direction="both", criterion = "BIC",  k = log(nrow(weather)))
(fwmodel.sum <- summary(fwmodel))

bwmodel <- step(fullmod, scope = list(lower=nullmod,upper=fullmod),
                direction="both", criterion = "BIC",  k = log(nrow(weather)))
(bwmodel.sum <- summary(bwmodel))


#Poisson regression model####
(model.locspeed <- glm(rain_integer ~ location + speed*pressure + temp, family = "poisson", data = weather))
summary(model.locspeed)
cbind(summary(model.locspeed)$coefficients, ci = confint(model.locspeed))
cbind(exp(model.locspeed$coefficients), exp(confint(model.locspeed)))

## Differences between programs####
beta <- model.locspeed$coefficients
exp(beta[1] + beta[2])
exp(beta[1] + beta[3])
# compared to the general program and for fixed final math exam, 
# students from the academic program get about 3 times more awards.

# For a fixed study program, a unit increase in the students maths grade
# predicts a 7% increase (from 1 to 1.07) in the number of awards. Or, should 
# the students population get on average an increase of 10 points in the
# maths grade (for fixed program), we would have an increase of awards equal 
# to exp(10*0.07)=2, that is double the awards

model.weather <- bwmodel

##Estimated mu with confidence interval using xbeta####
pred.weather <- cbind(
  weather,
  muhat = predict(model.weather, type = "response"),
  xb = predict(model.weather, se.fit = TRUE))
pred.weather$xb.residual.scale <- NULL

pred.weather$xb.lwr <- pred.weather$xb.fit - 1.96*pred.weather$xb.se.fit
pred.weather$xb.upr <- pred.weather$xb.fit + 1.96*pred.weather$xb.se.fit
pred.weather$mu.lwr <- exp(pred.weather$xb.lwr)
pred.weather$mu.upr <- exp(pred.weather$xb.upr)
head(pred.weather)

##Plot data, mu and ci by program####
ggplot(pred.weather, aes(temp, rain_integer, color = location)) +
  geom_jitter(height = 0.1, width = 0) +
  geom_line(aes(y = muhat), linewidth = 1) +
  geom_ribbon(aes(ymin = mu.lwr, ymax = mu.upr), alpha = 0.1) +
  labs(title = "Expected number of awards",
       caption = "95% confidence interval",
       color = "program") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ location)

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
model.math <- update(model.weather, . ~ . - location)
summary(model.math)
model.math$deviance - model.weather$deviance
qchisq(1 - 0.05, model.math$df.residual - model.weather$df.residual)

anova(model.math, model.weather)
qchisq(1 - 0.05, 2)
pchisq(model.math$deviance - model.weather$deviance,
       model.math$df.residual - model.weather$df.residual,
       lower.tail = FALSE)

#Influence measures####
infl.weather <- influence(model.weather)
pred.weather$v <- infl.weather$hat
pred.weather$devres <- infl.weather$dev.res
pred.weather$std.devres <- pred.weather$devres/sqrt(1 - pred.weather$v)
pred.weather$D <- cooks.distance(model.weather)

##Leverage####
ggplot(pred.weather, aes(speed, v, color = location)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 2*(length(model.weather$coefficients))/nrow(weather), 
             color = "red") +
  labs(title = "Leverage",
       color = "program", caption = "horizontal line = 2(p+1)/n") +
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


# PT2 - neg. likelihood #### 

library(MASS)









