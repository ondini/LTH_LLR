library(ggplot2)

# Part 2 of project 2 ####

## Load the data ####
weather <- read.csv("project2/data/weather.csv")
summary(weather)

## Add variables to the df #### 
(Q1 <- quantile(weather$rain, 0.25))
weather$lowrain <- as.numeric(weather$rain < Q1)
weather$low_cat <- factor(weather$lowrain,
                          levels = c(0, 1),
                          labels = c("high", "low"))
weather$monthnr <- as.factor(substr(weather$month, 6, 7))

## Fit full model ####
fullmod <- glm(lowrain ~ pressure*location*speed*temp*monthnr, family = "binomial", data = weather)
(sum_full <- summary(fullmod))

nullmod <- glm(lowrain ~1, family = "binomial", data = weather)

## Choose best models using BIC and Stepwise selection ####
fwmodel <- step(nullmod, scope = list(lower=nullmod,upper=fullmod),
                 direction="both", criterion = "BIC",  k = log(nrow(weather)))
(fwmodel.sum <- summary(fwmodel))

bwmodel <- step(fullmod, scope = list(lower=nullmod,upper=fullmod),
                direction="both", criterion = "BIC",  k = log(nrow(weather)))
(bwmodel.sum <- summary(bwmodel))

model.sum <- summary(fwmodel)

# #AIC and BIC####
aic <- AIC(nullmod, fwmodel, bwmodel)
bic <- BIC(nullmod, fwmodel, bwmodel)
(collect.AIC <- data.frame(aic, bic))

# model 3: with cars and zerodiff is the best (BIC)

# Pseudo R2####
# Null model: ln L(b0)
logLik(nullmod)
(lnL0 <- logLik(nullmod)[1])

(R2CS.max <- 1 - (exp(lnL0))^(2/nrow(weather)))
# Collect the log likelihoods L(betahat)
collect.AIC$loglik <- 
  c(logLik(nullmod)[1],
    logLik(fwmodel)[1],
    logLik(bwmodel)[1])
##R2_McF####
collect.AIC$R2McF <- 1 - collect.AIC$loglik/lnL0
##R2_McF,adj.####
# Note that p+1 = df (and df.1):
( collect.AIC$R2McF.adj <- 1 - (collect.AIC$loglik - (collect.AIC$df - 1)/2)/lnL0 )



## Cook's distance####

weather.pred <- cbind(
  weather,
  phat = predict(bwmodel),
  xb = predict(bwmodel),
  v = influence(bwmodel)$hat
)

weather.pred$Dcook <- cooks.distance(bwmodel)
head(weather.pred)

(
  plt.cook <- ggplot(weather.pred, aes(xb, Dcook, color = low_cat)) +
    geom_point() +
    geom_hline(yintercept = 4/nrow(weather), linetype = "dotted",
               size = 1) +
    labs(title = "Cook's distance vs linear predictor, by lowrain",
         color = "Y", 
         caption = "4/n in black, high leverage highlighted") +
    xlab("Linear predictor") +
    ylab("Cook's distance") +
    theme(text = element_text(size = 14)) 
)

ggsave(filename = "project2/plots/mod2abw_cook.png", plot = plt.cook)



## Deviance residuals, standardised####
weather.pred$devres <- influence(bwmodel)$dev.res
weather.pred$devstd <- weather.pred$devres/sqrt(1 - weather.pred$v)
head(weather.pred)

ggplot(weather.pred, aes(xb, devstd, color = low_cat)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), linetype = "dotted", size = 1) +
  labs(title = "Standardized deviance residuals vs linear predictor",
       color = "Y") +
  theme(text = element_text(size = 14))

## Get highest cook distance for lowrain and highrain ####
weather.lowrains <- weather.pred[weather.pred$low_cat == 'low',]

low_max.D <- which.max(weather.lowrains$Dcook)
weather.lowrains[low_max.D, ]

weather.highrains <- weather.pred[weather.pred$low_cat == 'high',]

high_max.D <- which.max(weather.highrains$Dcook)
weather.highrains[high_max.D, ]


(
  plt.cookM <- ggplot(weather.pred, aes(xb, Dcook, color = low_cat)) +
    geom_point() +
    geom_hline(yintercept = 4/nrow(weather), linetype = "dotted",
               size = 1) +
    geom_point(data = weather.lowrains[low_max.D, ], 
               color = "black", size = 4, shape = 24) +
    geom_point(data = weather.highrains[high_max.D, ], 
               color = "black", size = 4, shape = 23) +
    labs(title = "Cook's distance vs linear predictor, by lowrain",
         color = "Y", 
         caption = "4/n in black, high leverage highlighted") +
    xlab("Linear predictor") +
    ylab("Cook's distance") +
    theme(text = element_text(size = 14)) 
)

ggsave(filename = "project2/plots/mod2abw_cookM.png", plot = plt.cookM)

## Mark the highest CookD in the deviance plot ####

(
  plt.devstd <-
ggplot(weather.pred, aes(xb, devstd, color = low_cat)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_point(data = weather.lowrains[low_max.D, ], 
             color = "black", size = 4, shape = 24) +
  geom_point(data = weather.highrains[high_max.D, ], 
             color = "black", size = 4, shape = 23) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), linetype = "dotted", size = 1) +
  labs(title = "Standardized deviance residuals vs linear predictor",
       color = "Y") +
  xlab("Linear predictor") +
  ylab("Standardised deviance") +
  theme(text = element_text(size = 14))

)
ggsave(filename = "project2/plots/mod2abw_devstd.png", plot = plt.devstd)




## Estimate prob and logit ####
(Y <- sum(weather$drymonth == 1))
(n <- nrow(weather))

(p <- Y/n)
(odds <- p/(1-p))
(lo <- log(odds))


## Fit null model ####
(mod_0 <- glm(drymonth ~ 1, family = "binomial", data = weather))


## Estimate CIs ####
(lambda <- qnorm(1 - 0.05/2))

(dp <- sqrt(p*(1-p)/n))
(p.lwr <- p - lambda*dp )
(p.upr <- p + lambda*dp )

(dlo <- sqrt(1/Y + 1/(n-Y)))
(lo.lwr <- lo - lambda*dlo )
(lo.upr <- lo + lambda*dlo )

(ci.lo <- confint(mod_0))
(ci.o <- exp(ci.lo))
(ci.p <- ci.o / (1+ci.o))

## Lowrain as first quantile ####
(Q1 <- quantile(weather$rain, 0.25))
weather$lowrain <- as.numeric(weather$rain < Q1)
weather$low_cat <- factor(weather$lowrain,
                          levels = c(0, 1),
                          labels = c("high", "low"))

## Relationship between location and lowraint ####

(tab <- table(weather$location, weather$low_cat))

# compute stuff here

## Fit model with location and get CI ####

table(weather$location)
# weather$location <- relevel(weather$location, "Katterjåkk") # not needed, since Katterjåkk is already a refernce variable
(mod_1 <- glm(lowrain ~ location, family = "binomial", data = weather))

(ci.beta <- confint(mod_1))
exp(mod_1$coefficients)
(ci.or <- exp(ci.beta))
summary(mod_1)$coefficients[, "Std. Error"]


## Plot lowerain against pressure ####
(plt.lowraingeom <-
  ggplot(weather, aes(pressure, lowrain)) +
    geom_point() +
    geom_smooth() +
    xlab("Pressure (hPa)") +
    ylab("Low rain") +
    labs(title = "Low rain (=1) or Not low rain (=0) vs Pressure") +
    theme(text = element_text(size = 14))
)


ggsave(filename = "project2/plots/lowraingeom.png", plot = plt.lowraingeom)

## Fit model with airpressure ####

(mod_2 <- glm(lowrain ~ pressure, family = "binomial", data = weather))

(ci.beta <- confint(mod_2))
exp(mod_2$coefficients)
(ci.or <- exp(ci.beta))

(sum2 <- summary(mod_2))

## compare the fitted model with Null model using the summary output ####
(D_diff <- sum2$null.deviance - sum2$deviance)
(df_diff <- sum2$df.null - sum2$df.residual)

###chi2-quantile to compare D_diff with####
qchisq(1 - 0.05, df_diff)
### or P-value####
pchisq(D_diff, df_diff, lower.tail = FALSE)


## Compute the relative change ####


## Create the plot with CI ####

weather.pred <- cbind(
  weather,
  phat = predict(mod_2, type = "response"))

ggplot(weather.pred, aes(pressure, lowrain)) +
  geom_point() +
  geom_smooth(se = FALSE, linetype = "dashed") +
  geom_line(aes(y = phat), color = "red", size = 1) +
  xlab("Pressure (hPa)") +
  ylab("Low rain") +
  labs(title = "Low rain (=1) or Not low rain (=0) vs Pressure",
       caption = "red = fitted line, blue dashed = moving average") +
  theme(text = element_text(size = 14))

## logit####
# = logodds with s.e. for constructing C.I.
weather.pred <- cbind(
  weather.pred,
  logit = predict(mod_2, se.fit = TRUE))
head(weather.pred)
# An unnecessary variable:
weather.pred$logit.residual.scale <- NULL

## CI for logit (Wald)####
# Wald ok here since we do not want to test 
# and there is no feasible alternative method!
# Standard normal quantile:
(lambda <- qnorm(1 - 0.05/2))
weather.pred$logit.lwr <- weather.pred$logit.fit - lambda*weather.pred$logit.se.fit
weather.pred$logit.upr <- weather.pred$logit.fit + lambda*weather.pred$logit.se.fit
head(weather.pred)

## CI for odds####
weather.pred$odds.lwr <- exp(weather.pred$logit.lwr)
weather.pred$odds.upr <- exp(weather.pred$logit.upr)
head(weather.pred)

## CI for p####
weather.pred$p.lwr <- weather.pred$odds.lwr/(1 + weather.pred$odds.lwr)
weather.pred$p.upr <- weather.pred$odds.upr/(1 + weather.pred$odds.upr)
head(weather.pred)

## plot intervals####
( 
  mod1c.plotint <- 
    ggplot(weather.pred, aes(pressure, lowrain)) +
      geom_point() +
      geom_line(aes(y = phat), color = "red", size = 1) +
      geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.2) +
      xlab("Pressure (hPa)") +
      ylab("Low rain") +
      labs(title = "Low rain (=1) or Not low rain (=0) vs Pressure",
           caption = "red = fitted line, with 95% confidence interval") +
      theme(text = element_text(size = 14))
)

ggsave(filename = "project2/plots/mod1c_plotint.png", plot = mod1c.plotint)

## Calculate Leverage####
weather.pred <- cbind(weather,
                   xb = predict(mod_2),
                   v = influence(mod_2)$hat)
head(weather.pred)


### plot leverage against pressure ####
(plot.v <- ggplot(weather.pred, aes(pressure, v)) + 
   geom_point() +
   geom_hline(yintercept = 2*length(mod_2$coefficients)/nrow(weather), 
              color = "red", size = 1) +
   labs(title = "Leverage vs pressure, by Y=0 or Y=1",
        caption = "2(p+1)/n in red") +
   xlab("Pressure (hPa)") +
   ylab("Leverage") +
   ylim(-0.0001, 0.0062)+
   theme(text = element_text(size = 14))
 )

ggsave(filename = "project2/plots/mod1c_leverage.png", plot = plot.v)


# #AIC and BIC####
(mod_01 <- glm(lowrain ~ 1, family = "binomial", data = weather))

aic <- AIC(mod_01, mod_1, mod_2)
bic <- BIC(mod_01, mod_1, mod_2)
(collect.AIC <- data.frame(aic, bic))

# model 3: with cars and zerodiff is the best (BIC)

# Pseudo R2####
# Null model: ln L(b0)
logLik(mod_01)
(lnL0 <- logLik(mod_01)[1])

(R2CS.max <- 1 - (exp(lnL0))^(2/nrow(weather)))
# Collect the log likelihoods L(betahat)
collect.AIC$loglik <- 
  c(logLik(mod_01)[1],
    logLik(mod_1)[1],
    logLik(mod_2)[1])
##R2_McF####
collect.AIC$R2McF <- 1 - collect.AIC$loglik/lnL0
##R2_McF,adj.####
# Note that p+1 = df (and df.1):
( collect.AIC$R2McF.adj <- 1 - (collect.AIC$loglik - (collect.AIC$df - 1)/2)/lnL0 )

