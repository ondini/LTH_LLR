library(ggplot2)

# Part 1 of project 2 ####

## Load the data ####
weather <- read.csv("project2/data/weather.csv")
summary(weather)

## Add drymonth ####
weather$drymonth <- as.numeric(weather$rain < 0.5)

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

# Calculate the row and column totals
row_totals <- rowSums(tab)
col_totals <- colSums(tab)

# Calculate the probabilities and odds for each location
(prob_low <- tab[,2] / row_totals)
(prob_high <- tab[,1] / row_totals)
(odds <- prob_low / prob_high)

# Calculate the odds ratios for Lund and Uppsala, using Katterjåkk as reference
(odds_ratio_lund <- odds[2] / odds[1])
(odds_ratio_uppsala <- odds[3] / odds[1])

## Fit model with location and get CI ####

table(weather$location)
# weather$location <- relevel(weather$location, "Katterjåkk") # not needed, since Katterjåkk is already a refernce variable
(mod_1 <- glm(lowrain ~ location, family = "binomial", data = weather))

(ci.beta <- confint(mod_1))
exp(mod_1$coefficients)
(ci.or <- exp(ci.beta))
summary(mod_1)$coefficients[, "Std. Error"]

(mod_1.x0 <- data.frame(location = c("Katterjåkk", "Lund", "Uppsala")))
mod_1.y0.pred <- cbind(mod_1.x0,
                       logit = predict(mod_1, mod_1.x0, se.fit = TRUE))

head(mod_1.y0.pred)
# An unnecessary variable:
mod_1.y0.pred$logit.residual.scale <- NULL

## CI for logit (Wald)####
# Wald ok here since we do not want to test 
# and there is no feasible alternative method!
# Standard normal quantile:
(lambda <- qnorm(1 - 0.05/2))
mod_1.y0.pred$logit.lwr <- mod_1.y0.pred$logit.fit - lambda*mod_1.y0.pred$logit.se.fit
mod_1.y0.pred$logit.upr <- mod_1.y0.pred$logit.fit + lambda*mod_1.y0.pred$logit.se.fit

## CI for odds####
mod_1.y0.pred$odds <- exp(mod_1.y0.pred$logit.fit)
mod_1.y0.pred$odds.lwr <- exp(mod_1.y0.pred$logit.lwr)
mod_1.y0.pred$odds.upr <- exp(mod_1.y0.pred$logit.upr)

## CI for p####
mod_1.y0.pred$p <- mod_1.y0.pred$odds/(1 + mod_1.y0.pred$odds)
mod_1.y0.pred$p.lwr <- mod_1.y0.pred$odds.lwr/(1 + mod_1.y0.pred$odds.lwr)
mod_1.y0.pred$p.upr <- mod_1.y0.pred$odds.upr/(1 + mod_1.y0.pred$odds.upr)
head(mod_1.y0.pred)



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

