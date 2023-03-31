# Project1 1A Y = log(Rain)

library(ggplot2)
library(gridExtra)


# Read in the data
weather <- read.csv("Data/weather.csv")

# Fit linear regression model with log(rain) 
model_log_rain <- lm(log(rain) ~ log(pressure), data = weather)

# Fit linear regression model with rain^(1/3) 
model_cubert_rain <- lm(I(rain^(1/3)) ~ pressure, data = weather)

model = model_log_rain

weather$yhat <- predict(model)


## Get and save standard errors, etc####
(weather.logsum <- summary(model))

# Extract and save the sigma estimate from the summary
# so you can use it later.
(weather.logsigma <- weather.logsum$sigma)

# Extract the beta estimates table from the summary
weather.logsum$coefficients
(a <- exp(weather.logsum$coefficients[1]))

# Calculate the confidence intervals for beta0:
confint(model)
(Ia <- exp(confint(model)["(Intercept)", ]))

## Get confidence and prediction interval for x0 = 34####

pressure_seq <- seq(min(weather$pressure), max(weather$pressure), length = 100)

(weather.x0 <- data.frame(pressure = pressure_seq))
(weather.logy0pred <- 
    cbind(weather.x0, 
          predict(model, weather.x0, se.fit = TRUE),
          conf = predict(model, weather.x0,
                         interval = "confidence"),
          pred = predict(model, weather.x0,
                         interval = "prediction"))
)

# We now have three versions of the fitted line! 
# And an extra copy of sigma. And df.
# Get rid of the extra ones by setting them to NULL:
weather.logy0pred$df <- weather.logy0pred$residual.scale <- 
  weather.logy0pred$conf.fit <- weather.logy0pred$pred.fit <- NULL
weather.logy0pred

# Calculate the standard error of the prediction 
# using the saved sigma-estimate, and add it;
weather.logy0pred$se.pred <- 
  sqrt(weather.logsigma^2 + weather.logy0pred$se.fit^2)
weather.logy0pred

# This is the predictions of ln(rain)!
# Transform them to the original scale
weather.logy0pred$exp.fit <- exp(weather.logy0pred$fit)
weather.logy0pred$expconf.lwr <- exp(weather.logy0pred$conf.lwr)
weather.logy0pred$expconf.upr <- exp(weather.logy0pred$conf.upr)
weather.logy0pred$exppred.lwr <- exp(weather.logy0pred$pred.lwr)
weather.logy0pred$exppred.upr <- exp(weather.logy0pred$pred.upr)
weather.logy0pred

#Predict in full data set####


## Calculate fitted line and intervals####
weather.logpred <- 
  cbind(weather,
        fit = predict(model),
        conf = predict(model, interval = "confidence"),
        pred = predict(model, interval = "prediction"))
head(weather.logpred)
weather.logpred$conf.fit <- weather.logpred$pred.fit <- NULL

## Fine tune original plot####

(
  plot.data <- 
    ggplot(data = weather.logpred, aes(x = pressure, y = rain)) + 
    geom_point() +
    xlab("Pressure (hPa)") +
    ylab("Rain (mm)") +
    labs(title = "Precipitations: Rain vs Pressure") +
    labs(caption = "original scale") +
    expand_limits(x = c(min(weather$pressure), max(weather$pressure))) +
    theme(text = element_text(size = 15))
)

## Save log-plot as well####
(
  plot.logdata <- 
    ggplot(data = weather.logpred, 
           aes(x = log(pressure), y = log(rain))) + 
    geom_point() +
    xlab("ln(Pressure) (hPa)") +
    ylab("ln(Rain) (mm)") +
    labs(title = "Precipitations: ln(Rain) vs ln(Pressure)") +
    theme(text = element_text(size = 15)) +
    labs(caption = "logarithms before plotting") +
    expand_limits(x = c(log(min(weather$pressure)), log(max(weather$pressure))))
)

## Add to logplot####
plot.logdata + 
  geom_line(aes(y = fit), color = "blue", size = 1) +
  geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2) +
  geom_line(aes(y = pred.lwr),
            color = "red", linetype = "dashed", size = 1) +
  geom_line(aes(y = pred.upr),
            color = "red", linetype = "dashed", size = 1) +
  labs(caption = "logarithms before plotting")

## Add to original plot####
# Add the fitted line, confidence interval and prediction
# interval to the original data. Requires "anti-log"
# of all predictions.
plot.data +
  geom_line(aes(y = exp(fit)),
            color = "blue", size = 1) +
  geom_ribbon(aes(ymin = exp(conf.lwr), 
                  ymax = exp(conf.upr)),
              alpha = 0.2) +
  geom_line(aes(y = exp(pred.lwr)),
            color = "red", linetype = "dashed", size = 1) +
  geom_line(aes(y = exp(pred.upr)),
            color = "red", linetype = "dashed", size = 1) +
  labs(caption = "original scale")

# Basic residual analysis####

## Add residuals to predicted data####
weather.logpred$e.log <- model$residuals
head(weather.logpred)

# Save the max-value in order to make the y-axins symmetrical 
# in the plots.
(max.elog <- max(abs(weather.logpred$e.log)))
(weather.lim.elog <- c(-max.elog, max.elog))

## Plot against x####
# Add a horizontal line at y=0,
# and expand the y-axis to include +/- max residual.

ggplot(data = weather, 
       aes(x = pressure, y = weather.logpred$e.log)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  expand_limits(y = weather.lim.elog) +
  xlab("Pressure (hPa))") +
  ylab("Residual") +
  labs(tag = "A") +
  labs(title = "Residuals vs x-values") +
  theme(text = element_text(size = 15))

## Plot against yhat####
ggplot(data = weather, aes(x = weather.logpred$fit, y = weather.logpred$e.log)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  expand_limits(y = weather.lim.elog) +
  xlab("Predicted ln rain") +
  ylab("Residual") +
  labs(tag = "B") +
  labs(title = "Residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 15))

##Normal qq-plot####
ggplot(data = weather.logpred, aes(sample = e.log)) +
  geom_qq() + geom_qq_line() +
  labs(tag = "C") +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 15))





















rm(list = ls())

