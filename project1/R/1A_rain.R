# Project1 1A Y = Rain

library(ggplot2)
library(gridExtra)


# Read in the data
weather <- read.csv("Data/weather.csv")

# Fit linear regression model with rain 
model_rain <- lm(rain ~ pressure, data = weather)

# Fit linear regression model with log(rain) 
model_log_rain <- lm(log(rain) ~ pressure, data = weather)

# Fit linear regression model with rain^(1/3) 
model_cubert_rain <- lm(I(rain^(1/3)) ~ pressure, data = weather)

model = model_rain

weather$yhat <- predict(model)


## Get and save standard errors, etc####
(weather.linsum <- summary(model))

# Extract and save the sigma estimate from the summary
# so you can use it later.
(weather.linsigma <- weather.linsum$sigma)

# Extract the beta estimates table from the summary
weather.linsum$coefficients

# Calculate the confidence intervals for beta:
confint(model)

## Save all beta-related things together####
# Since they are matrices we also need to force the result
# to become a data frame so we can refer to the columns
# by name.
(
  weather.linbeta <- 
    cbind.data.frame(weather.linsum$coefficients, 
                     confint(model))
)


## Get confidence and prediction interval for x0 = 34####

# New data frame with the new x0-value(s) which must have
# the same name as in the data frame used to fit the model!

pressure_seq <- seq(min(weather$pressure), max(weather$pressure), length = 100)

(weather.x0 <- data.frame(pressure = pressure_seq))
(weather.liny0pred <- 
    cbind(weather.x0, 
          predict(model, weather.x0, se.fit = TRUE),
          conf = predict(model, weather.x0, 
                         interval = "confidence"),
          pred = predict(model, weather.x0,
                         interval = "prediction"))
)
# Get rid of the extra variables by setting them to NULL:
weather.liny0pred$df <- weather.liny0pred$residual.scale <- 
  weather.liny0pred$conf.fit <- weather.liny0pred$pred.fit <- NULL
weather.liny0pred

# Calculate the standard error of the prediction 
# using the saved sigma-estimate, and add it;
weather.liny0pred$se.pred <- 
  sqrt(weather.linsigma^2 + weather.liny0pred$se.fit^2)
weather.liny0pred

# Plot the data####


##Calculate yhat, confint, predint####
# Create a data frame with the data, the fitted line and
# confidence and prediction intervals:

weather.linpred <- 
  cbind(weather, 
        fit = predict(model),
        conf = predict(model, interval = "confidence"),
        pred = predict(model, interval = "prediction"))
head(weather.linpred)
# get rid of the extra fits
weather.linpred$conf.fit <- weather.linpred$pred.fit <- NULL

##Plot everything####
(plot.linpred <- 
    ggplot(data = weather.linpred, 
           aes(x = pressure, y = rain)) + 
    geom_point() +
    geom_line(aes(y = fit), color = "blue", size = 1) +
    geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.3) +
    geom_line(data = weather.linpred, aes(y = pred.lwr),
              color = "red", linetype = "dashed", size = 0.1) +
    geom_line(data = weather.linpred, aes(y = pred.upr),
              color = "red", linetype = "dashed", size = 0.1) +
    xlab("Pressure (hPa)") +
    ylab("Rain (mm)") +
    labs(title = "Precipitations: Rain vs Pressure") +
    labs(caption = 
           "fitted linear model and 95% conf intervals") +
    theme(text = element_text(size = 15)))


# Basic residual analysis####

## Add the residuals to the predicted data####
weather.linpred$e.lin <- model$residuals
head(weather.linpred)

# Save the max-value in order to make the y-axins symmetrical 
# in the plots.
(max.elin <- max(abs(weather.linpred$e.lin)))
(weather.lim.elin <- c(-max.elin, max.elin))

## Plot against x####
# Add a horizontal line at y=0,
# add a moving average, geom_smooth(), to see trends,
# and expand the y-axis to include +/- max residual.

ggplot(data = weather.linpred, aes(x = pressure, y = e.lin)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  expand_limits(y = weather.lim.elin) +
  xlab("Pressure (cm)") +
  ylab("Residual") +
  labs(tag = "A") +
  labs(title = "Residuals vs x-values") +
  theme(text = element_text(size = 15))

## Plot against yhat####
ggplot(data = weather.linpred, aes(x = fit, y = e.lin)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  expand_limits(y = weather.lim.elin) +
  xlab("Predicted precipitations (mm)") +
  ylab("Residual")+
  labs(tag = "B") +
  labs(title = "Residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 15))

## Normal qq-plot####
ggplot(data = weather.linpred, aes(sample = e.lin)) +
  geom_qq() +
  geom_qq_line()+
  labs(tag = "C") +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 15))










# Arrange the plots side-by-side
grid.arrange(resid_plot, qq_plot, ncol = 2)























rm(list = ls())

