#Lecture 2. Example 3. 22/3-2023
# log-log regression: Atlantic cod####

library(ggplot2)

## Read in the data file####
load("Data/cod.RData")

## Fit the loglog model####
(cod.logmod <- lm(log(weight) ~ log(length), data = cod.data))

## Get and save standard errors, etc####
(cod.logsum <- summary(cod.logmod))

# Extract and save the sigma estimate from the summary
# so you can use it later.
(cod.logsigma <- cod.logsum$sigma)

# Extract the beta estimates table from the summary
cod.logsum$coefficients
(a <- exp(cod.logsum$coefficients[1]))

# Calculate the confidence intervals for beta0:
confint(cod.logmod)
(Ia <- exp(confint(cod.logmod)["(Intercept)", ]))

## Get confidence and prediction interval for x0 = 34####
(cod.x0 <- data.frame(length = c(34)))
(cod.logy0pred <- 
    cbind(cod.x0, 
          predict(cod.logmod, cod.x0, se.fit = TRUE),
          conf = predict(cod.logmod, cod.x0,
                         interval = "confidence"),
          pred = predict(cod.logmod, cod.x0,
                         interval = "prediction"))
)

# We now have three versions of the fitted line! 
# And an extra copy of sigma. And df.
# Get rid of the extra ones by setting them to NULL:
cod.logy0pred$df <- cod.logy0pred$residual.scale <- 
  cod.logy0pred$conf.fit <- cod.logy0pred$pred.fit <- NULL
cod.logy0pred

# Calculate the standard error of the prediction 
# using the saved sigma-estimate, and add it;
cod.logy0pred$se.pred <- 
  sqrt(cod.logsigma^2 + cod.logy0pred$se.fit^2)
cod.logy0pred

# This is the predictions of ln(weight)!
# Transform them to the original scale
cod.logy0pred$exp.fit <- exp(cod.logy0pred$fit)
cod.logy0pred$expconf.lwr <- exp(cod.logy0pred$conf.lwr)
cod.logy0pred$expconf.upr <- exp(cod.logy0pred$conf.upr)
cod.logy0pred$exppred.lwr <- exp(cod.logy0pred$pred.lwr)
cod.logy0pred$exppred.upr <- exp(cod.logy0pred$pred.upr)
cod.logy0pred

#Predict in full data set####

## Original scale####
ggplot(data = cod.data, aes(x = length, y = weight)) +
  geom_point() +
  expand_limits(x = c(20, 50))

## Plot the logarithms####
ggplot(data = cod.data, 
       aes(x = log(length), y = log(weight))) +
  geom_point() +
  expand_limits(x = c(log(20), log(50)))

# Alternative way that changes the scale instead of
# the values:
ggplot(data = cod.data, aes(x = length, y = weight)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  expand_limits(x = c(20, 50))

## Calculate fitted line and intervals####
cod.logpred <- 
  cbind(cod.data,
        fit = predict(cod.logmod),
        conf = predict(cod.logmod, interval = "confidence"),
        pred = predict(cod.logmod, interval = "prediction"))
head(cod.logpred)
cod.logpred$conf.fit <- cod.logpred$pred.fit <- NULL

## Fine tune original plot####

(
  plot.data <- 
    ggplot(data = cod.logpred, aes(x = length, y = weight)) + 
    geom_point() +
    xlab("Length (cm)") +
    ylab("Weight (g)") +
    labs(title = "Atlantic cod: weight by length") +
    labs(caption = "original scale") +
    expand_limits(x = c(20, 50)) +
    theme(text = element_text(size = 18))
)

## Save log-plot as well####
(
  plot.logdata <- 
    ggplot(data = cod.logpred, 
           aes(x = log(length), y = log(weight))) + 
    geom_point() +
    xlab("ln(Length(cm))") +
    ylab("ln(Weight(g))") +
    labs(title = "Atlantic cod: ln weight by ln length") +
    theme(text = element_text(size = 18)) +
    labs(caption = "logarithms before plotting") +
    expand_limits(x = c(log(20), log(50)))
)

## Add to logplot####
plot.logdata + 
  geom_line(aes(y = fit), color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2) +
  geom_line(aes(y = pred.lwr),
            color = "red", linetype = "dashed", linewidth = 1) +
  geom_line(aes(y = pred.upr),
            color = "red", linetype = "dashed", linewidth = 1) +
  labs(caption = "logarithms before plotting")
  
## Add to original plot####
# Add the fitted line, confidence interval and prediction
# interval to the original data. Requires "anti-log"
# of all predictions.
plot.data +
  geom_line(aes(y = exp(fit)),
            color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = exp(conf.lwr), 
                  ymax = exp(conf.upr)),
              alpha = 0.2) +
  geom_line(aes(y = exp(pred.lwr)),
            color = "red", linetype = "dashed", linewidth = 1) +
  geom_line(aes(y = exp(pred.upr)),
            color = "red", linetype = "dashed", linewidth = 1) +
  labs(caption = "original scale")

# Basic residual analysis####

## Add residuals to predicted data####
cod.logpred$e.log <- cod.logmod$residuals
head(cod.logpred)

# Save the max-value in order to make the y-axins symmetrical 
# in the plots.
(max.elog <- max(abs(cod.logpred$e.log)))
(cod.lim.elog <- c(-max.elog, max.elog))

## Plot against x####
# Add a horizontal line at y=0,
# and expand the y-axis to include +/- max residual.

ggplot(data = cod.logpred, 
       aes(x = log(length), y = e.log)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  expand_limits(y = cod.lim.elog) +
  xlab("ln(Length (cm))") +
  ylab("Residual") +
  labs(title = "Residuals vs x-values") +
  theme(text = element_text(size = 18))

## Plot against yhat####
ggplot(data = cod.logpred, aes(x = fit, y = e.log)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  expand_limits(y = cod.lim.elog) +
  xlab("Predicted ln weight") +
  ylab("Residual") +
  labs(title = "Residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 18))

##Normal qq-plot####
ggplot(data = cod.logpred, aes(sample = e.log)) +
  geom_qq() + geom_qq_line() +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 18))

##Histogram####
ggplot(data = cod.logpred, aes(x = e.log)) +
  geom_histogram(bins = 20) +
  xlab("Residuals") +
  labs(title = "Histogram of residuals") +
  theme(text = element_text(size = 18))
