# Lecture 2. Example 1. 22/3-23
# Simple linear regression: Ice cream####
# Part 2. Intervals and residuals

# Load ggplot2 to access the ggplot-command
library(ggplot2)

#Quick version of Lecture 1####
load("lectures/data/icecream.RData")
ice.model <- lm(loss ~ weeks, data = ice.data)
ice.summary <- summary(ice.model)

# Calculate the confidence intervals for beta:
confint(ice.model)

# Confidence and prediction interval for x0 = 34####

(ice.x0 <- data.frame(weeks = c(34)))
(ice.y0.pred <- cbind(ice.x0,
                      fit = predict(ice.model, ice.x0),
                      conf = predict(ice.model, ice.x0, interval = "confidence"),
                      pred = predict(ice.model, ice.x0, interval = "prediction")))

# We now have three versions of the fitted line! 
# Get rid of the extra ones by setting them to NULL:
ice.y0.pred$conf.fit <- ice.y0.pred$pred.fit <- NULL
ice.y0.pred

#Intervals for the all x####

##Save predictions and intervals####
# Make a new data frame by adding the confidence and
# prediction intervals for each of the observations. It is
# a good idea not to mess up your original data! 
#
# You will get a warning that you should contemplate.

ice.pred <- 
  cbind(ice.data, 
        fit = predict(ice.model),
        conf = predict(ice.model, interval = "confidence"),
        pred = predict(ice.model, interval = "prediction"))
head(ice.pred)
# get rid of the extra fits
ice.pred$conf.fit <- ice.pred$pred.fit <- NULL
head(ice.pred)

## Plot the data####

# Make the basic plot, add some modifications and
# save it so that we can add other things to it later:
(
  plot.data <- 
    ggplot(data = ice.pred, aes(x = weeks, y = loss)) + 
    geom_point(size = 3) +
    xlab("Time in storage (weeks)") +
    ylab("Weight loss (g)") +
    labs(title = "Ice cream: weight loss by time in storage") +
    theme(text = element_text(size = 18))
  )

## Add the fitted line####
(
  plot.line <- plot.data + 
    geom_line(aes(y = fit), color = "blue", linewidth = 1) +
    labs(caption = "data and fitted line")
  )

## Add confidence interval####
(
  plot.conf <- plot.line + 
    geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2) +
    labs(caption = "data, fitted line and 95% confidence interval")
)

## Add prediction interval####
plot.conf +
  geom_line(aes(y = pred.lwr),
            color = "red", linetype = "dashed", linewidth = 1) +
  geom_line(aes(y = pred.upr),
            color = "red", linetype = "dashed", linewidth = 1) +
  labs(caption = "data, fitted line, 95% confidence and prediction intervals")

# Basic residual analysis####

## Add the residuals to the predicted data####
ice.pred$e <- ice.model$residuals
head(ice.pred)

# Save the max-value in order to make the y-axis symmetrical 
# in the plots.
(max.e <- max(abs(ice.pred$e)))
(ice.elims <- c(-max.e, max.e))

## Plot against x####
# Add a horizontal line at y=0,
# and expand the y-axis to include +/- max residual.

ggplot(data = ice.pred, 
       aes(x = weeks, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = ice.elims) +
  xlab("Storage time (weeks)") +
  ylab("Residual") +
  labs(tag = "A") +
  labs(title = "Residuals vs x-values") +
  theme(text = element_text(size = 18))

## Plot against yhat####
# Add a horizontal line at y=0,
# and expand the y-axis to include +/- max residual.

ggplot(data = ice.pred, aes(x = fit, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = ice.elims) +
  xlab("Predicted weight loss (g)") +
  ylab("Residual") +
  labs(tag = "B") +
  labs(title = "Residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 18))

##Normal qq-plot####
ggplot(data = ice.pred, aes(sample = e)) +
  geom_qq(size = 3) +
  geom_qq_line() +
  labs(tag = "C") +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 18))

## Histogram####
ggplot(data = ice.pred, aes(x = e)) +
  geom_histogram(bins = 10) +
  xlab("Residuals") +
  labs(title = "Histogram of residuals") +
  theme(text = element_text(size = 18))
