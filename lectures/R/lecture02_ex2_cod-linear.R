#Lecture 2. Example 2. 22/3-2023
#Simple linear regression: Atlantic cod####
#
# This cod-code does the same thing as the icecream example
# but in a situation where the relationship is not linear.

# Activate the ggplot-commands:
library(ggplot2)

## Read in the data file####
load("Data/cod.RData")
summary(cod.data)
head(cod.data)

## Plot the data####
ggplot(cod.data, aes(x = length, y = weight)) +
  geom_point() +
  labs(x = "Length (cm)",
       y = "Weight (g)",
       caption = "Data: IVL Svenska Miljöinstitutet, ivl.se",
       title = "Atlantic cod: weight by length") +
  theme(text = element_text(size = 18))

## Fit the linear model####
(cod.linmod <- lm(weight ~ length, data = cod.data))

## Get and save standard errors, etc####
(cod.linsum <- summary(cod.linmod))

# Extract and save the sigma estimate from the summary
# so you can use it later.
(cod.linsigma <- cod.linsum$sigma)

# Extract the beta estimates table from the summary
cod.linsum$coefficients

# Calculate the confidence intervals for beta:
confint(cod.linmod)

## Save all beta-related things together####
# Since they are matrices we also need to force the result
# to become a data frame so we can refer to the columns
# by name.
(
  cod.linbeta <- 
    cbind.data.frame(cod.linsum$coefficients, 
                     confint(cod.linmod))
)

## Get confidence and prediction interval for x0 = 34####

# New data frame with the new x0-value(s) which must have
# the same name as in the data frame used to fit the model!
(cod.x0 <- data.frame(length = c(34)))
(cod.liny0pred <- 
    cbind(cod.x0, 
          predict(cod.linmod, cod.x0, se.fit = TRUE),
          conf = predict(cod.linmod, cod.x0, 
                         interval = "confidence"),
          pred = predict(cod.linmod, cod.x0,
                         interval = "prediction"))
)
# Get rid of the extra variables by setting them to NULL:
cod.liny0pred$df <- cod.liny0pred$residual.scale <- 
  cod.liny0pred$conf.fit <- cod.liny0pred$pred.fit <- NULL
cod.liny0pred

# Calculate the standard error of the prediction 
# using the saved sigma-estimate, and add it;
cod.liny0pred$se.pred <- 
  sqrt(cod.linsigma^2 + cod.liny0pred$se.fit^2)
cod.liny0pred

# Plot the data####

##Calculate yhat, confint, predint####
# Create a data frame with the data, the fitted line and
# confidence and prediction intervals:

cod.linpred <- 
  cbind(cod.data, 
        fit = predict(cod.linmod),
        conf = predict(cod.linmod, interval = "confidence"),
        pred = predict(cod.linmod, interval = "prediction"))
head(cod.linpred)
# get rid of the extra fits
cod.linpred$conf.fit <- cod.linpred$pred.fit <- NULL

##Plot everything####
(
  plot.linpred <- 
    ggplot(data = cod.linpred, 
           aes(x = length, y = weight)) + 
    geom_point() +
    geom_line(aes(y = fit), color = "blue", linewidth = 1) +
    geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2) +
    geom_line(data = cod.linpred, aes(y = pred.lwr),
              color = "red", linetype = "dashed", linewidth = 1) +
    geom_line(data = cod.linpred, aes(y = pred.upr),
              color = "red", linetype = "dashed", linewidth = 1) +
    xlab("Length (cm)") +
    ylab("Weight (g)") +
    labs(title = "Atlantic cod: weight by length") +
    labs(caption = 
           "fitted linear model, 95% conf. and pred. intervals") +
    theme(text = element_text(size = 18))
)

###Highlight observations####
# outside the prediction intervals:
(
  cod.linoutside <- which(
    cod.linpred$weight < cod.linpred$pred.lwr | 
      cod.linpred$weight > cod.linpred$pred.upr)
  )

### Replot with only the data outside the prediction interval####
ggplot(data = cod.linpred, aes(x = length, y = weight)) +
  geom_line(aes(y = pred.lwr), color = "red", 
            linetype = "dashed", linewidth = 1) +
  geom_line(aes(y = pred.upr), color = "red", 
            linetype = "dashed", linewidth = 1) +
  geom_point(data = cod.linpred[cod.linoutside, ], size = 3) +
  xlab("Length (cm)") +
  ylab("Weight (g)") +
  labs(title = "Atlantic cod: data outside the prediction interval") +
  labs(caption = "95% prediction interval") +
  theme(text = element_text(size = 18))

# Basic residual analysis####

## Add the residuals to the predicted data####
cod.linpred$e.lin <- cod.linmod$residuals
head(cod.linpred)

# Save the max-value in order to make the y-axins symmetrical 
# in the plots.
(max.elin <- max(abs(cod.linpred$e.lin)))
(cod.lim.elin <- c(-max.elin, max.elin))

## Plot against x####
# Add a horizontal line at y=0,
# add a moving average, geom_smooth(), to see trends,
# and expand the y-axis to include +/- max residual.

ggplot(data = cod.linpred, aes(x = length, y = e.lin)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  expand_limits(y = cod.lim.elin) +
  xlab("Length (cm)") +
  ylab("Residual") +
  labs(tag = "A") +
  labs(title = "Residuals vs x-values") +
  theme(text = element_text(size = 18))

## Plot against yhat####
ggplot(data = cod.linpred, aes(x = fit, y = e.lin)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  expand_limits(y = cod.lim.elin) +
  xlab("Predicted weight (g)") +
  ylab("Residual") +
  labs(title = "Residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 18))

## Normal qq-plot####
ggplot(data = cod.linpred, aes(sample = e.lin)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 18))

## Histogram####

ggplot(data = cod.linpred, aes(x = e.lin)) +
  geom_histogram(bins = 20) +
  xlab("Residuals") +
  labs(title = "Histogram of residuals") +
  theme(text = element_text(size = 18))
