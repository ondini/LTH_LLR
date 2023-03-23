# Lecture 1. Example 1. 20/3-23
# Simple linear regression: Ice cream####
# Part 1. Model and estimates

# Load ggplot2 to access the ggplot-command
library(ggplot2)

# Read in the data file.
# Assumes that the data is located in a subfolder called "Data".
# This is generally good practice!
# Keep your data in one subfolder,  
# your R-code in another, 
# your plots in a third folder,
# and your reports in a fourth.

# Load and look at the data####
load("Data/icecream.RData")

# Check what's in your environment:
ls()

# look at the first few lines:
head(ice.data)

# make a basic summary of the data
summary(ice.data)

# Plot the data as points:
# Tell ggplot in which data frame all necessary 
# variables for the plot are located, also specifying 
# an aesthetic stating the default x- and y-variables.
# Then add a geometry specifying that we want points
# with the default x- and y-variables on the axes.

ggplot(ice.data, aes(x = weeks, y = loss)) + geom_point()

# Fit the linear model####
# When a command saves its result in a variable, the
# default is to NOT print the result as well.
# The () around the command tells R to print it.

(ice.model <- lm(loss ~ weeks, data = ice.data))

## Get and save standard errors, etc####
(ice.summary <- summary(ice.model))

# Extract and save the sigma estimate from the summary
# so you can use it later.
(ice.sigma <- ice.summary$sigma)

# Extract the beta estimates table from the summary
ice.summary$coefficients

## Covariance matrix for beta####
# Extract (X'X)^{-1}
(ice.xtxinv <- ice.summary$cov.unscaled)

# Calculate the covariance matrix for beta:
(ice.beta.cov <- ice.sigma^2*ice.xtxinv)

# Just double checking that, yes, the standard 
# errors of the beta-estimates are the square roots 
# of the diagonal elements of the covariance matrix.
sqrt(diag(ice.beta.cov))
ice.summary$coefficients

#Predict and plot####

## Predict for x0 = 34####

# New data frame with the new x0-value(s) which must
# have the same name as in the data frame used to fit
# the model!.
# It is possible to do this for several x0-values at once.
# Use, e.g., weeks = c(34, 54, 23).
(ice.x0 <- data.frame(weeks = c(34)))

# Make a new data frame for the predictions:
# Add the fitted line, "fit", 
# and its standard error, "se.fit",
# degrees of freedom, "df" = n-(p+1), 
# and, yet another, sigma-estimate = "residual.scale";

(ice.y0.pred <- cbind(
  ice.x0,
  predict(ice.model, ice.x0, se.fit = TRUE)
  )
)

# We now have an extra copy of sigma.
# We can get rid of it by setting it to NULL:
ice.y0.pred$residual.scale <- NULL
ice.y0.pred

# Calculate the standard error of the prediction 
# using the previously saved sigma-estimate, 
# and add it;
ice.y0.pred$se.pred <- 
  sqrt(ice.sigma^2 + ice.y0.pred$se.fit^2)
ice.y0.pred

## Predict for all x####
# First add the fitted line to the original data.
# predict(model) predicts for the same data frame that was used in the estimation.
ice.data$yhat <- predict(ice.model)
head(ice.data)

## Plot the data####
(plot.data <- 
    ggplot(data = ice.data, aes(x = weeks, y = loss)) + 
    geom_point(size = 3) +
    xlab("Time in storage (weeks)") +
    ylab("Weight loss (g)") +
    labs(title = "Ice cream: weight loss by time in storage") +
    theme(text = element_text(size = 18))
)

## Add the fitted line####
(plot.line <- plot.data + 
   geom_line(aes(y = yhat), color = "blue", linewidth = 1) +
   labs(caption = "data and fitted line")
)

# Histograms of marginal distributions####
ggplot(ice.data, aes(x = loss)) + 
  geom_histogram(bins = 20) +
  xlab("Weight loss (g)") +
  labs(title = "Marginal distribution of weight loss") +
  labs(caption = "non-normal distribution") +
  theme(text = element_text(size = 18))

ggplot(ice.data, aes(x = weeks)) + 
  geom_histogram(bins = 20) +
  xlab("Storage time (weeks)") +
  labs(title = "Distribution of storage time") +
  labs(caption = "reason for the non-normal weight loss") +
  theme(text = element_text(size = 18))
