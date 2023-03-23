# import packages
library(ggplot2)

# useful function
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

# 1.A ####
## Load and look at the data ####
pb.data <- loadRData("lab1/data/Pb_Vasternorrland.rda")
summary(pb.data)
head(pb.data)

(plot.data <- 
    ggplot(data = pb.data, aes(x = year , y = Pb))  + geom_point()
)
# we can see, that the relation is not linear and we did not expect it to be

## Fit the linear model ####
(pb.model <- lm(Pb ~ I(year - 1975), data = pb.data))
(pb.summary <- summary(pb.model))

## Predict for all x ####
pb.data$yhat <- predict(pb.model)
head(pb.data)

## Plot the fitted model ####
(plot.data <- 
   ggplot(data = pb.data, aes(x = I(year-1975) , y = Pb))  + geom_point()
)

(plot.line <- plot.data + 
   geom_line(aes(y = yhat), color = "blue", linewidth = 1)
)

## Calculate the coeff. confidence intervals #####
pb.summary$coefficients
confint(pb.model)

## Predict a single value ####
(pb.x0 <- data.frame(year = c(2002)))
pb.y0.pred <- cbind(pb.x0,
                      fit = predict(pb.model, pb.x0),
                      conf = predict(pb.model, pb.x0, interval = "confidence"),
                      pred = predict(pb.model, pb.x0, interval = "prediction"))
pb.y0.pred$conf.fit <- pb.y0.pred$pred.fit <- NULL
pb.y0.pred

## Intervals for the all x####
pb.pred <- 
  cbind(pb.data, 
        fit = predict(pb.model),
        conf = predict(pb.model, interval = "confidence"),
        pred = predict(pb.model, interval = "prediction"))
head(pb.pred)
# get rid of the extra fits
pb.pred$conf.fit <- pb.pred$pred.fit <- NULL
head(pb.pred)

## Plot the data with conf interval and fitted line####

# Make the basic plot
(
  plot.data <- 
    ggplot(data = pb.pred, aes(x = year, y = Pb)) + 
    geom_point(size = 3)
)

#  Add the fitted line
(
  plot.line <- plot.data + 
    geom_line(aes(y = fit), color = "blue", linewidth = 1)
)

# Add confidence intervals
(
  plot.conf <- plot.line + 
    geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2)
)

# Add prediction interval
plot.conf +
  geom_line(aes(y = pred.lwr),
            color = "red", linetype = "dashed", linewidth = 1) +
  geom_line(aes(y = pred.upr),
            color = "red", linetype = "dashed", linewidth = 1) +
  labs(caption = "data, fitted line, 95% confidence and prediction intervals")


## Add the residuals to the predicted data ####
pb.pred$e <- pb.model$residuals
head(pb.pred)

# Save the max-value in order to make the y-axis symmetrical 
# in the plots.
(max.e <- max(abs(pb.pred$e)))
(pb.elims <- c(-max.e, max.e))

## Plot against x####
# Add a horizontal line at y=0,
# and expand the y-axis to include +/- max residual.

ggplot( data = pb.pred, 
        aes(x = year, y = e)) +
        geom_point(size = 3) +
        geom_hline(yintercept = 0) +
        expand_limits(y = pb.elims) +
        ylab("Residual") +
        labs(title = "Residuals vs x-values") +
        theme(text = element_text(size = 18)
      )

## Plot against yhat####
# Add a horizontal line at y=0,
# and expand the y-axis to include +/- max residual.

ggplot(data = pb.pred, aes(x = fit, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = pb.elims) +
  ylab("Residual") +
  labs(title = "Residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 18))

##Normal qq-plot####
ggplot(data = pb.pred, aes(sample = e)) +
  geom_qq(size = 3) +
  geom_qq_line() +
  labs(tag = "C") +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 18))

## Histogram####
ggplot(data = pb.pred, aes(x = e)) +
  geom_histogram(bins = 10) +
  xlab("Residuals") +
  labs(title = "Histogram of residuals") +
  theme(text = element_text(size = 18))

# 1.B ####

## Fit the loglin model####
(pb.logmod <- lm(log(Pb) ~ I(year-1975), data = pb.data))
(pb.logsum <- summary(pb.logmod))

## Get CIs of coefficients####

pb.logsum$coefficients
confint(pb.logmod)

## Predicted single value with intervals ####
(pb.xl0 <- data.frame(year = c(2002)))
pb.yl0.pred <- cbind(pb.xl0,
                     fit = predict(pb.logmod, pb.xl0),
                     conf = predict(pb.logmod, pb.xl0, interval = "confidence"),
                     pred = predict(pb.logmod, pb.xl0, interval = "prediction"))
pb.yl0.pred$conf.fit <- pb.yl0.pred$pred.fit <- NULL
pb.yl0.pred


## Calculate fitted line and intervals####
pb.logpred <- 
  cbind(pb.data,
        fit = predict(pb.logmod),
        conf = predict(pb.logmod, interval = "confidence"),
        pred = predict(pb.logmod, interval = "prediction"))
head(pb.logpred)
pb.logpred$conf.fit <- pb.logpred$pred.fit <- NULL


## Plot the fitted log-model ####
(
  plot.logdata <- 
    ggplot(data = pb.logpred, 
           aes(x = year, y = log(Pb))) + 
    geom_point() +
    theme(text = element_text(size = 18))
)

plot.logdata + 
  geom_line(aes(y = fit), color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2) +
  geom_line(aes(y = pred.lwr),
            color = "red", linetype = "dashed", linewidth = 1) +
  geom_line(aes(y = pred.upr),
            color = "red", linetype = "dashed", linewidth = 1)

## Plot the fitted orig model ####
(
  plot.data <- 
    ggplot(data = pb.logpred, aes(x = year, y = Pb)) + 
    geom_point() +
    theme(text = element_text(size = 18))
)

plot.data +
  geom_line(aes(y = exp(fit)),
            color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = exp(conf.lwr), 
                  ymax = exp(conf.upr)),
              alpha = 0.2) +
  geom_line(aes(y = exp(pred.lwr)),
            color = "red", linetype = "dashed", linewidth = 1) +
  geom_line(aes(y = exp(pred.upr)),
            color = "red", linetype = "dashed", linewidth = 1) 

## Add residuals to predicted data####
pb.logpred$e.log <- pb.logmod$residuals
head(pb.logpred)

# Save the max-value in order to make the y-axins symmetrical 
# in the plots.
(max.elog <- max(abs(pb.logpred$e.log)))
(pb.lim.elog <- c(-max.elog, max.elog))

## Plot against x####
# Add a horizontal line at y=0,
# and expand the y-axis to include +/- max residual.

ggplot(data = pb.logpred, 
       aes(x = year, y = e.log)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  expand_limits(y = pb.lim.elog) +
  xlab("ln(Length (cm))") +
  ylab("Residual") +
  labs(title = "Residuals vs x-values") +
  theme(text = element_text(size = 18))

## Plot against yhat####
ggplot(data = pb.logpred, aes(x = fit, y = e.log)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  expand_limits(y = pb.lim.elog) +
  xlab("Predicted ln weight") +
  ylab("Residual") +
  labs(title = "Residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 18))

##Normal qq-plot####
ggplot(data = pb.logpred, aes(sample = e.log)) +
  geom_qq() + geom_qq_line() +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 18))

##Histogram####
ggplot(data = pb.logpred, aes(x = e.log)) +
  geom_histogram(bins = 20) +
  xlab("Residuals") +
  labs(title = "Histogram of residuals") +
  theme(text = element_text(size = 18))


# 1.C ####

## Calculate betas and the confidence intervals for betas ####
exp(pb.logsum$coefficients[1])
exp(pb.logsum$coefficients[2])

exp(confint(pb.logmod))

## Predict for a single value ####

(pb.xl0 <- data.frame(year = c(2002)))
pb.yl0.pred <- cbind(pb.xl0,
                     fit = predict(pb.logmod, pb.xl0),
                     conf = predict(pb.logmod, pb.xl0, interval = "confidence"),
                     pred = predict(pb.logmod, pb.xl0, interval = "prediction"))
pb.yl0.pred$conf.fit <- pb.yl0.pred$pred.fit <- NULL
exp(pb.yl0.pred)

