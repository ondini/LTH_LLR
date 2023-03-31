library(ggplot2)

# 1.A ####

## Load the data ####
weather <- read.csv("project1/data/weather.csv")
summary(weather)

## Fit the lin model ####
(linmod <- lm(rain ~ pressure, data = weather))
(linmod.sum <- summary(linmod))

linmod.sum$coefficients
confint(linmod)

model = linmod
model.pred <- 
  cbind(weather,
        fit = predict(model),
        conf = predict(model, interval = "confidence"),
        pred = predict(model, interval = "prediction"))
head(model.pred)
model.pred$conf.fit <- model.pred$pred.fit <- NULL


### Plot the fitted model ####
plot.data <- 
  ggplot(data = model.pred, 
         aes(x = pressure, y = rain)) + 
  geom_point() +
  theme(text = element_text(size = 18))

(
  plot.logfit <-plot.data + 
    geom_line(aes(y = fit), color = "blue", linewidth = 1) +
    geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2) +
    geom_line(aes(y = pred.lwr),
              color = "red", linetype = "dashed", linewidth = 1) +
    geom_line(aes(y = pred.upr),
              color = "red", linetype = "dashed", linewidth = 1) +
    xlab("Pressure (hPa)") +
    ylab("log(Rain (mm))") +
    labs(title = "Precipitations: log(Rain) vs Pressure") +
    labs(caption = 
           "fitted loglin model and 95% conf. and pred. intervals") +
    theme(text = element_text(size = 15))
)

# ggsave(filename = "project1/plots/logfitfull.png", plot = plot.logfitfull)

### Add and plot the residuals to the predicted data ####
model.pred$e.log <- model$residuals
head(model.pred)

(max.elog <- max(abs(model.pred$e.log)))
(pb.lim.elog <- c(-max.elog, max.elog))

#### Plot against x ####
( 
  plot.logresx <- ggplot(data = model.pred, aes(x = pressure, y = e.log)) +
    geom_point() +
    geom_hline(yintercept = 0) +
    geom_smooth() +
    expand_limits(y = pb.lim.elog) +
    xlab("Pressure (hPa)") +
    ylab("Residual") +
    labs(title = "Residuals vs x-values") +
    theme(text = element_text(size = 18))
)

# ggsave(filename = "project1/plots/logresx.png", plot = plot.logresx)

#### Plot against yhat ####
(
  plot.logresy <- ggplot(data = model.pred, aes(x = fit, y = e.log)) +
    geom_point() +
    geom_hline(yintercept = 0) +
    geom_smooth() +
    expand_limits(y = pb.lim.elog) +
    xlab("Predicted log(Rain(mm))") +
    ylab("Residual") +
    labs(title = "Residuals vs predicted values Y-hat") +
    theme(text = element_text(size = 18))
)

# ggsave(filename = "project1/plots/logresy.png", plot = plot.logresy)

#### Normal qq-plot####
(
  plot.logqq <- ggplot(data = model.pred, aes(sample = e.log)) +
    geom_qq() + geom_qq_line() +
    labs(title = "Normal Q-Q-plot of the residuals") +
    theme(text = element_text(size = 18))
)

# ggsave(filename = "project1/plots/logqq.png", plot = plot.logqq)

#### Histogram####
(
  plot.loghist <-ggplot(data = model.pred, aes(x = e.log)) +
    geom_histogram(bins = 20) +
    xlab("Residuals") +
    labs(title = "Histogram of residuals") +
    theme(text = element_text(size = 18))
)

# ggsave(filename = "project1/plots/loghist.png", plot = plot.loghist)


## Fit the loglin model ####

(logmod <- lm(log(rain) ~ pressure, data = weather))
(logmod.sum <- summary(logmod))

logmod.sum$coefficients
confint(logmod)

model = logmod
model.pred <- 
  cbind(weather,
        fit = predict(model),
        conf = predict(model, interval = "confidence"),
        pred = predict(model, interval = "prediction"))
head(model.pred)
model.pred$conf.fit <- model.pred$pred.fit <- NULL


### Plot the fitted model ####
plot.data <- 
  ggplot(data = model.pred, 
         aes(x = pressure, y = log(rain))) + 
  geom_point() +
  theme(text = element_text(size = 18))

(
  plot.logfit <-plot.data + 
    geom_line(aes(y = fit), color = "blue", linewidth = 1) +
    geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2) +
    geom_line(aes(y = pred.lwr),
              color = "red", linetype = "dashed", linewidth = 1) +
    geom_line(aes(y = pred.upr),
              color = "red", linetype = "dashed", linewidth = 1) +
    xlab("Pressure (hPa)") +
    ylab("log(Rain (mm))") +
    labs(title = "Precipitations: log(Rain) vs Pressure") +
    labs(caption = 
           "fitted loglin model and 95% conf. and pred. intervals") +
    theme(text = element_text(size = 15))
)

ggsave(filename = "project1/plots/logfit.png", plot = plot.logfit)

### Plot the fitted model in orig data ####
plot.data <- 
    ggplot(data = model.pred, aes(x = pressure, y = rain)) + 
    geom_point() +
    theme(text = element_text(size = 18))

(
  plot.logfitfull <- plot.data +
    geom_line(aes(y = exp(fit)),
              color = "blue", linewidth = 1) +
    geom_ribbon(aes(ymin = exp(conf.lwr), 
                    ymax = exp(conf.upr)),
                alpha = 0.2) +
    geom_line(aes(y = exp(pred.lwr)),
              color = "red", linetype = "dashed", linewidth = 1) +
    geom_line(aes(y = exp(pred.upr)),
              color = "red", linetype = "dashed", linewidth = 1) +
    xlab("Pressure (hPa)") +
    ylab("Rain (mm)") +
    labs(title = "Precipitations: Rain vs Pressure") +
    labs(caption = 
           "fitted loglin model and 95% conf. and pred. intervals") +
    theme(text = element_text(size = 15))
)

ggsave(filename = "project1/plots/logfitfull.png", plot = plot.logfitfull)

### Add and plot the residuals to the predicted data ####
model.pred$e.log <- model$residuals
head(model.pred)

(max.elog <- max(abs(model.pred$e.log)))
(pb.lim.elog <- c(-max.elog, max.elog))

#### Plot against x ####
( 
  plot.logresx <- ggplot(data = model.pred, aes(x = pressure, y = e.log)) +
    geom_point() +
    geom_hline(yintercept = 0) +
    geom_smooth() +
    expand_limits(y = pb.lim.elog) +
    xlab("Pressure (hPa)") +
    ylab("Residual") +
    labs(title = "Residuals vs x-values") +
    theme(text = element_text(size = 18))
)

ggsave(filename = "project1/plots/logresx.png", plot = plot.logresx)

#### Plot against yhat ####
(
  plot.logresy <- ggplot(data = model.pred, aes(x = fit, y = e.log)) +
    geom_point() +
    geom_hline(yintercept = 0) +
    geom_smooth() +
    expand_limits(y = pb.lim.elog) +
    xlab("Predicted log(Rain(mm))") +
    ylab("Residual") +
    labs(title = "Residuals vs predicted values Y-hat") +
    theme(text = element_text(size = 18))
)

ggsave(filename = "project1/plots/logresy.png", plot = plot.logresy)

#### Normal qq-plot####
(
  plot.logqq <- ggplot(data = model.pred, aes(sample = e.log)) +
    geom_qq() + geom_qq_line() +
    labs(title = "Normal Q-Q-plot of the residuals") +
    theme(text = element_text(size = 18))
)

ggsave(filename = "project1/plots/logqq.png", plot = plot.logqq)

#### Histogram####
(
  plot.loghist <-ggplot(data = model.pred, aes(x = e.log)) +
    geom_histogram(bins = 20) +
    xlab("Residuals") +
    labs(title = "Histogram of residuals") +
    theme(text = element_text(size = 18))
)

ggsave(filename = "project1/plots/loghist.png", plot = plot.loghist)

## Fit the cbrt model ####

(cbrtmod <- lm(I(rain^(1/3)) ~ pressure, data = weather))
(cbrtmod.sum <- summary(cbrtmod))

linmod.sum$coefficients
confint(linmod)


model = logmod
model.pred <- 
  cbind(weather,
        fit = predict(model),
        conf = predict(model, interval = "confidence"),
        pred = predict(model, interval = "prediction"))
head(model.pred)
model.pred$conf.fit <- model.pred$pred.fit <- NULL


### Plot the fitted model ####
plot.data <- 
  ggplot(data = model.pred, 
         aes(x = pressure, y = I(rain^(1/3)))) + 
  geom_point() +
  theme(text = element_text(size = 18))

(
  plot.logfit <-plot.data + 
    geom_line(aes(y = fit), color = "blue", linewidth = 1) +
    geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2) +
    geom_line(aes(y = pred.lwr),
              color = "red", linetype = "dashed", linewidth = 1) +
    geom_line(aes(y = pred.upr),
              color = "red", linetype = "dashed", linewidth = 1) +
    xlab("Pressure (hPa)") +
    ylab("log(Rain (mm))") +
    labs(title = "Precipitations: log(Rain) vs Pressure") +
    labs(caption = 
           "fitted loglin model and 95% conf. and pred. intervals") +
    theme(text = element_text(size = 15))
)

# ggsave(filename = "project1/plots/logfit.png", plot = plot.logfit)

### Plot the fitted model in orig data ####
plot.data <- 
  ggplot(data = model.pred, aes(x = pressure, y = rain)) + 
  geom_point() +
  theme(text = element_text(size = 18))

(
  plot.logfitfull <- plot.data +
    geom_line(aes(y = I((fit)^3)),
              color = "blue", linewidth = 1) +
    geom_ribbon(aes(ymin = I((conf.lwr)^3), 
                    ymax = I((conf.upr)^3)),
                alpha = 0.2) +
    geom_line(aes(y = I((pred.lwr)^3)),
              color = "red", linetype = "dashed", linewidth = 1) +
    geom_line(aes(y = I((pred.upr)^3)),
              color = "red", linetype = "dashed", linewidth = 1) +
    xlab("Pressure (hPa)") +
    ylab("Rain (mm)") +
    labs(title = "Precipitations: Rain vs Pressure") +
    labs(caption = 
           "fitted loglin model and 95% conf. and pred. intervals") +
    theme(text = element_text(size = 15))
)


# ggsave(filename = "project1/plots/logfitfull.png", plot = plot.logfitfull)

### Add and plot the residuals to the predicted data ####
model.pred$e.log <- model$residuals
head(model.pred)

(max.elog <- max(abs(model.pred$e.log)))
(pb.lim.elog <- c(-max.elog, max.elog))

#### Plot against x ####
( 
  plot.logresx <- ggplot(data = model.pred, aes(x = pressure, y = e.log)) +
    geom_point() +
    geom_hline(yintercept = 0) +
    geom_smooth() +
    expand_limits(y = pb.lim.elog) +
    xlab("Pressure (hPa)") +
    ylab("Residual") +
    labs(title = "Residuals vs x-values") +
    theme(text = element_text(size = 18))
)

# ggsave(filename = "project1/plots/logresx.png", plot = plot.logresx)

#### Plot against yhat ####
(
  plot.logresy <- ggplot(data = model.pred, aes(x = fit, y = e.log)) +
    geom_point() +
    geom_hline(yintercept = 0) +
    geom_smooth() +
    expand_limits(y = pb.lim.elog) +
    xlab("Predicted log(Rain(mm))") +
    ylab("Residual") +
    labs(title = "Residuals vs predicted values Y-hat") +
    theme(text = element_text(size = 18))
)

# ggsave(filename = "project1/plots/logresy.png", plot = plot.logresy)

#### Normal qq-plot####
(
  plot.logqq <- ggplot(data = model.pred, aes(sample = e.log)) +
    geom_qq() + geom_qq_line() +
    labs(title = "Normal Q-Q-plot of the residuals") +
    theme(text = element_text(size = 18))
)

# ggsave(filename = "project1/plots/logqq.png", plot = plot.logqq)

#### Histogram####
(
  plot.loghist <-ggplot(data = model.pred, aes(x = e.log)) +
    geom_histogram(bins = 20) +
    xlab("Residuals") +
    labs(title = "Histogram of residuals") +
    theme(text = element_text(size = 18))
)

# ggsave(filename = "project1/plots/loghist.png", plot = plot.loghist)

