library(ggplot2)

# Part 1 of project 1 ####

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
  plot.linfit <- plot.data + 
    geom_line(aes(y = fit), color = "blue", linewidth = 1) +
    geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2) +
    geom_line(aes(y = pred.lwr),
              color = "red", linetype = "dashed", linewidth = 1) +
    geom_line(aes(y = pred.upr),
              color = "red", linetype = "dashed", linewidth = 1) +
    xlab("Pressure (hPa)") +
    ylab("Rain (mm)") +
    labs(title = "Precipitations: Rain vs Pressure") +
    labs(caption = 
           "fitted linlin model and 95% conf. and pred. intervals") +
    theme(text = element_text(size = 15))
)

# ggsave(filename = "project1/plots/linfitfull.png", plot = plot.linfitfull)

### Add and plot the residuals to the predicted data ####
model.pred$e.lin <- model$residuals
head(model.pred)

(max.elin <- max(abs(model.pred$e.lin)))
(pb.lim.elin <- c(-max.elin, max.elin))

#### Plot against x ####
( 
  plot.linresx <- ggplot(data = model.pred, aes(x = pressure, y = e.lin)) +
    geom_point() +
    geom_hline(yintercept = 0) +
    geom_smooth() +
    expand_limits(y = pb.lim.elin) +
    xlab("Pressure (hPa)") +
    ylab("Residual") +
    labs(title = "Residuals vs x-values") +
    theme(text = element_text(size = 18))
)

ggsave(filename = "project1/plots/linresx.png", plot = plot.linresx)

#### Plot against yhat ####
(
  plot.linresy <- ggplot(data = model.pred, aes(x = fit, y = e.lin)) +
    geom_point() +
    geom_hline(yintercept = 0) +
    geom_smooth() +
    expand_limits(y = pb.lim.elin) +
    xlab("Predicted Rain(mm)") +
    ylab("Residual") +
    labs(title = "Residuals vs predicted values Y-hat") +
    theme(text = element_text(size = 18))
)

ggsave(filename = "project1/plots/linresy.png", plot = plot.linresy)

#### Normal qq-plot####
(
  plot.linqq <- ggplot(data = model.pred, aes(sample = e.lin)) +
    geom_qq() + geom_qq_line() +
    labs(title = "Normal Q-Q-plot of the residuals") +
    theme(text = element_text(size = 18))
)

# ggsave(filename = "project1/plots/linqq.png", plot = plot.linqq)

#### Histogram####
(
  plot.linhist <-ggplot(data = model.pred, aes(x = e.lin)) +
    geom_histogram(bins = 20) +
    xlab("Residuals") +
    labs(title = "Histogram of residuals") +
    theme(text = element_text(size = 18))
)

# ggsave(filename = "project1/plots/linhist.png", plot = plot.linhist)


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

cbrtmod.sum$coefficients
confint(cbrtmod)


model = cbrtmod
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
  plot.cbrtfit <-plot.data + 
    geom_line(aes(y = fit), color = "blue", linewidth = 1) +
    geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2) +
    geom_line(aes(y = pred.lwr),
              color = "red", linetype = "dashed", linewidth = 1) +
    geom_line(aes(y = pred.upr),
              color = "red", linetype = "dashed", linewidth = 1) +
    xlab("Pressure (hPa)") +
    ylab("cbrt(Rain (mm))") +
    labs(title = "Precipitations: cbrt(Rain) vs Pressure") +
    labs(caption = 
           "fitted cbrtlin model and 95% conf. and pred. intervals") +
    theme(text = element_text(size = 15))
)

# ggsave(filename = "project1/plots/cbrtfit.png", plot = plot.cbrtfit)

### Plot the fitted model in orig data ####
plot.data <- 
  ggplot(data = model.pred, aes(x = pressure, y = rain)) + 
  geom_point() +
  theme(text = element_text(size = 18))

(
  plot.cbrtfitfull <- plot.data +
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
           "fitted cbrtlin model and 95% conf. and pred. intervals") +
    theme(text = element_text(size = 15))
)


# ggsave(filename = "project1/plots/cbrtfitfull.png", plot = plot.cbrtfitfull)

### Add and plot the residuals to the predicted data ####
model.pred$e.cbrt <- model$residuals
head(model.pred)

(max.ecbrt <- max(abs(model.pred$e.cbrt)))
(pb.lim.ecbrt <- c(-max.ecbrt, max.ecbrt))

#### Plot against x ####
( 
  plot.cbrtresx <- ggplot(data = model.pred, aes(x = pressure, y = e.cbrt)) +
    geom_point() +
    geom_hline(yintercept = 0) +
    geom_smooth() +
    expand_limits(y = pb.lim.ecbrt) +
    xlab("Pressure (hPa)") +
    ylab("Residual") +
    labs(title = "Residuals vs x-values") +
    theme(text = element_text(size = 18))
)

# ggsave(filename = "project1/plots/cbrtresx.png", plot = plot.cbrtresx)

#### Plot against yhat ####
(
  plot.cbrtresy <- ggplot(data = model.pred, aes(x = fit, y = e.cbrt)) +
    geom_point() +
    geom_hline(yintercept = 0) +
    geom_smooth() +
    expand_limits(y = pb.lim.ecbrt) +
    xlab("Predicted cbrt(Rain(mm))") +
    ylab("Residual") +
    labs(title = "Residuals vs predicted values Y-hat") +
    theme(text = element_text(size = 18))
)

# ggsave(filename = "project1/plots/cbrtresy.png", plot = plot.cbrtresy)

#### Normal qq-plot####
(
  plot.cbrtqq <- ggplot(data = model.pred, aes(sample = e.cbrt)) +
    geom_qq() + geom_qq_line() +
    labs(title = "Normal Q-Q-plot of the residuals") +
    theme(text = element_text(size = 18))
)

# ggsave(filename = "project1/plots/cbrtqq.png", plot = plot.cbrtqq)

#### Histogram####
(
  plot.cbrthist <-ggplot(data = model.pred, aes(x = e.cbrt)) +
    geom_histogram(bins = 20) +
    xlab("Residuals") +
    labs(title = "Histogram of residuals") +
    theme(text = element_text(size = 18))
)

# ggsave(filename = "project1/plots/cbrthist.png", plot = plot.cbrthist)


