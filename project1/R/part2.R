library(ggplot2)

# Part 2 of project 1 ####
# 2.A ####

## Load the data ####
weather <- read.csv("project1/data/weather.csv")
summary(weather)

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

# 2.B ####

## Categorical variables ####
weather$location <- as.factor(weather$location)

table(weather$location)

### Add and plot the residuals to the predicted data ####
model.pred$e.cbrt <- model$residuals
head(model.pred)

(max.ecbrt <- max(abs(model.pred$e.cbrt)))
(pb.lim.ecbrt <- c(-max.ecbrt, max.ecbrt))

#### Plot against x ####
(
  plot.boxplot <- ggplot( model.pred, aes(x=location, y=e.cbrt, color=location)) + 
    geom_boxplot()+
    xlab("Pressure (hPa)") +
    ylab("Residual") +
    labs(title = "Residuals vs x-values and location") + 
    theme(text = element_text(size = 18))
)

ggsave(filename = "project1/plots/box.png", plot = plot.boxplot)

## Fit model with location ####

(locmod <- lm(I(rain^(1/3)) ~ pressure + location, data = weather))
(locmod.sum <- summary(locmod))

confint(locmod)

# (dmod<- lm(I(rain^(1/3)) ~ location, data = weather))

## Test signiicance against previous model ####
(model.anova <- anova(cbrtmod, locmod))

## Pred the model and compute residuals ####

model = locmod
model.pred <- 
  cbind(weather,
        fit = predict(model),
        conf = predict(model, interval = "confidence"),
        pred = predict(model, interval = "prediction"))
head(model.pred)
model.pred$conf.fit <- model.pred$pred.fit <- NULL

model.pred$e.cbrt <- model$residuals
head(model.pred)

(max.ecbrt <- max(abs(model.pred$e.cbrt)))
(pb.lim.ecbrt <- c(-max.ecbrt, max.ecbrt))

#### Plot against x ####
(
  plot.boxplotfit <- ggplot( model.pred, aes(x=location, y=e.cbrt, color=location)) + 
    geom_boxplot()+
    xlab("Pressure (hPa)") +
    ylab("Residual") +
    labs(title = "Residuals vs x-values and location") + 
    theme(text = element_text(size = 18))
)

ggsave(filename = "project1/plots/boxfit.png", plot = plot.boxplotfit)

## Plot transformed relations in one plot  ####
(
  plot.jointfit  <- ggplot(data = model.pred, aes(x = pressure, y = I(rain^(1/3)),
                                   shape = location, color = location)) +
    geom_point(size = 3) +
    geom_line(aes(y = fit), linewidth = 1) +
    xlab("Presshure [hPa]") +
    ylab("Precipitation [mm^(1/3)]") + 
    labs(title = "Precipitation^(1/3) as a function of location and pressure") +
    theme(text = element_text(size = 15))
)

ggsave(filename = "project1/plots/jointfit.png", plot = plot.jointfit)


# 2.C ####

## Plot the relations of speed, rain, location and pressure ####
(
  plot.data <- 
    ggplot(data = weather, 
           aes(x = speed, y = I(rain^(1/3)))) + 
    geom_point() +
    xlab("Wind speed (m/s)") +
    ylab("(Precipitation (mm))^(1/3)") +
    theme(text = element_text(size = 18))
  
)

ggsave(filename = "project1/plots/rainWspeed.png", plot = plot.data)

(
  plot.data <- 
    ggplot(data = weather, 
           aes(x = pressure, y = speed)) + 
    xlab("Pressure (hPa)") +
    ylab("Wind speed (m/s)") +
    geom_point() +
    theme(text = element_text(size = 18))
)

ggsave(filename = "project1/plots/speedWpressure.png", plot = plot.data)

(
  plot.boxplot <- ggplot( data=weather, aes(x=location, y=speed, color=location)) + 
    geom_boxplot()+
    xlab("Location") +
    ylab("Wind speed (m/s)") +
    theme(text = element_text(size = 18))
)

ggsave(filename = "project1/plots/speedWloc.png", plot = plot.boxplot)

## Fit the model with speed ####
(speedmod<- lm(I(rain^(1/3)) ~ pressure + location + speed, data = weather))
(speedmod.sum <- summary(speedmod))

## Test model significance ####

(anova(locmod, speedmod))

(speedmoddef<- lm(I(rain^(1/3)) ~ pressure + speed, data = weather))

(anova(speedmod, speedmoddef))

# 2.D ####

## Get residuals ####
model = speedmod

model.pred <- 
  cbind(weather,
        fit = predict(model),
        conf = predict(model, interval = "confidence"),
        pred = predict(model, interval = "prediction"))
head(model.pred)
model.pred$conf.fit <- model.pred$pred.fit <- NULL

model.pred$e.cbrt <- model$residuals
head(model.pred)

(max.ecbrt <- max(abs(model.pred$e.cbrt)))
(pb.lim.ecbrt <- c(-max.ecbrt, max.ecbrt))

### Plot against x ####
( 
  plot.speedresx <- ggplot(data = model.pred, aes(x = speed, y = e.cbrt)) +
    geom_point() +
    geom_hline(yintercept = 0) +
    geom_smooth() +
    expand_limits(y = pb.lim.ecbrt) +
    xlab("Speed (m/s)") +
    ylab("Residual") +
    theme(text = element_text(size = 18)) +
    facet_wrap(~location, scales = "free_x")
)

ggsave(filename = "project1/plots/speedresx.png", plot = plot.speedresx)

## Fit new model with interaction ####
(speedimod<- lm(I(rain^(1/3)) ~ pressure + location * speed, data = weather))
(speedimod.sum <- summary(speedimod))

confint(speedimod)

## Partial F-test ####
(pf34 <- anova(speedmod, speedimod))


# Compare the F-value with upper F-quantile 
(Fvalue <- pf34$F[2])
qf(1 - 0.05, 2, 1489)

# Get P-value
pf(Fvalue, 2, 1489, lower.tail = FALSE)
pf34$`Pr(>F)`[2]


## Get residuals ####
model = speedimod

model.pred <- 
  cbind(weather,
        fit = predict(model),
        conf = predict(model, interval = "confidence"),
        pred = predict(model, interval = "prediction"))
head(model.pred)
model.pred$conf.fit <- model.pred$pred.fit <- NULL

model.pred$e.cbrt <- model$residuals
head(model.pred)

(max.ecbrt <- max(abs(model.pred$e.cbrt)))
(pb.lim.ecbrt <- c(-max.ecbrt, max.ecbrt))

### Plot against x ####
( 
  plot.speediresx <- ggplot(data = model.pred, aes(x = speed, y = e.cbrt)) +
    geom_point() +
    geom_hline(yintercept = 0) +
    geom_smooth() +
    expand_limits(y = pb.lim.ecbrt) +
    xlab("Speed (m/s)") +
    ylab("Residual") +
    theme(text = element_text(size = 18)) +
    facet_wrap(~location, scales = "free_x")
)

ggsave(filename = "project1/plots/speediresx.png", plot = plot.speediresx)


# 2.E ####

## Preditct values for all models ####

### Create query points ####
(model.query <- rbind(data.frame(pressure = c(1011), speed=c(2), location=c("Lund","Uppsala", "Katterjåkk")),
                  data.frame(pressure = c(1011), speed=c(5), location=c("Lund","Uppsala", "Katterjåkk")))
)

### Predict for all models
model.query$locmod <- predict(locmod, model.query)^3
model.query$speedmod <- predict(speedmod, model.query)^3
model.query$speedimod <- predict(speedimod, model.query)^3
model.query
