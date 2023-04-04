library(ggplot2)

# 2.A ####

## Load the data ####
weather <- read.csv("project1/data/weather.csv")
summary(weather)

## Fit the cbrt model ####
(cbrtmod <- lm(I(rain^(1/3)) ~ pressure, data = weather))
(cbrtmod.sum <- summary(cbrtmod))

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

(locmod<- lm(I(rain^(1/3)) ~ pressure + location, data = weather))
(locmod.sum <- summary(locmod))

confint(locmod)

# (dmod<- lm(I(rain^(1/3)) ~ location, data = weather))

## Test signiicance against previous model ####
(model.anova <- anova(dmod, locmod))

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

  
