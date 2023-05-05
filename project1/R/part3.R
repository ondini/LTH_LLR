library(ggplot2)

# Part 3 of project 1 ####
# 3.A ####

## Load the data ####
weather <- read.csv("project1/data/weather.csv")
summary(weather)

## Fit the cbrt model with interaction ####
(speedimod<- lm(I(rain^(1/3)) ~ pressure + location * speed, data = weather))
(speedimod.sum <- summary(speedimod))

confint(speedimod)

model = speedimod
model.pred <- 
  cbind(weather,
        fit = predict(model),
        conf = predict(model, interval = "confidence"),
        pred = predict(model, interval = "prediction"))

## Find influential observations####
model.pred$v <- influence(model)$hat
head(model.pred)

### Plot against something ####
ggplot(cbind(model.pred), aes(x = pressure, y = v)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 1/nrow(model.pred)) +
  geom_hline(yintercept = 2*length(model$coefficients)/nrow(model.pred), 
             color = "red") +
  labs(title = "Pike: leverage vs log length") +
  labs(caption = "y = 1/n (black) and 2(p+1)/n (red)") +
  theme(text = element_text(size = 18))

### Get 7 largest leverages from the plot ####
I.v.large <- which(model.pred$v > 0.025)
model.pred[I.v.large, ]


## Get the observation with largest Cook's D####
model.pred$D <- cooks.distance(model)
head(model.pred)

I.D.max <- which.max(model.pred$D)
model.pred[I.D.max, ]


## Plot airpressure against windspeed with marked influentials ####

(
  plot.infdata <- 
    ggplot(data = weather, 
           aes(x = pressure, y = speed)) + 
    geom_point(data = model.pred[I.v.large, ], 
               color = "red", size = 4, shape = 24) +
    geom_point(data = model.pred[I.D.max, ], 
               color = "blue", size = 4, shape = 23) +
    xlab("Pressure (hPa)") +
    ylab("Wind speed (m/s)") +
    geom_point() +
    facet_wrap(~location, scales = "free")
)

## Solve the DBFETAS thingy
db <- dfbetas(model)
db[I.D.max, ]


## Plot transformed rain against windspeed, as it is most influenced ####
# highlight the highest cook-D
(
  plot.infdata <- 
    ggplot(data = weather, 
           aes(x = speed, y=I(rain^(1/3)) )) + 
    geom_point(data = model.pred[I.D.max, ], 
               color = "blue", size = 4, shape = 23) +
    xlab("Pressure (hPa)") +
    ylab("Wind speed (m/s)") +
    geom_point() 
)

# 3.B ####

## Compute studentized residuals ####
model.pred$r <- rstudent(model)


### Plot r* vs yhat####
(
  plot.rdata <- 
    ggplot(model.pred, aes(x = fit, y = r)) +
    geom_point(size = 3) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = c(-2, 2)) +
    geom_hline(yintercept = c(-3, 3), linetype = "dashed") +
    geom_smooth() +
    xlab("Fitted values") +
    ylab("r*") +
    labs(title = "Studentized residuals vs fitted values") +
    labs(caption = "y = +/- 2 and +/- 3") +
    theme(text = element_text(size = 18))
)

ggsave(filename = "project1/plots/rdata.png", plot = plot.rdata)

##Plot sqrt(|r*|) vs yhat####
(
  plot.rsdata <- 
    ggplot(model.pred, aes(x = fit, y = sqrt(abs(r)))) +
    geom_point(size = 3) +
    geom_hline(yintercept = c(sqrt(qnorm(0.75)), sqrt(2))) +
    geom_hline(yintercept = sqrt(3), linetype = 2) +
    geom_smooth() +
    xlab("Fitted values") +
    ylab("sqrt(|r*|)") +
    labs(title = "Studentized residuals vs fitted values") +
    labs(caption = "y = 0.82, sqrt(2), sqrt(3)") +
    theme(text = element_text(size = 18))
)

ggsave(filename = "project1/plots/rsdata.png", plot = plot.rsdata)

# 3.C ####

## Add seasonal variable to dataframe ####
weather$monthnr <- as.factor(substr(weather$month, 6, 7))

## Fit the full and null models ####

fullmod <- lm(rain^(1/3) ~ pressure*location*speed*temp*monthnr, data = weather)

nullmod <- lm(rain^(1/3) ~ 1, data = weather)

## Choose best model using BIC and Stepwise selection ####
bicModel <- step(nullmod, scope = list(lower=nullmod,upper=fullmod),
     direction="both", criterion = "BIC",  k = log(nrow(weather)))

## Compare all models
model = bicModel # select model and run the commands below

model.sum <- summary(model)
length(model$coefficients)
model.sum$r.squared
model.sum$adj.r.squared
AIC(model)
BIC(model)

## Plot BIC model residuals ####

model = bicModel
model.pred <- 
  cbind(weather,
        fit = predict(model),
        conf = predict(model, interval = "confidence"),
        pred = predict(model, interval = "prediction"))


## Compute studentized residuals ####
model.pred$r <- rstudent(model)

### Plot r* vs yhat####
(
  plot.rdatabic <- 
    ggplot(model.pred, aes(x = fit, y = r)) +
    geom_point(size = 3) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = c(-2, 2)) +
    geom_hline(yintercept = c(-3, 3), linetype = "dashed") +
    geom_smooth() +
    xlab("Fitted values") +
    ylab("r*") +
    labs(title = "Studentized residuals vs fitted values") +
    labs(caption = "y = +/- 2 and +/- 3") +
    theme(text = element_text(size = 18))
)

ggsave(filename = "project1/plots/rdatabic.png", plot = plot.rdatabic)

##Plot sqrt(|r*|) vs yhat####
(
  plot.rsdatabic <- 
    ggplot(model.pred, aes(x = fit, y = sqrt(abs(r)))) +
    geom_point(size = 3) +
    geom_hline(yintercept = c(sqrt(qnorm(0.75)), sqrt(2))) +
    geom_hline(yintercept = sqrt(3), linetype = 2) +
    geom_smooth() +
    xlab("Fitted values") +
    ylab("sqrt(|r*|)") +
    labs(title = "Studentized residuals vs fitted values") +
    labs(caption = "y = 0.82, sqrt(2), sqrt(3)") +
    theme(text = element_text(size = 18))
)

ggsave(filename = "project1/plots/rsdatabitc.png", plot = plot.rsdatabic)

