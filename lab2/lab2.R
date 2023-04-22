# import packages
library(ggplot2)

# useful function
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

# 2.A ####
## Load and look at the data ####
pb <- loadRData("lab2/data/Pb_all.rda")

head(pb)
summary(pb)

# count regions and unique values
length(unique(pb$region))
table(pb$region)

## Fit a log-lin model ####
model.y <- lm(log(Pb) ~  I(year-1975), data = pb)
(sum.full <- summary(model.y))

## Intervals for the all x####
model.pred <- 
  cbind(pb, 
        fit = predict(model.y),
        conf = predict(model.y, interval = "confidence"),
        pred = predict(model.y, interval = "prediction"))
head(model.pred)

## Plot the fitted model ####

(
  plot.data <- 
    ggplot(data = model.pred, aes(x = year, y = Pb)) + 
    geom_point(aes(color = region), size = 3)+
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


## Predict a single value ####

(model.x0 <- data.frame(year = c(2002)))
model.y0.pred <- cbind(model.x0,
                     fit = predict(model.y, model.x0),
                     conf = predict(model.y, model.x0, interval = "confidence"),
                     pred = predict(model.y, model.x0, interval = "prediction"))
model.y0.pred$conf.fit <- model.y0.pred$pred.fit <- NULL
exp(model.y0.pred)

# get rid of the extra fits
model.pred$conf.fit <- model.pred$pred.fit <- NULL
head(model.pred)

## Add and plot the residuals to the predicted data ####
model.pred$e <- model.y$residuals
head(model.pred)

# Save the max-value in order to make the y-axis symmetrical 
# in the plots.
(max.e <- max(abs(model.pred$e)))
(model.elims <- c(-max.e, max.e))

### Plot against x####
ggplot( data = model.pred, 
        aes(x = year, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = model.elims) +
  ylab("Residual") +
  labs(title = "Residuals vs x-values") +
  theme(text = element_text(size = 18)
  ) +
facet_wrap(~ region) 

### Plot against yhat####
ggplot(data = model.pred, aes(x = fit, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = model.elims) +
  ylab("Residual") +
  labs(title = "Residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ region) 

### Normal qq-plot####
ggplot(data = model.pred, aes(sample = e)) +
  geom_qq(size = 3) +
  geom_qq_line() +
  labs(tag = "C") +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ region) 

### Residuals histogram####
ggplot(data = model.pred, aes(x = e)) +
  geom_histogram(bins = 10) +
  xlab("Residuals") +
  labs(title = "Histogram of residuals") +
  theme(text = element_text(size = 18))+
  facet_wrap(~ region) 


# 2.B ####

## Show dirstributions by regions ####

ggplot(data = pb, aes(x = year, y = Pb)) + 
  geom_point() + 
  facet_wrap(~ region) 

ggplot(data = pb, aes(x = year, y = log(Pb))) + 
  geom_point() + 
  facet_wrap(~ region) 


pb.logsum$coefficients
confint(pb.logmod)

## Fit (and refit) the log-lin model ####
model.f <- lm(log(Pb) ~  I(year-1975) + region, data = pb)
(sum.s <- summary(model.f))

# show region frequency  
table(pb$region)
# not a good idea to use Orebro as reference, since Norr. is the most freq. one 
pb$region <- relevel(pb$region, "Vasternorrland")
model.f <- lm(log(Pb) ~  I(year-1975) + region, data = pb)
(sum.s <- summary(model.f))

## Get coeff. data ### 
exp(sum.s$coefficients)
exp(confint(model.f))

## Predicted single value with intervals ####
(model.f.x0 <- data.frame(year = c(2002), region="Vasternorrland"))
model.f.y0.pred <- cbind(model.f.x0,
                     fit = predict(model.f, model.f.x0),
                     conf = predict(model.f, model.f.x0, interval = "confidence"),
                     pred = predict(model.f, model.f.x0, interval = "prediction"))
model.f.y0.pred$conf.fit<- model.f.y0.pred$region  <- model.f.y0.pred$pred.fit <- NULL
exp(model.f.y0.pred)

(model.f.x0 <- data.frame(year = c(1975), region="Orebro"))
model.f.y0.pred <- cbind(model.f.x0,
                         fit = predict(model.f, model.f.x0),
                         conf = predict(model.f, model.f.x0, interval = "confidence"),
                         pred = predict(model.f, model.f.x0, interval = "prediction"))
model.f.y0.pred$conf.fit<- model.f.y0.pred$region  <- model.f.y0.pred$pred.fit <- NULL
exp(model.f.y0.pred)

(model.f.x0 <- data.frame(year = c(2002), region="Orebro"))
model.f.y0.pred <- cbind(model.f.x0,
                         fit = predict(model.f, model.f.x0),
                         conf = predict(model.f, model.f.x0, interval = "confidence"),
                         pred = predict(model.f, model.f.x0, interval = "prediction"))
model.f.y0.pred$conf.fit<- model.f.y0.pred$region  <- model.f.y0.pred$pred.fit <- NULL
exp(model.f.y0.pred)

#  2.C ####

## Fit the model ####
model.f <- lm(log(Pb) ~  I(year-1975) + region, data = pb)
(sum.s <- summary(model.f))

sum.s$coefficients

model.r <- lm(log(Pb) ~  I(year-1975), data = pb)
(model.anova <- anova(model.r, model.f))

## PErform the partial F-Statistics ####
### Compare the F-value with upper F-quantile####
(Fvalue <- model.anova$F[2])
qf(1 - 0.05, 4, 1225)

### Calculate P-value####
pf(Fvalue, 4, 1225, lower.tail = FALSE)
model.anova$`Pr(>F)`[2]


## Add and plot the residuals to the predicted data ####
model.pred$e <- model.f$residuals
head(model.pred)

# Save the max-value in order to make the y-axis symmetrical 
# in the plots.
(max.e <- max(abs(model.pred$e)))
(model.elims <- c(-max.e, max.e))

### Plot against x####
ggplot( data = model.pred, 
        aes(x = year, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = model.elims) +
  ylab("Residual") +
  labs(title = "Residuals vs x-values") +
  theme(text = element_text(size = 18)
  ) +
  facet_wrap(~ region) 

### Plot against yhat####
ggplot(data = model.pred, aes(x = fit, y = e)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  expand_limits(y = model.elims) +
  ylab("Residual") +
  labs(title = "Residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ region) 

### Normal qq-plot####
ggplot(data = model.pred, aes(sample = e)) +
  geom_qq(size = 3) +
  geom_qq_line() +
  labs(tag = "C") +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ region) 

### Residuals histogram####
ggplot(data = model.pred, aes(x = e)) +
  geom_histogram(bins = 10) +
  xlab("Residuals") +
  labs(title = "Histogram of residuals") +
  theme(text = element_text(size = 18))+
  facet_wrap(~ region) 

