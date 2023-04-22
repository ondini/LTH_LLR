# import packages
library(ggplot2)

# fuseful function for better dataloading
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

# 3.A ####
## Load and look at the data ####
pb <- loadRData("lab2/data/Pb_all.rda")

head(pb)
summary(pb)

# count regions and unique values
length(unique(pb$region))
table(pb$region)

## Fit a lin-lin and log-lin model without region ####
mod_1 <- lm(Pb ~  I(year-1975), data = pb)
(sum_1 <- summary(mod_1))

mod_2 <- lm(log(Pb) ~  I(year-1975), data = pb)
(sum_2 <- summary(mod_2))


## Get preds. and intervals for both models####
mod_1.pred <- 
  cbind(pb, 
        fit = predict(mod_1),
        conf = predict(mod_1, interval = "confidence"),
        pred = predict(mod_1, interval = "prediction"))

mod_2.pred <- 
  cbind(pb, 
        fit = predict(mod_2),
        conf = predict(mod_2, interval = "confidence"),
        pred = predict(mod_2, interval = "prediction"))


## Get leverages (influences) for models####
mod_1.pred$v <- influence(mod_1)$hat
head(mod_1.pred)

mod_2.pred$v <- influence(mod_2)$hat
head(mod_2.pred)


###  Plot them with 1/n and 2(p+1)/n horizontal lines ####
# print p+1 for both models and n
length(mod_1$coefficients)
length(mod_2$coefficients)
nrow(pb)

ggplot(cbind(mod_1.pred), aes(x = year, y = v)) +
  geom_jitter(width = 1)+
  geom_hline(yintercept = 1/nrow(pb)) +
  geom_hline(yintercept = 2*length(mod_1$coefficients)/nrow(pb), 
             color = "red") +
  labs(caption = "y = 1/n (black) and 2(p+1)/n (red)") +
  theme(text = element_text(size = 18))

ggplot(cbind(mod_2.pred), aes(x = year, y = v)) +
  geom_jitter(width = 1) +
  geom_hline(yintercept = 1/nrow(pb)) +
  geom_hline(yintercept = 2*length(mod_2$coefficients)/nrow(pb), 
             color = "red") +
  labs(caption = "y = 1/n (black) and 2(p+1)/n (red)") +
  theme(text = element_text(size = 18))

## Get mean of year####
mean(mod_1.pred$year)

## Fit model with regions, Vasternorrland as reference ####
pb$region <- relevel(pb$region, "Vasternorrland")
mod_3 <- lm(log(Pb) ~  I(year-1975) + region, data = pb)
(sum_3 <- summary(mod_3))


## Get preds. and intervals and leverage for the model ####
mod_3.pred <- 
  cbind(pb, 
        fit = predict(mod_3),
        conf = predict(mod_3, interval = "confidence"),
        pred = predict(mod_3, interval = "prediction"))

mod_3.pred$v <- influence(mod_3)$hat
head(mod_3.pred)

length(mod_3$coefficients)
nrow(pb)

### Plot the leverages ####
ggplot(cbind(mod_3.pred), aes(x = year, y = v)) +
  geom_jitter(width = 1, aes(color = region))+
  geom_hline(yintercept = 1/nrow(pb)) +
  geom_hline(yintercept = 2*length(mod_3$coefficients)/nrow(pb), 
             color = "red") +
  labs(caption = "y = 1/n (black) and 2(p+1)/n (red)") +
  theme(text = element_text(size = 18))

## Compute studentized residuals ####
mod_3.pred$r <- rstudent(mod_3)

### Plot r* against yhat ####

ggplot(mod_3.pred, aes(x = fit, y = r)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2)) +
  geom_hline(yintercept = c(-3, 3), linetype = "dashed") +
  geom_smooth() +
  xlab("Fitted values") +
  ylab("r*") +
  labs(caption = "y = +/- 2 and +/- 3") +
  theme(text = element_text(size = 18))

# Get number of large residuals
sum(abs(mod_3.pred$r)>3)

### Plot r* against yhat by regions####

ggplot(mod_3.pred, aes(x = fit, y = r)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2)) +
  geom_hline(yintercept = c(-3, 3), linetype = "dashed") +
  geom_smooth() +
  xlab("Fitted values") +
  ylab("r*") +
  labs(caption = "y = +/- 2 and +/- 3") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ region) 

## Compute cook's distance ####
mod_3.pred$D <- cooks.distance(mod_3)

### Plot cook's distance against year ####
(f1.exc <- length(mod_3$coefficients))
(f2.exc <- mod_3$df.residual)
(cook.limit.exc <- qf(0.5, f1.exc, f2.exc))

ggplot(mod_3.pred, aes(fit, D)) + 
  geom_point(size = 3) +
  # geom_hline(yintercept = cook.limit.exc, color = "red") +
  geom_hline(yintercept = 4/nrow(mod_3.pred), linetype = 2, color = "red") +
  xlab("Fitted values") +
  ylab("D_i") +
  labs(title = "Pike: Cook's D",
       subtitle = "without the strange fish") +
  labs(caption = "4/n (dashed), F_0.5, p+1, n-(p+1) (solid)") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ region) 

## Compute DFBETAS for time####
head(dfbetas(mod_3))
mod_3.pred$dbf <- dfbetas(mod_3)[, "I(year - 1975)"]

### Plot dfbetas(time) vs yhat by regions ####
ggplot(mod_3.pred, aes(x = fit, y = dbf)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0) +
  # geom_hline(yintercept = sqrt(cook.limit.exc)*c(-1, 1), color = "red") +
  geom_hline(yintercept = 2/sqrt(nrow(mod_3.pred))*c(-1, 1), color = "red", linetype = "dashed") +
  ylab("DFBETAS_0(i)") +
  xlab("Fitted values") +
  labs(title = "Pike: DFBETAS_0: impact on the intercept",
       subtitle = "without the strange fish") +
  labs(caption = "y = sqrt(F_0.5) and 2/sqrt(n)") +
  theme(text = element_text(size = 18)) + 
  facet_wrap(~ region) 

# 3.B ####
## Fit the product model ####

mod_4 <- lm(log(Pb) ~  I(year-1975) * region, data = pb)
(sum_4 <- summary(mod_4))

## Compare to prev model using partial F-test ####

(pf34 <- anova(mod_3, mod_4))

# Compare the F-value with upper F-quantile
(Fvalue <- pf34$F[2])
qf(1 - 0.05, 4, 1221)

# Get P-value
pf(Fvalue, 4, 1221, lower.tail = FALSE)
pf34$`Pr(>F)`[2]

## Get R^2, adjR^2, BIC and AIC for all models ####

sum_2$r.squared
sum_2$adj.r.squared
AIC(mod_2)
BIC(mod_2)

sum_3$r.squared
sum_3$adj.r.squared
AIC(mod_3)
BIC(mod_3)

sum_4$r.squared
sum_4$adj.r.squared
AIC(mod_4)
BIC(mod_4)
