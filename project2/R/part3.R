
library(ggplot2)
library(pROC)
library(ResourceSelection)

# Part 3 of project 2 ####

## Load the data ####
weather <- read.csv("project2/data/weather.csv")
summary(weather)

## Add variables to the df #### 
(Q1 <- quantile(weather$rain, 0.25))
weather$lowrain <- as.numeric(weather$rain < Q1)
weather$low_cat <- factor(weather$lowrain,
                          levels = c(0, 1),
                          labels = c("high", "low"))
weather$monthnr <- as.factor(substr(weather$month, 6, 7))

## Fit full model ####
fullmod <- glm(lowrain ~ pressure*location*speed*temp*monthnr, family = "binomial", data = weather)
(sum_full <- summary(fullmod))

nullmod <- glm(lowrain ~1, family = "binomial", data = weather)

## Choose best models using BIC and Stepwise selection ####
fwmodel <- step(nullmod, scope = list(lower=nullmod,upper=fullmod),
                 direction="both", criterion = "BIC",  k = log(nrow(weather)))
(fwmodel.sum <- summary(fwmodel))

bwmodel <- step(fullmod, scope = list(lower=nullmod,upper=fullmod),
                direction="both", criterion = "BIC",  k = log(nrow(weather)))
(bwmodel.sum <- summary(bwmodel))

model.sum <- summary(fwmodel)



## Make confusion matrix ####
pred.phat <- cbind(
  weather,
  p.1 = predict(mod_1, type = "response"),
  p.2 = predict(mod_2, type = "response"),
  p.fw = predict(fwmodel, type = "response"),
  p.bw = predict(bwmodel, type = "response")
)

pred.phat$yhat <- as.numeric(pred.phat$p.bw > 0.5)

head(pred.phat)

(row.01 <- table(weather$lowrain))
(col.01 <- table(pred.phat$yhat))
(confusion <- table(pred.phat$lowrain, pred.phat$yhat))
(spec <- confusion[1, 1] / row.01[1])
(sens <- confusion[2, 2] / row.01[2])
(accu <- sum(diag(confusion)) / sum(confusion))
(prec <- confusion[2, 2] / col.01[2])

## Plot the ROC curves ####

# ROC-curves for all models####
roc.1 <- roc(lowrain ~ p.1, data = pred.phat)
roc.df.1 <- coords(roc.1, transpose = FALSE)
roc.df.1$model <- "1c"
roc.2 <- roc(lowrain ~ p.2, data = pred.phat)
roc.df.2 <- coords(roc.2, transpose = FALSE)
roc.df.2$model <- "1d"
roc.fw <- roc(lowrain ~ p.fw, data = pred.phat)
roc.df.fw <- coords(roc.fw, transpose = FALSE)
roc.df.fw$model <- "fw"
roc.bw <- roc(lowrain ~ p.bw, data = pred.phat)
roc.df.bw<- coords(roc.bw, transpose = FALSE)
roc.df.bw$model <- "bw"

roc.df <- rbind(roc.df.1, roc.df.2, roc.df.fw, roc.df.bw)

## Plot all the curves, in different colors####
(plt.rocs <-
    ggplot(roc.df, aes(specificity, sensitivity, color = model)) +
    geom_path(size = 1) +
    coord_fixed() +       # square plotting area
    scale_x_reverse() +   # Reverse scale on the x-axis!
    labs(title = "ROC-curves for all the models") +
    theme(text = element_text(size = 14))
)

ggsave(filename = "project2/plots/rocs.png", plot = plt.rocs)

##All models####
(aucs <- 
   data.frame(
     model = c( "1", "2", "fw", "bw"),
     auc = c(auc(roc.1), auc(roc.2), auc(roc.fw), auc(roc.bw)),
     lwr = c(ci(roc.1)[1], ci(roc.2)[1],
             ci(roc.fw)[1], ci(roc.bw)[1]),
     upr = c(ci(auc(roc.1))[3], ci(auc(roc.2))[3],
             ci(auc(roc.fw))[3], ci(auc(roc.bw))[3])))

## Compare AUC for the models####
roc.test(roc.1, roc.bw)
roc.test(roc.2, roc.bw)
roc.test(roc.fw, roc.bw)

## Find optimal parameter #### 

ggroc(roc.bw) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
  coord_fixed() +
  labs(title = "ROC-curve for model 3")

##Find best sens-spec####
### Experiment with different values####
# of levels for sens and spec, level.ss, to find
# the one that gives the optimal combination of sens and spec.
#level.ss <- 0.635
level.ss <- 0.25
roc.df.bw[roc.df.bw$sensitivity > level.ss & 
           roc.df.bw$specificity > level.ss, ]
##find the rownumber:
(I_max.bw <- which(roc.df.bw$sensitivity > level.ss & 
                    roc.df.bw$specificity > level.ss))
roc.df.bw[I_max.bw, ]
### Pick out the corresponding threshold for p####
roc.df.bw[I_max.bw, "threshold"]


pred.sort <- pred.phat[order(pred.phat$p.bw), ]
pred.sort$rank <- seq(1, nrow(pred.sort))
head(pred.sort)

# Divide the n=500 observations into g=10 groups:
n <- nrow(pred.sort)
g <- 10
# with ng = 50 observations each:
ng <- n/g

# Plot p_i and Y_i
# Add i vertical jitter to Y_i to separate them
ggplot(pred.sort, aes(rank, p.bw)) +
  geom_point() +
  geom_jitter(aes(y = lowrain), height = 0.01) +
  geom_vline(xintercept = seq(ng, nrow(pred.sort) - ng, ng)) +
  labs(title = "Model 3: Estimated probabilities by increasing size",
       caption = "g = 10 groups",
       x = "(i) = 1,...,n", y = "p-hat") +
  theme(text = element_text(size = 14))



## HL using hoslem.test####
###Model 3####
# p+1:
length(bwmodel$coefficients)
# so we need g > 3
g = 22
# while the smallest expected value is at least approx 5:
# Allowing 4 here and have experimented with g:
(HL.3 <- hoslem.test(pred.sort$lowrain, pred.sort$p.bw, g = g))
HL.3$expected
# All yhat0- and yhat1-values should be at least approx 5.

# Collect the data in a useful form for plotting:
(HL.df.3 <- data.frame(group = seq(1, g),
                       Obs0 = HL.3$observed[, 1],
                       Obs1 = HL.3$observed[, 2],
                       Exp0 = HL.3$expected[, 1],
                       Exp1 = HL.3$expected[, 2]))

ggplot(HL.df.3, aes(x = group)) +
  geom_line(aes(y = Obs0, linetype = "observed", color = "Y = 0"), size = 1) +
  geom_line(aes(y = Obs1, linetype = "observed", color = "Y = 1"), size = 1) +
  geom_line(aes(y = Exp0, linetype = "expected", color = "Y = 0"), size = 1) +
  geom_line(aes(y = Exp1, linetype = "expected", color = "Y = 1"), size = 1) +
  labs(title = "Model 3: Observed and expected in each group",
       y = "number of observations") +
  scale_x_continuous(breaks = seq(1, g)) +
  theme(text = element_text(size = 14))

