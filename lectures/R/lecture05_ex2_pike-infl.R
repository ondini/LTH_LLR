# Lecture 5. Example 2. 3/4-2023
# Pike: Influential observations####

library(ggplot2)

load("Data/pike.rda")
summary(pike)

# 478 cm !?
head(pike)

# Plot the data####
ggplot(pike, aes(length, weight)) + geom_point()

##Select the strange long fish####
I.strange <- which(pike$length > 200)
pike[I.strange, ]

## Add it to the plot####
#marked in red:
# pike[I_strange, ] will select the row(s) with the 
# strange fish and all the columns.
ggplot(pike, aes(length, weight)) + 
  geom_point() +
  geom_point(data = pike[I.strange, ], 
             color = "red", size = 3, shape = 24) +
  labs(title = "Pike in Halland: weight by length",
       caption = "including one 4.8m(?!) long weighing 0.7kg(!?)") +
  theme(text = element_text(size = 18))

#log-log-scale####
## Fit and predict####
model.pike <- lm(log(weight) ~ log(length), pike)
pike.pred <- cbind(
  pike, 
  fit = predict(model.pike),
  r = rstudent(model.pike))

##Plot data in log-log####
(pike.plot <- ggplot(pike.pred, aes(log(length), log(weight))) +
    geom_point(size = 3) +
    geom_point(data = pike[I.strange, ], 
               color = "red", size = 4, shape = 24) +
    geom_line(aes(y = fit), linewidth = 1, color = "red", linetype = "dashed") +
    labs(title = "Pike: fitted line including strange observation") +
    theme(text = element_text(size = 18))
)

## Plot r* vs yhat####
ggplot(pike.pred, aes(x = fit, y = r)) +
  geom_point(size = 3) +
  geom_hline(yintercept = c(-2, 0, 2)) +
  geom_hline(yintercept = c(-3, 3), linetype = 2) +
  geom_smooth() +
  geom_point(data = pike.pred[I.strange, ], 
             color = "red", size = 4, shape = 24) +
  labs(title = "Pike: residuals vs fitted values") +
  xlab("fitted values (log weight)") +
  ylab("studentized residuals") +
  theme(text = element_text(size = 18))

##Plot sqrt(|r*|) vs yhat####
ggplot(pike.pred, aes(x = fit, y = sqrt(abs(r)))) +
  geom_point(size = 3) +
  geom_hline(yintercept = c(sqrt(qnorm(0.75)), sqrt(2))) +
  geom_hline(yintercept = sqrt(3), linetype = 2) +
  geom_point(data = pike.pred[I.strange, ], 
             color = "red", size = 4, shape = 24) +
  labs(title = "Pike: constant variance?") +
  xlab("fitted values (log weight)") +
  ylab("sqrt(|r*|)") +
  theme(text = element_text(size = 18))

#Influential observations####
## leverage####
pike.pred$v <- influence(model.pike)$hat
head(pike.pred)
pike.pred[I.strange, ]

# with 1/n and 2(p+1)/n horizontal lines:
# p+1 = 
length(model.pike$coefficients)

ggplot(cbind(pike.pred), aes(x = log(length), y = v)) +
  geom_point(size = 3) +
  geom_point(data = pike.pred[I.strange, ], 
             color = "red", size = 4, shape = 24) +
  geom_hline(yintercept = 1/nrow(pike)) +
  geom_hline(yintercept = 2*length(model.pike$coefficients)/nrow(pike), 
             color = "red") +
  labs(title = "Pike: leverage vs log length") +
  labs(caption = "y = 1/n (black) and 2(p+1)/n (red)") +
  theme(text = element_text(size = 18))

## Cook's D####
pike.pred$D <- cooks.distance(model.pike)
head(pike.pred)

## Plot D vs yhat####
(f1.pike <- length(model.pike$coefficients))
(f2.pike <- model.pike$df.residual)
(cook.limit.pike <- qf(0.5, f1.pike, f2.pike))
ggplot(pike.pred, aes(fit, D)) + 
  geom_point(size = 3) +
  geom_point(data = pike.pred[I.strange, ], 
             color = "red", size = 4, shape = 24) +
  geom_hline(yintercept = cook.limit.pike, color = "red") +
  geom_hline(yintercept = 4/nrow(pike.pred), linetype = 2, color = "red") +
  xlab("Fitted values") +
  ylab("D_i") +
  labs(title = "Pike: Cook's D") +
  labs(caption = "4/n (dashed), F_0.5, p+1, n-(p+1) (solid)") +
  theme(text = element_text(size = 18))

# Exclude the strange fish####
## re-fit the model####
pike.exc <- pike[-I.strange, ]
model.exc <- lm(log(weight) ~ log(length), data = pike.exc)

##add predictions to whole data set####
pike.pred$fit.exc <- predict(model.exc, newdata = pike)

## add new line to pike.plot####
# data = pike.pred necessary since fit.exc did not
# exist in pike.pred when we made the original plot, pike.plot.
pike.plot + 
  geom_line(data = pike.pred, aes(y = fit.exc), color = "blue", linewidth = 1) +
  labs(title = "Pike: fitted line excluding strange observation") +
  labs(caption = "Fitted line with (red dashed) and without (blue solid) problematic fish")

#Residual analysis####
pike.exc.pred <- cbind(
  pike.exc,
  fit = predict(model.exc),
  r = rstudent(model.exc),
  D = cooks.distance(model.exc))

##Find large r*####
(I.r.large <- which(abs(pike.exc.pred$r) > 3))

##Plot r* vs yhat####
ggplot(pike.exc.pred, aes(x = fit, y = r)) +
  geom_point(size = 3) +
  #highlight |r|>3 in red:
  geom_point(data = pike.exc.pred[I.r.large, ], 
             color = "red", size = 4) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2)) +
  geom_hline(yintercept = c(-3, 3), linetype = "dashed") +
  geom_smooth() +
  xlab("Fitted values") +
  ylab("r*") +
  labs(title = "Pike: studentized residuals vs fitted values",
       subtitle = "without the strange fish") +
  labs(caption = "y = +/- 2 and +/- 3") +
  theme(text = element_text(size = 18))

##Plot sqrt(|r*|) vs yhat####
ggplot(pike.exc.pred, aes(x = fit, y = sqrt(abs(r)))) +
  geom_point(size = 3) +
  geom_hline(yintercept = c(sqrt(qnorm(0.75)), sqrt(2))) +
  geom_hline(yintercept = sqrt(3), linetype = 2) +
  xlab("Fitted values") +
  ylab("sqrt(|r*|)") +
  labs(title = "Pike: studentized residuals vs fitted values",
       subtitle = "without the strange fish") +
  labs(caption = "y = 0.82, sqrt(2), sqrt(3)") +
  theme(text = element_text(size = 18))

#Cook's distance####
(f1.exc <- length(model.exc$coefficients))
(f2.exc <- model.exc$df.residual)
(cook.limit.exc <- qf(0.5, f1.exc, f2.exc))

##Find large D####
(I.D.large <- which(pike.exc.pred$D > 0.1))
pike.exc.pred[I.D.large, ]
# obs. 214 and 269 have both large residual and large D

##Plot D vs yhat####
ggplot(pike.exc.pred, aes(fit, D)) + 
  geom_point(size = 3) +
  geom_point(data = pike.exc.pred[I.r.large, ], 
             color = "red", size = 3) +
  geom_point(data = pike.exc.pred[I.D.large, ], 
             color = "blue", shape = 24, size = 3) +
  geom_hline(yintercept = cook.limit.exc, color = "red") +
  geom_hline(yintercept = 4/nrow(pike.exc.pred), linetype = 2, color = "red") +
  xlab("Fitted values") +
  ylab("D_i") +
  labs(title = "Pike: Cook's D",
       subtitle = "without the strange fish") +
  labs(caption = "4/n (dashed), F_0.5, p+1, n-(p+1) (solid)") +
  theme(text = element_text(size = 18))

# DFBETAS####
#Note: dfbetas, not dfbeta. The S stands for "standardized")
head(dfbetas(model.exc))
pike.exc.pred$df0 <- dfbetas(model.exc)[, "(Intercept)"]
pike.exc.pred$df1 <- dfbetas(model.exc)[, "log(length)"]

##Plot dfbetas0 vs yhat####
ggplot(pike.exc.pred, aes(x = fit, y = df0)) +
  geom_point(size = 2) +
  geom_point(data = pike.exc.pred[I.r.large, ], 
             color = "red", size = 3) +
  geom_point(data = pike.exc.pred[I.D.large, ], 
             shape = 24, size = 3) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = sqrt(cook.limit.exc)*c(-1, 1), color = "red") +
  geom_hline(yintercept = 2/sqrt(nrow(pike.exc))*c(-1, 1), color = "red", linetype = "dashed") +
  ylab("DFBETAS_0(i)") +
  xlab("Fitted values") +
  labs(title = "Pike: DFBETAS_0: impact on the intercept",
       subtitle = "without the strange fish") +
  labs(caption = "y = sqrt(F_0.5) and 2/sqrt(n)") +
  theme(text = element_text(size = 18))

##Plot dfbetas1 vs yhat####
ggplot(pike.exc.pred, aes(x = fit, y = df1)) +
  geom_point(size = 3) +
  geom_point(data = pike.exc.pred[I.r.large, ], 
             color = "red", size = 3) +
  geom_point(data = pike.exc.pred[I.D.large, ], 
             shape = 24, size = 3) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = sqrt(cook.limit.exc)*c(-1, 1), color = "red") +
  geom_hline(yintercept = 2/sqrt(nrow(pike.exc))*c(-1, 1), color = "red", linetype = "dashed") +
  xlab("Fitted values") +
  ylab("DFBETAS_1(i)") +
  labs(title = "Pike: DFBETAS_1: impact on the slope",
       subtitle = "without the strange fish") +
  labs(caption = "y = sqrt(F_0.5) and 2/sqrt(n)") +
  theme(text = element_text(size = 18))
