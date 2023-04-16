#Lecture 5. Example 1. 3/4-2023
# Simple linear regression: Atlantic cod
# Studentized residuals for Cod####

# Activate the ggplot-commands:
library(ggplot2)

load("Data/cod.RData")

#Linear model####
cod.linmod <- lm(weight ~ length, data = cod.data)
cod.linsum <- summary(cod.linmod)

##Fitted values, yhat####
cod.data$fit.lin <- predict(cod.linmod)

##Studentized residuals, r*####
cod.data$r.lin <- rstudent(cod.linmod)

##Plot r* vs yhat####
# and highlight |r*| > 3:
ggplot(data = cod.data, aes(x = fit.lin, y = r.lin)) +
  geom_point(size = 3) +
  geom_smooth(color = "red") +
  geom_hline(yintercept = c(-2, 0, 2)) +
  geom_hline(yintercept = c(-3, 3), linetype = 2) +
  geom_point(data = cod.data[abs(cod.data$r.lin) > 3, ], 
             color = "red", size = 4) +
  xlab("Predicted weight (g)") +
  ylab("Studentized residual") +
  labs(title = "Studentized residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 18))

##Plot sqrt(|r*|) vs yhat####
ggplot(data = cod.data, aes(x = fit.lin, y = sqrt(abs(r.lin)))) +
  geom_point(size = 3) +
  geom_hline(yintercept = c(sqrt(qnorm(0.75)), sqrt(2))) +
  geom_hline(yintercept = sqrt(3), linetype = 2) +
  expand_limits(y = 0) +
  xlab("Predicted weight (g)") +
  ylab("sqrt(|r*|)") +
  labs(title = "Constant variance?",
       caption = "0.82, sqrt(2), sqrt(3)") +
  theme(text = element_text(size = 18))

#Log-log model####
cod.logmod <- lm(log(weight) ~ log(length), data = cod.data)

##Fitted (log)values####
cod.data$fit.log <- predict(cod.logmod)

##Studentized residuals####
cod.data$r.log <- rstudent(cod.logmod)

##Plot r* vs yhat####
# and highlight |r*| > 3:
ggplot(data = cod.data, aes(x = fit.log, y = r.log)) +
  geom_point(size = 3) +
  geom_smooth(color = "red") +
  geom_hline(yintercept = c(-2, 0, 2)) +
  geom_hline(yintercept = c(-3, 3), linetype = 2) +
  geom_point(data = cod.data[abs(cod.data$r.log) > 3,], 
             color = "red", size = 4) +
  xlab("Predicted log-weight (ln g)") +
  ylab("Studentized residual") +
  labs(title = "Studentized residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 18))

##Plot sqrt(|r*|) vs yhat####
ggplot(data = cod.data, aes(x = fit.log, y = sqrt(abs(r.log)))) +
  geom_point(size = 3) +
  geom_hline(yintercept = c(sqrt(qnorm(0.75)), sqrt(2))) +
  geom_hline(yintercept = sqrt(3), linetype = 2) +
  expand_limits(y = 0) +
  xlab("Predicted log-weight (ln g)") +
  ylab("sqrt(|r*|)") +
  labs(title = "Constant variance?",
       caption = "0.82, sqrt(2), sqrt(3)") +
  theme(text = element_text(size = 18))
