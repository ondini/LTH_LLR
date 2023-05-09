#Lecture 10, GLM - Poisson####
# 8/5-23: Example 1 Poisson regression

library(ggplot2)

#Plot Poisson-distributions####
##Create probability function####
# dpois(k, m) = Pr(Y = k) when Y is Po(m)
Py <- data.frame(
  y = rep(seq(0, 20), 4),
  mu = c(rep(0.2, 21), rep(1, 21), rep(5, 21), rep(10, 21)),
  label = c(rep("m = 0.2", 21), rep("m = 1", 21),
            rep("m = 5", 21), rep("m = 10", 21)))
Py$prob <- dpois(Py$y, Py$mu)

##Plot density####
# geom_col() plots columns of the specified height
# position = "dodge" places the columns side-by-side
ggplot(Py, aes(y, prob, fill = label)) +
  geom_col(position = "dodge") +
  labs(title = "Some Poisson distributions",
       fill = "Mean",
       y = "probability") +
  theme(text = element_text(size = 18))

# Number of student awards####
##Data####
#read the data from a comma separated (csv) file on the net.
award <- read.csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")
head(award)
summary(award)

##Factor labels####
# give the programs their correct factor labels, not numbers
award$prog <- factor(
  award$prog, 
  levels = c(1, 2, 3), 
  labels = c("General", "Academic", "Vocational"))

## Frequency table of the number of awards####
(award.table <- table(award$num_awards))
## Save table as data frame and add proportions####
(award.df <- data.frame(
  num_awards = as.numeric(names(award.table)),
  award.table))
award.df$prob <- award.df$Freq / sum(award.df$Freq)
award.df$Var1 <- NULL
award.df$model <- "Observed"
award.df

##Average number of awards####
(mu <- mean(award$num_awards))
## and a variance estimate####
# s2 = sum(x_i - mu)^2/(n-1)
var(award$num_awards)

##Expected frequencies####
#according to a Poisson distribution with mean mu:
#New version of the table were the observed prob are
#replaced by the expected according to the Poisson.
#The model variable is used for colorcoding the plot:
award.df2 <- award.df
award.df2$prob <- dpois(award.df2$num_awards, mu)
award.df2$model <- "Poisson"
award.df2

#Add expected as rows:
award.df <- rbind(award.df, award.df2)
award.df

##Plot observed and expected####
ggplot(award.df, aes(num_awards, prob, fill = model)) +
  geom_col(position = "dodge") +
  labs(title = "Distribution of awards",
       x = "number of awards",
       y = "relative frequency") +
  theme(text = element_text(size = 18))

#Table by program####
table(award$prog, award$num_awards)
aggregate(num_awards ~ prog, data = award, FUN = mean)
aggregate(num_awards ~ prog, data = award, FUN = var)
# large variance in Academic. Need maths?
# variance larger than the mean. Might be explained by covariates?

#Poisson regression model####
(model.award <- glm(num_awards ~ prog + math, family = "poisson", data = award))
summary(model.award)
cbind(summary(model.award)$coefficients, ci = confint(model.award))
cbind(exp(model.award$coefficients), exp(confint(model.award)))

## Differences between programs####
beta <- model.award$coefficients
exp(beta[1] + beta[2])
exp(beta[1] + beta[3])
# compared to the general program and for fixed final math exam, 
# students from the academic program get about 3 times more awards.

# For a fixed study program, a unit increase in the students maths grade
# predicts a 7% increase (from 1 to 1.07) in the number of awards. Or, should 
# the students population get on average an increase of 10 points in the
# maths grade (for fixed program), we would have an increase of awards equal 
# to exp(10*0.07)=2, that is double the awards

##Estimated mu with confidence interval using xbeta####
pred.award <- cbind(
  award,
  muhat = predict(model.award, type = "response"),
  xb = predict(model.award, se.fit = TRUE))
pred.award$xb.residual.scale <- NULL

pred.award$xb.lwr <- pred.award$xb.fit - 1.96*pred.award$xb.se.fit
pred.award$xb.upr <- pred.award$xb.fit + 1.96*pred.award$xb.se.fit
pred.award$mu.lwr <- exp(pred.award$xb.lwr)
pred.award$mu.upr <- exp(pred.award$xb.upr)
head(pred.award)

##Plot data, mu and ci by program####
ggplot(pred.award, aes(math, num_awards, color = prog)) +
  geom_jitter(height = 0.1, width = 0) +
  geom_line(aes(y = muhat), linewidth = 1) +
  geom_ribbon(aes(ymin = mu.lwr, ymax = mu.upr), alpha = 0.1) +
  labs(title = "Expected number of awards",
       caption = "95% confidence interval",
       color = "program") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ prog)

#Deviance tests####
##Against null model####
summary(model.award)
(D_diff <- model.award$null.deviance - model.award$deviance)
(df_diff <- model.award$df.null - model.award$df.residual)
#quantile:
qchisq(1 - 0.05, df_diff)
#p-value:
pchisq(D_diff, df_diff, lower.tail = FALSE)

##Against removing prog####
#Update the model by removing prog:
model.math <- update(model.award, . ~ . - prog)
model.math$deviance - model.award$deviance
qchisq(1 - 0.05, model.math$df.residual - model.award$df.residual)

anova(model.math, model.award)
qchisq(1 - 0.05, 2)
pchisq(model.math$deviance - model.award$deviance,
       model.math$df.residual - model.award$df.residual,
       lower.tail = FALSE)

#Influence measures####
infl.award <- influence(model.award)
pred.award$v <- infl.award$hat
pred.award$devres <- infl.award$dev.res
pred.award$std.devres <- pred.award$devres/sqrt(1 - pred.award$v)
pred.award$D <- cooks.distance(model.award)

##Leverage####
ggplot(pred.award, aes(math, v, color = prog)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 2*(length(model.award$coefficients))/nrow(award), 
             color = "red") +
  labs(title = "Leverage",
       color = "program", caption = "horizontal line = 2(p+1)/n") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ prog)

##Standardised deviance residuals####
ggplot(pred.award, aes(math, std.devres, color = prog)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), linetype = "dotted", size = 1) +
  labs(title = "Standardized deviance residuals",
       color = "program") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ prog)

##Cook's D####
ggplot(pred.award, aes(math, D, color = prog)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 4/nrow(award), color = "red") +
  labs(title = "Cook's distance",
       color = "program", caption = "horizontal line = 4/n") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ prog)
  
#Prediction intervals using bootstrap####
# If you are interested: see lecture10_ex1_poisson-predint.R for
# generating the data and boot_poisson.R for the function.
load("Data/award_predint.RData")
head(award.predint)

ggplot(pred.award, aes(math, num_awards, color = prog)) +
  geom_point() +
  geom_line(aes(y = muhat)) +
  geom_ribbon(aes(ymin = mu.lwr, ymax = mu.upr), alpha = 0.1) +
  geom_line(data = award.predint, aes(y = pred.lwr), linewidth = 1) +
  geom_line(data = award.predint, aes(y = pred.upr), linewidth = 1) +
  labs(title = "Expected number of awards",
       caption = "95% conf.int. and 95% bootstrap pred.int.") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ prog)

# Read more at http://stats.idre.ucla.edu/r/dae/poisson-regression/
