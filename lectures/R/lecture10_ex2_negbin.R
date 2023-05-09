# Lecture 10, GLM - Negative binomial####
# 8/5-23: Example 2: Negative binomial regression

library(ggplot2)

# Library for negative binomial. glm.nb()
# MASS = Modern Applied Statistics with S
# ("S" is the statistical software "R" is inspired by)
library(MASS)

# The Negbin example data is in Stata (another statistical 
# software) format. The package "foreign" has functions for
# reading, e.g., SAS, Stata, Minitab, Octave, SPSS formats.
# Use it if you like.
# If not, I have make an R version of the data.

# library(foreign)

#Plot Negative binomial distributions####
##Create probability functions####
Pnb <- data.frame(
  label = c(rep("theta = 50", 16), rep("theta = 5", 16),
            rep("theta = 0.5", 16), rep("theta = 0.05", 16)),
  y = rep(seq(0, 15, 1), 4),
  theta = c(rep(50, 16), rep(5, 16), rep(0.5, 16), rep(0.05, 16)))
Pnb$prob <- dnbinom(Pnb$y, mu = 5, size = Pnb$theta)
head(Pnb)

##Plot probability functions####

ggplot(Pnb, aes(y, prob, fill = label)) +
  geom_col(position = "dodge") +
  labs(title = "Some negative binomial distributions with mu = 5") +
  theme(text = element_text(size = 14))

#Example: Number of days absent####
## Read the data####
# This requires the foreign library:
# data.nb <- read.dta("https://stats.idre.ucla.edu/stat/stata/dae/nb_data.dta")

# or use the R-file from the course home page:
load("Data/nb_data.RData")
head(data.nb)
##Set factor labels####
data.nb$prog <- factor(
  data.nb$prog, levels = c(1, 2, 3), 
  labels = c("General", "Academic", "Vocational"))
# Also make the student id categorical
data.nb$id <- factor(data.nb$id)
summary(data.nb)
head(data.nb)

##Plot vs math by program####
ggplot(data.nb, aes(math, daysabs, color = prog)) +
  geom_point() +
  facet_wrap(~ prog)

# Fit a negbin model####
#requires MASS:
(model.nb <- glm.nb(daysabs ~ math + prog, data = data.nb))
(sum.nb <- summary(model.nb))
cbind(model.nb$coefficients, confint(model.nb))
exp(cbind(model.nb$coefficients, confint(model.nb)))

##Estimate means with confidence intervals####
pred.nb <- cbind(
  data.nb,
  mu = predict(model.nb, type = "response"),
  xb = predict(model.nb, se.fit = TRUE))
pred.nb$xb.residual.scale <- NULL

pred.nb$xb.lwr <- pred.nb$xb.fit - 1.96 * pred.nb$xb.se.fit
pred.nb$xb.upr <- pred.nb$xb.fit + 1.96 * pred.nb$xb.se.fit
pred.nb$mu.lwr <- exp(pred.nb$xb.lwr)
pred.nb$mu.upr <- exp(pred.nb$xb.upr)

##Add to plot####
ggplot(pred.nb, aes(math, daysabs, color = prog)) +
  geom_point() +
  geom_line(aes(y = mu), linewidth = 1) +
  geom_ribbon(aes(ymin = mu.lwr, ymax = mu.upr), alpha = 0.1) +
  facet_wrap(~ prog)

# Bootstrap prediction intervals####
# If interested, see Lecture10_ex1_negbin-predint.R for calculating
# these using the function in boot_negbin.R

load("Data/nb_predint.RData")

#Plotting predints with geom_smooth to avoid the raggedness.
ggplot(pred.nb, aes(math, daysabs, color = prog)) +
  geom_point() +
  geom_line(aes(y = mu)) +
  geom_ribbon(aes(ymin = mu.lwr, ymax = mu.upr), alpha = 0.1) +
  geom_smooth(data = nb.predint, aes(y = pred.lwr), size = 1, se = FALSE) +
  geom_smooth(data = nb.predint, aes(y = pred.upr), size = 1, se = FALSE) +
  labs(title = "Expected number of days absent",
       caption = "95% conf.int. and 95% bootstrap pred.int.") +
  theme(text = element_text(size = 18)) +
  facet_wrap(~ prog)

#Negbin or Poisson?####
##With standardized deviance residuals####
# for the two models.
pred.nb$devres <- influence(model.nb)$dev.res/sqrt(1 - influence(model.nb)$hat)

model.pois <- glm(daysabs ~ math + prog, family = "poisson", data = data.nb)
pred.pois <- cbind(
  data.nb,
  xb.fit = predict(model.pois),
  devres = influence(model.pois)$dev.res/sqrt(1 - influence(model.pois)$hat))

#checking the min and max to get equal y-axes
summary(pred.pois$devres)
summary(pred.nb$devres)

###Residual plot for Poisson####
ggplot(pred.pois, aes(x = xb.fit, color = prog)) +
  geom_point(aes(y = devres), size = 2) +
  geom_hline(yintercept = c(-3, -2, 0, 2, 3), 
             linetype = 2, size = 1) +
  expand_limits(y = c(-4.5, 7.5)) +
  labs(y = "std dev.res", x = "xb", color = "program",
       title = "Absence: Poisson model") +
  theme(text = element_text(size = 18))

###Residual plot for negbin####
ggplot(pred.nb, aes(x = xb.fit, color = prog)) +
  geom_point(aes(y = devres), size = 2) +
  geom_hline(yintercept = c(-3, -2, 0, 2, 3), 
             linetype = 2, size = 1) +
  expand_limits(y = c(-4.5, 7.5)) +
  labs(y = "std dev.res", x = "xb", color = "program",
       title = "Absence: Negbin model") +
  theme(text = element_text(size = 18))

#Poisson residuals are too large. Use negbin model.

##With Likelihood ratio test####
# Do NOT use "anova" as that tests two models following 
# the same distributions but this is not our case. 
# Hence I compute everything "by hand".

-2*logLik(model.pois)
-2*logLik(model.nb)

(D_diff <- -2*logLik(model.pois)[1] + 2*logLik(model.nb)[1])
qchisq(1 - 0.05, 1)
pchisq(D_diff, 1, lower.tail = FALSE)

## Poisson gives the same overall mu as Negative Binomial regression, but 
# the fit does not account for increasing variability for increasing mu.
## We need that theta.

# Are the awards negative binomial####
award <- read.csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")
award$prog <- factor(
  award$prog, 
  levels = c(1, 2, 3), 
  labels = c("General", "Academic", "Vocational"))
(model.award.pois <- glm(num_awards ~ prog + math, family = "poisson", data = award))
model.award.nb <- glm.nb(num_awards ~ prog + math, data = award)
model.award.nb
award.pred <- cbind(
  award,
  xb.pois = predict(model.award.pois),
  xb.nb = predict(model.award.nb),
  devres.pois = influence(model.award.pois)$dev.res/sqrt(1 - influence(model.award.pois)$hat),
  devres.nb = influence(model.award.nb)$dev.res/sqrt(1 - influence(model.award.nb)$hat))

summary(award.pred)
ggplot(award.pred, aes(x = xb.pois, color = prog)) +
  geom_point(aes(y = devres.pois), size = 2) +
  geom_hline(yintercept = c(-3, -2, 0, 2, 3), 
             linetype = 2, size = 1) +
  labs(y = "std dev.res", x = "xb", color = "program",
       title = "Awards: Poisson model") +
  theme(text = element_text(size = 18))

ggplot(award.pred, aes(x = xb.nb, color = prog)) +
  geom_point(aes(y = devres.nb), size = 2) +
  geom_hline(yintercept = c(-3, -2, 0, 2, 3), 
             linetype = 2, size = 1) +
  labs(y = "std dev.res", x = "xb", color = "program",
       title = "Awards: Negbin model") +
  theme(text = element_text(size = 18))

(D_diff.award <- -2*(logLik(model.award.pois)[1] - logLik(model.award.nb)[1]))
qchisq(1 - 0.05, 1)
pchisq(D_diff.award, 1, lower.tail = FALSE)

# No, the number of awards does not need a negative binomial, 
# poisson is ok.
