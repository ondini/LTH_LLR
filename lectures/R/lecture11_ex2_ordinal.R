# Lecture 11, Example 2: Ordinal regression####
# 10/5-23: Likeliness to apply

library(ggplot2)
# Necessary for ordinal regression:
library(MASS)

#Data####
load("Data/ologit.rda")
head(ologit)
summary(ologit)

##Factors####
# apply: how likely are you to apply to graduate school:
# pared: at least one parent has a graduate degree
# public: the undergraduate institution is public
# gpa: student's grade point average
ologit$pared <- factor(ologit$pared, levels = c(0, 1),
                       labels = c("no", "yes"))
ologit$public <- factor(ologit$public, levels = c(0, 1),
                       labels = c("private", "public"))

#Some models####
##Null model####
(model.null <- polr(apply ~ 1, data = ologit))
(sum.null <- summary(model.null))

##Full model####
(model.full <- polr(apply ~ pared + public + gpa, data = ologit))
(sum.full <- summary(model.full))

##Stepwise model####
(model.final <- step(model.full))
(sum.final <- summary(model.final))

#Parameter estimates####
##beta-estimates####
cbind(beta = model.final$coefficients, 
      expbeta = exp(model.final$coefficients),
      exp(confint(model.final)))
##zeta-extimates####
cbind(zeta = model.final$zeta, 
      expzeta = exp(model.final$zeta))

## beta and zeta with s.e.####
sum.final$coefficients

#Odds####
## Odds for unlikely, all x=0####
exp(model.final$zeta[1])
## Odds for unlikely, pared = 1####
exp(model.final$zeta[1])/exp(model.final$coefficients[1])
## Odds for unlikely, pared = 1 and gpa = 3####
exp(model.final$zeta[1])/exp(model.final$coefficients[1] + 3*model.final$coefficients[2])

#Estimate props and categories####
##estimated probabilities####
predict(model.final, type = "prob")
##predicted category####
predict(model.final)
predict(model.final, type = "class")

#Predict and ...####
x0 <- data.frame(gpa = rep(seq(2, 4, 0.1), 2),
                 pared = c(rep("no", length(seq(2, 4, 0.1))),
                           rep("yes", length(seq(2, 4, 0.1)))))
pred.final <- cbind(
  x0,
  predict(model.final, newdata = x0, type = "prob"),
  yhat = predict(model.final, newdata = x0))

##plot probs####
ggplot(pred.final, aes(x = gpa)) +
  geom_line(aes(y = unlikely, color = "1.unlikely"), linewidth = 2) +
  geom_line(aes(y = `somewhat likely`, color = "2.somewhat likely"), linewidth = 2) +
  geom_line(aes(y = `very likely`, color = "3.very likely"), linewidth = 2) +
  labs(color = "apply", title = "Graduate school?") +
  facet_wrap(~ pared, labeller = "label_both") +
  theme(text = element_text(size = 14))

##plot stacked probs####
ggplot(pred.final, aes(x = gpa)) +
  geom_ribbon(aes(ymin = 0, ymax = unlikely, fill = "1.unlikely")) +
  geom_ribbon(aes(ymin = unlikely, 
                  ymax = unlikely + `somewhat likely`, 
                  fill = "2.somewhat likely")) +
  geom_ribbon(aes(ymin = unlikely + `somewhat likely`, ymax = 1,
                  fill = "3.very likely")) +
  labs(fill = "apply", title = "Graduate school?") +
  facet_wrap(~ pared, labeller = "label_both") +
  theme(text = element_text(size = 14))

#AIC, BIC, R2####
## deviance####
model.final$deviance
## total number of parameters (beta and zeta)####
model.final$edf
##collect measures####
info <- cbind(aic = AIC(model.null, model.final, model.full),
              bic = BIC(model.null, model.final, model.full),
              R2D = 100*c(1 - model.null$deviance/model.null$deviance, 
                          1 - model.final$deviance/model.null$deviance, 
                          1 - model.full$deviance/model.null$deviance),
              R2D.adj = 100*c(1 - (model.null$deviance + model.null$edf - model.null$edf)/
                                model.null$deviance, 
                              1 - (model.final$deviance + model.final$edf - model.null$edf)/
                                model.null$deviance, 
                              1 - (model.full$deviance + model.full$edf - model.null$edf)/
                                model.null$deviance))
round(info, digits = 1)

# LR-test comparing nested models####
anova(model.null, model.final)
anova(model.final, model.full)

#Goodness-of-fit####
##Confusion matrix####

pred.final <- cbind(ologit,
                    yhat = predict(model.final))
(conf.matrix <- table(pred.final$apply, pred.final$yhat))
table(pred.final$apply)
table(pred.final$yhat)
sum(conf.matrix)

##Sensitivity etc####
(sens <- 100*(diag(conf.matrix)/table(pred.final$apply)))
(prec <- 100*(diag(conf.matrix)/table(pred.final$yhat)))
(acc <- 100*sum(diag(conf.matrix)/sum(conf.matrix)))
