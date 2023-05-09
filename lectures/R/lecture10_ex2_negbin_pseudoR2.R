# Extra R-code for Lecture 10, VT-2023:
# Deviance based pseudo R2 for negative binomial.
# Note that D0 changes for each model, since the theta
# estimate is retained when the x-variables are removed.

library(MASS)

load("Data/nb_data.RData")

# Set the progam category names:
data.nb$prog <- factor(
  data.nb$prog, levels = c(1, 2, 3), 
  labels = c("General", "Academic", "Vocational"))
# Also make the student id categorical
data.nb$id <- factor(data.nb$id)

#re-fit the model:
(model.nb <- glm.nb(daysabs ~ math + prog, data = data.nb))
(sum.nb <- summary(model.nb))

#Create estimates in some other models to compare with.
# Two program categories: Vocational/not vocational:
data.nb$vocational <- factor(data.nb$prog == "Vocational", 
                         levels = c(FALSE, TRUE),
                         labels = c("Gen/Aca", "Vocational"))
model.3 <- glm.nb(daysabs ~ prog, data = data.nb)
model.2 <- glm.nb(daysabs ~ vocational + math, data = data.nb)
model.1 <- glm.nb(daysabs ~ math, data = data.nb)
model.0 <- glm.nb(daysabs ~ 1, data = data.nb)

pseudo.R2 <- data.frame(
  model = c("0:Null", "1:math", "2:voc+math", "3:prog", "nb:prog+math"),
  D0 = c(model.0$null.deviance, 
         model.1$null.deviance,
         model.2$null.deviance,
         model.3$null.deviance,
         model.nb$null.deviance),
  D = c(model.0$deviance, 
        model.1$deviance,
        model.2$deviance,
        model.3$deviance,
        model.nb$deviance),
  p = c(model.0$df.null - model.0$df.residual,
        model.1$df.null - model.1$df.residual,
        model.2$df.null - model.2$df.residual,
        model.3$df.null - model.3$df.residual,
        model.nb$df.null - model.nb$df.residual))
pseudo.R2$R2 <- round(100*(1 - pseudo.R2$D/pseudo.R2$D0), digits = 1)
pseudo.R2$R2.adj <- round(100*(1 - (pseudo.R2$D + pseudo.R2$p)/pseudo.R2$D0), digits = 1)

pseudo.R2
