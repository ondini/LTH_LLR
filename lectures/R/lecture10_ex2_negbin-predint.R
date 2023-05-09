# Lecture 10, Negbin bootstrap prediction intervals####
# 8/5-23 Example 2: Calculates bootstrap prediction intervals for the absence data:

#Create the bootstrap function####
# by running the file containing the code:
source("R/boot_negbin.R")

# The data used to fit the model####
load("Data/nb_data.RData")

data.nb$prog <- factor(
  data.nb$prog, levels = c(1, 2, 3), 
  labels = c("General", "Academic", "Vocational"))
(model.nb <- glm.nb(daysabs ~ math + prog, data = data.nb))

# A data frame to predict on####

math.x0.nb <- seq(0, 100)
nb.predint <- data.frame(
  math = rep(math.x0.nb, 3),
  prog = c(rep("General", length(math.x0.nb)),
           rep("Academic", length(math.x0.nb)),
           rep("Vocational", length(math.x0.nb))))

# Use the boot.nb function on the example####
boot.predint.nb <- boot.nb(model.nb, data.nb, nb.predint, 5000, 0.95)

##Extract the prediction limits####
nb.predint$pred.lwr <- boot.predint.nb$lower
nb.predint$pred.upr <- boot.predint.nb$upper
head(nb.predint)

##Plot the intervals####
ggplot(nb.predint, aes(x = math, color = prog)) +
  geom_line(aes(y = pred.lwr)) +
  geom_line(aes(y = pred.upr)) +
  facet_wrap(~ prog)

#Jagged due to simulation of pointwise intervals.

# save(nb.predint, file = "Data/nb_predint.RData")
