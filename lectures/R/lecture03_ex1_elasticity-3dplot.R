# Lecture 3. Example 1. Elasticity
# 3D-plotting 27/3-23.  ONLY for those who REALLY want to know
# how I did the rotatable plots.
# Requires two extra packages: 
# library(plotly) for the 3D-plot and
# library(reshape2) for manipulating data to fit the plot command.

library(ggplot2)
library(plotly)
library(reshape2)

#create the data####
elasticity <- 
  data.frame(
    tension = c(152, 150, 103, 99, 88, 89, 122, 120, 
                162, 161),
    temp = c(180, 180, 190, 190, 200, 200, 210, 210, 
             220, 220),
    pressure = c(450, 450, 375, 375, 350, 350, 375, 
                 375, 450, 450)
  )

#p0 plot data####
p0 <- plot_ly(elasticity,
              x = ~temp,
              y = ~pressure,
              z = ~tension,
              type = "scatter3d",
              mode = "markers",
              marker = list(color = "black",
                            size = 4)) %>%
  layout(title = "y = tension vs x1 = temp and x2 = pressure")
p0

#fit models####
model.elast <- lm(tension ~ temp + pressure, data = elasticity)
model.inter <- lm(tension ~ temp*pressure, data = elasticity)
summary(model.elast)

#plot surface####
# Create a 10x10 grid to plot over:
(axis_x <- seq(min(elasticity$temp), 
               max(elasticity$temp), 
               length = 10))
(axis_y <- seq(min(elasticity$pressure), 
               max(elasticity$pressure), 
               length = 10))
#initiate the surface:
elast_surfs <- expand.grid(temp = axis_x,
                           pressure = axis_y,
                           KEEP.OUT.ATTRS = F)
#predict using the gridded temp-and-pressure:
elast_surfs <- 
  cbind(elast_surfs, 
        fit = predict(model.elast, elast_surfs),
        conf = predict(model.elast, 
                       newdata = elast_surfs,
                       interval = "confidence"),
        pred = predict(model.elast,
                       newdata = elast_surfs,
                       interval = "prediction"))
head(elast_surfs)
#delete unnecessary etimates:
elast_surfs$conf.fit <- elast_surfs$pred.fit <- NULL

# cast the data back to grids####
elast_lm_fit <- acast(elast_surfs, 
                      pressure ~ temp,
                      value.var = "fit")
elast_lm_conflwr <- acast(elast_surfs, 
                          pressure ~ temp,
                          value.var = "conf.lwr")
elast_lm_confupr <- acast(elast_surfs, 
                          pressure ~ temp,
                          value.var = "conf.upr")
elast_lm_predlwr <- acast(elast_surfs, 
                          pressure ~ temp,
                          value.var = "pred.lwr")
elast_lm_predupr <- acast(elast_surfs, 
                          pressure ~ temp,
                          value.var = "pred.upr")
#p1 plot plane####
color <- rep(0, length(elast_lm_fit))
dim(color) <- dim(elast_lm_fit)
color2 <- color + .5
color3 <- color2 + .5

p1 <- plot_ly(colors = c("green", "green"),
              showscale = FALSE) %>%
  add_trace(x = ~axis_x,
            y = ~axis_y,
            z = ~elast_lm_fit,
            type = "surface",
            opacity = 0.5,
            name = "fitted plane",
            surfacecolor = color,
            cauto = F,
            cmax = 1,
            cmin = 0)
p1 <- p1 %>%
  add_trace(data = elasticity,
            x = ~temp,
            y = ~pressure,
            z = ~tension,
            type = "scatter3d",
            mode = "markers",
            inherit = FALSE,
            marker = list(color = "black", 
                          size = 3)) %>%
  layout(title = "Fitted plane") %>%
  layout(
  scene = list(
    xaxis = list(title = 'temp'),
    yaxis = list(title = 'pressure'),
    zaxis = list(title = 'tension')
  )
)
p1

#p2 plot intervals####

p2 <- plot_ly(colors = c("green", "red"),
              showscale = FALSE) %>%
  add_trace(x = ~axis_x,
            y = ~axis_y,
            z = ~elast_lm_fit,
            type = "surface",
            opacity = 0.5,
            name = "fitted plane",
            surfacecolor = color,
            cauto = F,
            cmax = 1,
            cmin = 0) %>%
  add_trace(x = ~axis_x,
            y = ~axis_y,
            z = ~elast_lm_conflwr,
            type = "surface",
            opacity = 0.5,
            name = "confidence interval",
            surfacecolor = color2,
            cauto = F,
            cmax = 1,
            cmin = 0) %>%
  add_trace(x = ~axis_x,
            y = ~axis_y,
            z = ~elast_lm_confupr,
            type = "surface",
            name = "confidence interval",
            opacity = 0.5,
            surfacecolor = color2,
            cauto = F,
            cmax = 1,
            cmin = 0) %>%
  add_trace(x = ~axis_x,
            y = ~axis_y,
            z = ~elast_lm_predlwr,
            type = "surface",
            name = "prediction interval",
            opacity = 0.5,
            surfacecolor = color3,
            cauto = F,
            cmax = 1,
            cmin = 0) %>%
  add_trace(x = ~axis_x,
            y = ~axis_y,
            z = ~elast_lm_predupr,
            type = "surface",
            name = "prediction interval",
            opacity = 0.5,
            surfacecolor = color3,
            cauto = F,
            cmax = 1,
            cmin = 0) %>%
  add_trace(data = elasticity,
            x = ~temp,
            y = ~pressure,
            z = ~tension,
            type = "scatter3d",
            mode = "markers",
            inherit = FALSE,
            marker = list(color = "black", 
                          size = 3)) %>%
  layout(title = "Plane with intervals") %>%
  layout(
    scene = list(
      xaxis = list(title = 'temp'),
      yaxis = list(title = 'pressure'),
      zaxis = list(title = 'tension')
    )
  )
p2

#interaction####
# Separate set of surfaces for the interaction model
elast_inter <- expand.grid(temp = axis_x,
                           pressure = axis_y,
                           KEEP.OUT.ATTRS = F)
elast_inter <- 
  cbind(elast_inter, 
        fit = predict(model.inter, elast_inter),
        conf = predict(model.inter, 
                       newdata = elast_inter,
                       interval = "confidence"),
        pred = predict(model.inter,
                       newdata = elast_inter,
                       interval = "prediction"))
head(elast_inter)
elast_inter$conf.fit <- elast_inter$pred.fit <- NULL

# cast interaction####
elast_in_fit <- acast(elast_inter, 
                      pressure ~ temp,
                      value.var = "fit")
elast_in_conflwr <- acast(elast_inter, 
                          pressure ~ temp,
                          value.var = "conf.lwr")
elast_in_confupr <- acast(elast_inter, 
                          pressure ~ temp,
                          value.var = "conf.upr")
elast_in_predlwr <- acast(elast_inter, 
                          pressure ~ temp,
                          value.var = "pred.lwr")
elast_in_predupr <- acast(elast_inter, 
                          pressure ~ temp,
                          value.var = "pred.upr")
#p3 plot interaction####

p3 <- plot_ly(colors = c("green", "green"),
              showscale = FALSE) %>%
  add_trace(x = ~axis_x,
            y = ~axis_y,
            z = ~elast_in_fit,
            type = "surface",
            opacity = 0.5,
            name = "fitted plane",
            surfacecolor = color,
            cauto = F,
            cmax = 1,
            cmin = 0) %>%
  add_trace(data = elasticity,
            x = ~temp,
            y = ~pressure,
            z = ~tension,
            type = "scatter3d",
            mode = "markers",
            inherit = FALSE,
            marker = list(color = "black", 
                          size = 3)) %>%
  layout(title = "Plane with interaction") %>%
  layout(
    scene = list(
      xaxis = list(title = 'temp'),
      yaxis = list(title = 'pressure'),
      zaxis = list(title = 'tension')
    )
  )
p3

#p4 plot interaction intervals####

p4 <- plot_ly(colors = c("green", "red"),
              showscale = FALSE) %>%
  add_trace(x = ~axis_x,
            y = ~axis_y,
            z = ~elast_in_fit,
            type = "surface",
            opacity = 0.5,
            name = "fitted plane",
            surfacecolor = color,
            cauto = F,
            cmax = 1,
            cmin = 0) %>%
  add_trace(x = ~axis_x,
            y = ~axis_y,
            z = ~elast_in_conflwr,
            type = "surface",
            opacity = 0.5,
            name = "confidence interval",
            surfacecolor = color2,
            cauto = F,
            cmax = 1,
            cmin = 0) %>%
  add_trace(x = ~axis_x,
            y = ~axis_y,
            z = ~elast_in_confupr,
            type = "surface",
            name = "confidence interval",
            opacity = 0.5,
            surfacecolor = color2,
            cauto = F,
            cmax = 1,
            cmin = 0) %>%
  add_trace(x = ~axis_x,
            y = ~axis_y,
            z = ~elast_in_predlwr,
            type = "surface",
            name = "prediction interval",
            opacity = 0.5,
            surfacecolor = color3,
            cauto = F,
            cmax = 1,
            cmin = 0) %>%
  add_trace(x = ~axis_x,
            y = ~axis_y,
            z = ~elast_in_predupr,
            type = "surface",
            name = "prediction interval",
            opacity = 0.5,
            surfacecolor = color3,
            cauto = F,
            cmax = 1,
            cmin = 0) %>%
  add_trace(data = elasticity,
            x = ~temp,
            y = ~pressure,
            z = ~tension,
            type = "scatter3d",
            mode = "markers",
            inherit = FALSE,
            marker = list(color = "black", 
                          size = 3)) %>%
  layout(title = "Plane with interaction and intervals") %>%
  layout(
    scene = list(
      xaxis = list(title = 'temp'),
      yaxis = list(title = 'pressure'),
      zaxis = list(title = 'tension')))
p4
