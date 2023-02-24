# Modelo cúbico
library("mixexp")

mix <- SCD(3)
SCD(3)
mix
DesignPoints(mix)

data <- c(3.10,0.45,0.35,1.70,4.13,0.27,3.50)
des1 <- cbind(mix,data)
des1

mixvars <-c("x1","x2","x3")
resposta <- c("data")
model <- MixModel(frame = des1,resposta,mixvars, model = 4) # onde 4 significa modelo cúbico especial
model

ModelPlot(model = model,
          dimensions = list(x1="x1", x2="x2", x3="x3"),
          contour = TRUE, axislabs = c("x1","x2","x3"),
          cornerlabs = c("x1","x2","x3"), fill = TRUE)
