#Tabela 7.1, modelo cúbico
library("mixexp")
#SLD, simplex-lattice design
#SLD(q,k), q=número de componentes, k=número de níveis de cada componente
#SCD(fac), fac=número de fatores
mix <- SCD(3)
SCD(3)
mix
data <- c(3.10,0.45,0.35,1.70,4.13,0.27,3.50)
des1 <- cbind(mix,data)
DesignPoints(mix)
des1
mixvars<-c("x1","x2","x3")
resposta<-c("data")
model <- MixModel(frame=des1,resposta,mixvars, model=4) #cúbico especial
model
ModelPlot(model=model, dimensions=list(x1="x1",x2="x2", x3="x3"),contour = TRUE)
#versão mais completa
ModelPlot(model=model, dimensions=list(x1="x1",x2="x2", x3="x3"),contour = TRUE, 
          axislabs = c("x1","x2","x3"), cornerlabs = c("A","B","C"))
#versão colorida
ModelPlot(model=model, dimensions=list(x1="x1",x2="x2", x3="x3"),contour = TRUE, 
          axislabs = c("x1","x2","x3"), cornerlabs = c("x1","x2","x3"), fill=TRUE)


