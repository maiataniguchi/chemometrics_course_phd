#Tabela 7.1
library("mixexp")
#SLD, simplex-lattice design
#SLD(q,k), q=número de componentes, k=número de níveis de cada componente
#SCD(fac), fac=número de fatores
mix <- SLD(3, 2)
SLD(3, 2)
mix
data <- c(3.10,1.70,0.45,4.13,0.27,0.35)
des1 <- cbind(mix,data)
des1
DesignPoints(mix)
mixvars<-c("x1","x2","x3")
resposta<-c("data")
model <- MixModel(frame=des1,resposta,mixvars, model=2)
model
ModelPlot(model=model, dimensions=list(x1="x1",x2="x2", x3="x3"),contour = TRUE)
#versão mais completa
ModelPlot(model=model, dimensions=list(x1="x1",x2="x2", x3="x3"),contour = TRUE, axislabs =
            c("x1","x2","x3"), cornerlabs = c("x1","x2","x3"))
#versão colorida
ModelPlot(model=model, dimensions=list(x1="x1",x2="x2", x3="x3"),contour = TRUE, axislabs =
            c("x1","x2","x3"), cornerlabs = c("x1","x2","x3"), fill=TRUE)

