# Tabela 7.2
library("mixexp")

#SLD(q,k), q=número de componentes (x1,x2,3), k=2
#SCD com ponto central
mix = SLD(3)
SLD(3)
DesignPoints(mix)
mix

#priemiro cria o planejamneto, depois coloca as resposta e depois roda as analises-inserir a respsta conforme a matriz
data <- c(11.7,15.3,9.4,16.9,10.5,16.4) 
des1 <- cbind (mix,data)
des1
DesignPoints(mix)

mixvars <- c("x1", "x2", "x3")
resposta <- c("data")
model <- MixModel(frame=des1, resposta, mixvars, model=4) #4 é o modelo cubico especial, que ajusta com 7 pontos

ModelPlot(model=model,dimensions = list (x1="x1", x2="x2", x3="x3"), contour = TRUE,
          axislabs= c("A", "B", "C"), cornerlabs = c("A", "B", "C"), fill=TRUE)
