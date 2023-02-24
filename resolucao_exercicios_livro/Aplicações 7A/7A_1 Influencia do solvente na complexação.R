# 7A.1 Influência do solvente na complexação do íon Fe(III)

library("mixexp")
#SLD(q,k), q=número de componentes (x1,x2,3), k=2
mix <- SLD(3,2) 
SLD(3,2)
mix

DesignPoints(mix)

#adicionar a resposta de acordo com a matriz
resposta <- c(0.411,0.612,0.614,0.451,0.693,0.456,0.461,0.608,0.521,0.607,0.468,0.520,0.533,0.412,0.528,0.682,
              0.412, 0.607,0.605,0.450,0.688,0.464,0.455,0.605,0.531,0.615,0.467,0.524,0.534,0.403,0.519,0.699)

des1 <- cbind(mix,resposta) 
des1
#para visualizar os pontos no grafico, so mostra o diagrama ternario
DesignPoints(mix) 

