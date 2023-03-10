# 7A.2 Resist?ncia ? tra??o de misturas polim?ricas

library("mixexp")
#SLD(q,k), q=n?mero de componentes (x1,x2,3), k=2
# SLD = n?o tem ponto central
# SCD = tem ponto central (inclui automatico)
mix <- SCD(3)
SCD(3)
mix

DesignPoints(mix)

#adicionar a resposta de acordo com a matriz
resposta <- c(51.2,20.0,20.2,44.8,23.5,35.7,45.4)

des1 <- cbind(mix,resposta) 
des1
#para visualizar os pontos no grafico, so mostra o diagrama ternario
DesignPoints(mix)

mixvars <-c("x1","x2","x3")
resposta <- c("resposta")
model <- MixModel(frame = des1,resposta,mixvars, model = 4) # onde 4 significa modelo c?bico especial
# que ajusta com 7 pontos
model

ModelPlot(model = model,
          dimensions = list(x1="x1", x2="x2", x3="x3"),
          contour = TRUE, axislabs = c("PVDF","PMMA","PS"),
          cornerlabs = c("x1","x2","x3"), fill = TRUE)

# Modelo c?bico especial
# equa??o: y = 51,2x1 + 20,0 x2 + 20,2x3 + 36,8x1x2 - 48,8x1x3 + 62,4x2x3 + 252,0x1x2x3

# A resist?ncia ? m?xima em altos valores de x1 (blenda de polifluoreto de vinilidenila)
# e em baixos valores de x2 (blenda de polimetacrilato de metila) e x3 (poliestireno),
# visto que a regi?o de m?ximo encontra-se na parte superior do tri?ngulo pr?ximo ao ponto m?ximo de x1.

# Interpretanto o gr?fico de curva de contorno :
#x1 = PVDF = +- 0.80
#x2 = PMMA = +- 0.19
#x3 = PS = +- 0.01