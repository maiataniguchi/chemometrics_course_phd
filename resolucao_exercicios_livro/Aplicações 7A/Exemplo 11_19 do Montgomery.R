#Tr?s componentes - polietileno (x1), poliestireno (x2) e polipropileno (x3) - foram combinados para
#formar fibras que ser?o usadas para fabricar fios para tecer cortinas. A resposta de interesse ? a
#elonga??o dos fios em quilogramas de for?a aplicada. Construiu-se um planejamento em rede simplex
#{3,2} e obtiveram-se os resultados da tabela abaixo.

# Fa?a o ajuste para o modelo quadr?tico e construa o gr?fico da superf?cie de resposta.
# Que condi??es maximizariam a elonga??o?
library("mixexp")

#SLD(q,k), q=n?mero de componentes (x1,x2,3), k=2
#SCD com ponto central
mix = SLD(3,2)
SLD(3,2)
DesignPoints(mix)
mix

# Priemiro cria-se o planejamento, depois se adicionam as respostas
# e depois roda as analises-inserir a resposta conforme a matriz:

data <- c(11.7,15.3,9.4,16.9,10.5,16.4) 
des1 <- cbind (mix,data)
des1
DesignPoints(mix)

mixvars <- c("x1", "x2", "x3")
resposta <- c("data")
model <- MixModel(frame=des1, resposta, mixvars, model=4) # 4 ? o modelo cubico especial, que ajusta com 7 pontos

ModelPlot(model=model,dimensions = list (x1="x1", x2="x2", x3="x3"), contour = TRUE,
          axislabs= c("Polietileno", "Poliestireno", "Polipropileno"), cornerlabs = c("A", "B", "C"), fill=TRUE)

## A melhor condi??o para maximizar a elonga??o ocorre aproximadamente quando A = 0.3; B = 0.05 e C = 0.65
# y = 11.7x1 + 9.4x2 + 16.4x3 + 19.0x2x1 + 11.4x3x1 -9.6x2x3
