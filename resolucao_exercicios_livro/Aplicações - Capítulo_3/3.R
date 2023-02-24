# 3A.3 Tempo de retenção em cromatografia líquida

library(FrF2)

fatorial1 <- FrF2(nruns = 8, nfactors=3, replications = 1,
                  randomize=FALSE, factor.names=list( T=c(-1,1),EtOH=c(-1,1),
                                                      Flux=c(-1,1)))
summary(fatorial1)

tempo <- c(49.26,31.27,42.20,26.61,23.81,15.07,19.57,12.86)
fatorial2 <- add.response(fatorial1,tempo)

summary(fatorial2)

modelo1 <- lm(tempo ~ T*EtOH*Flux, data=fatorial2)

summary(modelo1)

DanielPlot(fatorial2) #O fluxo e a temperatura mostrarma-se significativos
MEPlot(fatorial2) # O etanol parece ser pouco significativo quando comparado com os demais fatores
IAPlot(fatorial2)

# Modelo linear excluindo o fator Etanol:

modelo2 <- lm(tempo ~ T*Flux, data=fatorial2)

summary(modelo2)

# A partir dos gráficos é possível notar que a porcentagem de Etanol não é um fator tão significativo
# quando comparado com a temperatura e o fluxo do processo. Outro ponto a se citar é que os efeitos para
# todos os fatores geram uma resposta negativa, ou seja, quando partimos da condição inferior para a superior
# observamos uma diminuição no tempo de retenção. Como deseja-se obter o menor tempo de retenção possível,
# a melhor codição ocorre quando  T= 50 °C; C=0,2 mol.L-1 e EtOH = 70%.