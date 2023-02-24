#Construindo um planejamento fracionario 2^5-1:

library(FrF2)
fatorial1 <- FrF2(nruns = 16, nfactors=5, replications = 1,
                  randomize=FALSE, factor.names=list( HQ=c(-1,1),BQ=c(-1,1),
                                                      Cu=c(-1,1), Co=c(-1,1), DMA=c(-1,1)))
#É importante lembrar que nesse caso teremos 16 experimentos para 5 fatores, onde os fatores são:
# HQ, BQ, Cu, Co e DMA
summary(fatorial1)
tempo <- c(14.02,29.42,26.07,17.58,25.18,17.03,15.24,33.54,18.30,12.17,10.57,22.20,10.19,23.52,21.14,13.10)
fatorial2 <- add.response(fatorial1,tempo)
summary(fatorial2)


modelo1 <- lm(tempo ~ HQ*BQ*Cu*Co*DMA, data=fatorial2)
summary(modelo1)

MEPlot(fatorial2)
DanielPlot(fatorial2) # DMA, Co e HQ são significativos

# Para obter o menor tempo de gel, podemos trabalhar com os fatores nos seguintes níveis:
# 1: Hidroquinona (HQ) 190 ppm
# 2: Benzoquinona (BQ) 20 ppm
# 3: Octanoato de cobre (Cu) 180 ppm 
# 4: Octanoato de cobalto (Co) 1800 ppm
# 5: Dimetilanilina (DMA) 540 ppm

# Para gerar o erro padrão, precisamos criar um novo modelo que tenha grau de liberdade para isso.
# Por esse motivo, iremos criar um modelo sem as variaveis BQ e Cu, visto que estas se apresentaram
# pouco significativas. O modelo é dado por:
modelo2 <- lm(tempo ~ HQ*Co*DMA, data=fatorial2)
summary(modelo2)
# Podemos observar pela tabela de ajuste que a HQ, Co e DMA são significativos ao nível de 0.1%
# Ou seja, temos cerca de 99.9% de probabilidade de que efeitos sejam provenientes de erros aleatórios.
