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

#Como nós faremos apenas 16 experimento nesse planejamento, apenas 16 efeitos podem ser calculados.

ndiv <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
pdiv = (ndiv-0.5)/15
pdiv
z <- qnorm(pdiv,0,1)
z
efeitos <- c(-14.125,-8.625,-0.625,-0.625,-0.625,-0.125,0.375,0.375,0.375,
             0.875,0.875,0.875,0.875,8.875,22.875)
plot(efeitos,z)
MEPlot(fatorial2)

#Ao plotar o MEPlot é possivel notar que o fator DMA é bastante significativo e que o BQ e Cu não se mostram
#significativos para diminuir o tempo de gel.

IAPlot(fatorial2)
#O IAPlot não é interessante nesse caso, pois os fatores combinados de confundem em um planejamento fracionário.
IAPlot(fatorial2,select=c(1,2))
DanielPlot(fatorial2)

# Já o gráfico de Daniel permite observar que a DMA, Co e HQ são significativos para diminuir o tempo de gel.
# Pensando em termos práticos, o HQ, por exemplo, poderia ser utilizado num nível inferior ou, se possível,
# ser excluído do experimento prático.

# Agora o ajuste será feito utilizando apenas os fatores significativos:
modelo2 <- lm(tempo ~ HQ*Co*DMA, data=fatorial2)
summary(modelo2)
# Nesse caso, passamos a obter um planejamento 2^3  completo