#Construindo um planejamento fracionario 2^5-1:

library(FrF2)
fatorial1 <- FrF2(nruns = 8, nfactors=4, replications = 1,
                  randomize=FALSE, factor.names=list( Temperatura=c(-1,1),Base=c(-1,1),
                                                      Solvente=c(-1,1), Catalisador=c(-1,1)))
#? importante lembrar que nesse caso teremos 16 experimentos para 5 fatores, onde os fatores s?o:
# HQ, BQ, Cu, Co e DMA
summary(fatorial1)
rend <- c(0,70,65,0,100,85,50,95)
fatorial2 <- add.response(fatorial1,rend)
summary(fatorial2)

# $`generators` : [1] D=ABC Mostra a estrutura de confundimento

MEPlot(fatorial2) # Gr?fico dos efeitos principais
# O resultado mostrou que base e temp. sao poucos importante, mas nao ? possivel tirar a conclusao ainda

DanielPlot(fatorial2) # O solvente e o catalisador realmente sao importantes

#Criando um modelo:

modelo1 <- lm(rend ~ Temperatura*Base*Solvente*Catalisador, data=fatorial2)
summary(modelo1)

# (Intercept) 58.125 = m?dia
# Multiplicar os efeitos por dois (-1,1)
# Lembrando que as intera??es: Temperatura1:Base1; Temperatura1:Solvente1 e Base1:Solvente1 est?o confundidas
# Quando temos: Temperatura*Base*Solvente*Catalisador o modelo linear inclui tudo e como temos 2 intera??es
# (Temperatura1:Solvente1 e Base1:Solvente1) nao sao significativos aparentemente n?s eliminamos essa intera??o e 
# ajustamos o modelo incluindo apenas a intera??o que apresentou o melhor resultado (temp#base)

modelo3 <- lm(rend ~ Temperatura+Base+Solvente+Catalisador+Temperatura*Base, data=fatorial2)
summary(modelo3)

#Conclus?o: Apenas solvente e catalisador sao importantes


#Equivalente a um planejamento 2^4 completo:
modelo4<- lm(rend ~ Solvente*Catalisador, data=fatorial2)
summary(modelo4)


ndiv <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
pdiv = (ndiv-0.5)/15
pdiv
z <- qnorm(pdiv,0,1)
z
efeitos <- c(-14.125,-8.625,-0.625,-0.625,-0.625,-0.125,0.375,0.375,0.375,
             0.875,0.875,0.875,0.875,8.875,22.875)
plot(efeitos,z)
MEPlot(fatorial2)

#Ao plotar o MEPlot ? possivel notar que o fator DMA ? bastante significativo e que o BQ e Cu n?o se mostram
#significativos para diminuir o tempo de gel.

IAPlot(fatorial2)
#O IAPlot n?o ? interessante nesse caso, pois os fatores combinados de confundem em um planejamento fracion?rio.
IAPlot(fatorial2,select=c(1,2))
DanielPlot(fatorial2)

# J? o gr?fico de Daniel permite observar que a DMA, Co e HQ s?o significativos para diminuir o tempo de gel.
# Pensando em termos pr?ticos, o HQ, por exemplo, poderia ser utilizado num n?vel inferior ou, se poss?vel,
# ser exclu?do do experimento pr?tico.

# Agora o ajuste ser? feito utilizando apenas os fatores significativos:
modelo2 <- lm(tempo ~ HQ*Co*DMA, data=fatorial2)
summary(modelo2)
# Nesse caso, passamos a obter um planejamento 2^3  completo