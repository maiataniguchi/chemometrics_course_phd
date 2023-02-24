#Construindo um planejamento fracionario 2^5-1:

library(FrF2)
fatorial1 <- FrF2(nruns = 8, nfactors=4, replications = 1,
                  randomize=FALSE, factor.names=list( Temperatura=c(-1,1),Base=c(-1,1),
                                                      Solvente=c(-1,1), Catalisador=c(-1,1)))
#É importante lembrar que nesse caso teremos 16 experimentos para 5 fatores, onde os fatores são:
# HQ, BQ, Cu, Co e DMA
summary(fatorial1)
rend <- c(0,70,65,0,100,85,50,95)
fatorial2 <- add.response(fatorial1,rend)
summary(fatorial2)

# $`generators` : [1] D=ABC Mostra a estrutura de confundimento

MEPlot(fatorial2) # Gráfico dos efeitos principais
# O resultado mostrou que base e temp. sao poucos importante, mas nao é possivel tirar a conclusao ainda

DanielPlot(fatorial2) # O solvente e o catalisador realmente sao importantes

#Criando um modelo:

modelo1 <- lm(rend ~ Temperatura*Base*Solvente*Catalisador, data=fatorial2)
summary(modelo1)

# (Intercept) 58.125 = média
# Multiplicar os efeitos por dois (-1,1)
# Lembrando que as interações: Temperatura1:Base1; Temperatura1:Solvente1 e Base1:Solvente1 estão confundidas
# Quando temos: Temperatura*Base*Solvente*Catalisador o modelo linear inclui tudo e como temos 2 interações
# (Temperatura1:Solvente1 e Base1:Solvente1) nao sao significativos aparentemente nós eliminamos essa interação e 
# ajustamos o modelo incluindo apenas a interação que apresentou o melhor resultado (temp#base)

modelo3 <- lm(rend ~ Temperatura+Base+Solvente+Catalisador+Temperatura*Base, data=fatorial2)
summary(modelo3)

#Conclusão: Apenas solvente e catalisador sao importantes


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