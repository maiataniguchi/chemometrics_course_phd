library(FrF2)

#Criando um planejamento fatorial 2^3 de ordem aleatoria:
# 2 níveis (variação de cada fator,i.e., diferentes temperaturas)
# 3 fatores (neste caso serão a temperatura T; catalisador C e concentração M)

fatorial1 <- FrF2(nruns = 8, nfactors=3, replications = 2,
                  randomize=FALSE, factor.names=list( T=c(-1,1),C=c(-1,1),
                                                      M=c(-1,1)))

summary(fatorial1)

#onde 8 é o número de ensaios e 2 é o número de fatores.
#onde nruns = numero de ensaior; nfactors = número de fatores; reaplications = numero de repetições.
#randomize= desabilita a ordem aleatoria dos valores factor.names=definição dos fatores

#Incluindo os dados de resposta no planejamento:

rend <- c(56,85,49,64,65,92,57,70,52,88,47,62,61,95,60,74)

#Combinando a programação da ordem dos experimentos de fatorial1 com os rendimentos:

fatorial2 <- add.response(fatorial1,rend)

summary(fatorial2)

#add.response: combina os dados do planejamnto fatorial e rendimento.
#Criando um modelo linear (lm) de interação do rendimento com a temperatura e o catalisador:
# ~ função da temperatura
# * multiplicação dos termos presentes da equação linear, i.e., os fatores que irão ser avaliados. 

#Calculando os efeitos:

modelo1 <- lm(rend ~ T*C*M, data=fatorial2)

summary(modelo1)

#Efeito corresponde a variação de duas unidades (-1 até +1)

#Pr(>|t|) probabilidade de que 
#Quanto menor o valor de Pr(>|t|) mais significativas serão as interações.

DanielPlot(fatorial2)
