library(FrF2)

#Criando um planejamento fatorial 2^4 (16 experimentos) de ordem aleatoria:
# 2 níveis (variação de cada fator,i.e., diferentes temperaturas)
# 4 fatores (neste caso serão a temperatura T; catalisador C; concentração M e pH)

fatorial1 <- FrF2(nruns = 16, nfactors=4, replications = 1,
                  randomize=FALSE, factor.names=list( T=c(-1,1),C=c(-1,1),
                                                      M=c(-1,1), pH=c(-1,1)))

summary(fatorial1)
rend <- c(54,85,49,62,64,94,56,70,52,87,49,64,64,94,58,73)

fatorial2 <- add.response(fatorial1,rend)
summary(fatorial2)
modelo1 <- lm(rend ~ T*C*M*pH, data=fatorial2)
summary(modelo1)

#Mostra no gráfico quem é significativo.
DanielPlot(fatorial2)

#Coefficients nao podem ser definidos pq nao temos replicatas :Error t value Pr(>|t|).

#Para os dados do fatorial:
ndiv <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)

pdiv = (ndiv-0.5)/15
pdiv

#Construindo os valores normais de Z
#qnorm(pdiv,media,desvpad)

z <- qnorm(pdiv,0,1)
z

efeitos <- c(-14.125,-8.625,-0.625,-0.625,-0.625,-0.125,0.375,0.375,0.375,
             0.875,0.875,0.875,0.875,8.875,22.875)

# Efeitos organizados do menos positivo para o mais positivo)
# Pq multiplicar por 2 os valores dos efeitos? Da equacao linear temos a variação de uma unidade, porém o efeito
# refere-se a das unidades (-1 ate +1)

plot(efeitos,z)
#Observando o grafico notamos que 4 pontos estão fora da reta, ou seja, são valores significativos.
#Sao eles: os dois valores mais positivos e os dois valores menos negativos
#Permite ver quem é significativo mesmo tendo um unico experimento sem repetição.


#Gráfico dos efeitos principais:
#Mostra o comportamento apenas dos efeitos princiapais
MEPlot(fatorial2)

#Gráfico que mostra quando se tem interação entre dois fatores:
IAPlot(fatorial2)
IAPlot(fatorial2,select=c(1,2))

DanielPlot(fatorial2)
