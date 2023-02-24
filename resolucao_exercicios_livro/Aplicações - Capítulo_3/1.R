#3A.l Hidrólise de resinas
# Planejamento 2^3 em duplicata para estudar o efeito de três fatores
# (tempo e temperatura de hidrólise, e tipo de catalisador). Seu objetivo era maximizar a resposta.

library(FrF2)

fatorial1 <- FrF2(nruns = 8, nfactors=3, replications = 2,
                  randomize=FALSE, factor.names=list( T=c(-1,1),H=c(-1,1),
                                                      C=c(-1,1)))
summary(fatorial1)

subs <- c(0.52,0.57,0.55,0.58,0.47,0.53,0.52,0.54,0.54,0.58,0.54,0.56,0.45,0.56,0.53,0.52)
fatorial2 <- add.response(fatorial1,subs)

summary(fatorial2)

modelo1 <- lm(subs ~ T*H*C, data=fatorial2)

summary(modelo1)

DanielPlot(fatorial2)
IAPlot(fatorial2)
MEPlot(fatorial2)

# Ao executar o planejamento fatorial  23 podemos notar pelo gráfico de efeitos principais que o tempo e o
# tipo de catalisador influenciam de maneira mais significativa que a temperatura da hidrólise.
# Essa observação pode ser corroborada ao verificar os valores dos efeitos apresentados na Tabela, onde os
# efeitos obtidos para o tempo e o catalisador foram de 0,40 e -0,40, respectivamente, enquanto que o efeito
# da temperatura é de apenas 0,015. Como o objetivo do V. X. de Oliveira Jr. era maximizar a resposta,
# a melhor condição para a realização do experimento ocorre quando o tempo de hidrólise é de 48 horas (+),
# a temperatura de 160 °C (+)  e o catalisador TFA (-).
