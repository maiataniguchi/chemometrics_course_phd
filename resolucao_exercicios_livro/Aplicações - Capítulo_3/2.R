#3A.2 Voltametria cíclica do azul de metileno
library(FrF2)

fatorial1 <- FrF2(nruns = 8, nfactors=3, replications = 1,
                  randomize=FALSE, factor.names=list( C=c(-1,1),pH=c(-1,1),
                                                      S=c(-1,1)))
summary(fatorial1)
rever <- c(106,98,139,141,137,123,119,103)
fatorial2 <- add.response(fatorial1,rever)
summary(fatorial2)

modelo1 <- lm(rever ~ C*pH*S, data=fatorial2)
summary(modelo1)

MEPlot(fatorial2)
# Nesse gráfico é possível observar que a Sílica afeta muito pouco o valor da diferença de voltagem,
# Assim, podemos criar um novo modelo desconsiderando esse fator (S).

modelo2 <- lm(rever ~ C*pH, data=fatorial2)
summary(modelo2)

# A concentração e o pH afetam a diferença de voltagem. Como o objetivo é minimizar o ??E, os valores de
# concentração e pH devem ser trabalhadas no nível inferior (-), isto é, concentração de 0,1 mol-L-1 e pH 4.
