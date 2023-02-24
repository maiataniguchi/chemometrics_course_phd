#3A.7 Controlando a espuma
# 2^2 em triplicata

library(FrF2)

fatorial1 <- FrF2(nruns = 8, nfactors=3, replications = 1,
                  randomize=FALSE, factor.names=list( A=c(-1,1),B=c(-1,1), C=c(-1,1)))
summary(fatorial1)

resp <- c(75.719,76.998,75.557,76.928,81.032,82.298,80.848,82.146)
fatorial2 <- add.response(fatorial1,resp)
summary(fatorial2)

#,35.00,34.60,35.40

DanielPlot(fatorial2)
MEPlot(fatorial2)
IAPlot(fatorial2)

modelo1 <- lm(resp ~ A*B*C, data=fatorial2)
summary(modelo1)

modelo2 <- lm(resp ~ A*C, data=fatorial2)
summary(modelo2)
