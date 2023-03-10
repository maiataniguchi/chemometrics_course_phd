library(FrF2)

#nruns ? o num de ensaios m?nimos. Se quebrar a linha tem q executar uma por uma. Nosso objetivo ? fazer fatorial 2^3
fatorial1 <- FrF2(nruns= 16, nfactors = 4, replications = 1, randomize = FALSE,
                  factor.names = list( T=c(-1,1),C=c(-1,1),M=c(-1,1), pH=c(-1,1)))

summary(fatorial1)

rend <- c(54,85,49,62,64,94,56,70,52,87,49,64,64,94,58,73)

fatorial2 <- add.response(fatorial1,rend)

summary(fatorial2)
#vamos calcular os modelos agora, gerar a equa??o. lm lnear model ~fun??o da T,C,M, quero um ajuste linear que inclua at? 3 fatores

modelo1 <- lm(rend~T*C*M*pH,data=fatorial2)

summary(modelo1)
#estimativa dos efeitos e dos erros, que s?o varia??es por unidade de vari?vel codificada X. 
#os valores de p n?o aparecem pq n?o temos replicata.

DanielPlot(fatorial2)
