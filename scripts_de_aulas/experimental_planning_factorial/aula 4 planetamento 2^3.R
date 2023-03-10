library(FrF2)

#nruns ? o num de ensaios m?nimos. Se quebrar a linha tem q executar uma por uma. Nosso objetivo ? fazer fatorial 2^3
fatorial1 <- FrF2(nruns= 8, nfactors = 3, replications = 2, randomize = FALSE,
                  factor.names = list( T=c(-1,1),C=c(-1,1),M=c(-1,1)))

summary(fatorial1)

rend <- c(56,85,49,64,65,92,57,70,52,88,47,62,61,95,60,74)

fatorial2 <- add.response(fatorial1,rend)

summary(fatorial2)
#vamos calcular os modelos agora, gerar a equa??o. lm lnear model ~fun??o da T,C,M, quero um ajuste linear que inclua at? 3 fatores

modelo1 <- lm(rend~T*C*M,data=fatorial2)

summary(modelo1)
#estimativa dos efeitos e dos erros, que s?o varia??es por unidade de vari?vel codificada X. 