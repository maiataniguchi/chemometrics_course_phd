#3A.S Melhorando fun??es de onda

# Planejamento 2^4

library(FrF2)

fatorial1 <- FrF2(nruns = 16, nfactors=4, replications = 1,
                  randomize=FALSE, factor.names=list( A=c(-1,1),B=c(-1,1),C=c(-1,1), D=c(-1,1)))
summary(fatorial1)

freq <- c(3245.6,3212.4,3203.5,3190.3,3251.7,3209.4,3214.9,3193.5,3096.2,3049.3,3132.8,3087.6,3105.0,3050.4,3143.5,3093.5)
fatorial2 <- add.response(fatorial1,freq)
summary(fatorial2)

modelo1 <- lm(freq ~ A*B*C*D, data=fatorial2)
summary(modelo1)

# Como a vari?vel C n?o apresentou ser significativa tanto individualmente como interagindo com outros fatores,
#um novo modelo contendo somente A, B e D foi criado para obter-se o erro.

DanielPlot(fatorial2)
MEPlot(fatorial2)
IAPlot(fatorial2)
# O gr?fico dos efeitos principais (Figura 2) mostrou que os fatores B e C, fun??es de polariza??oe fun??es
# difusas, respectivamente, n?o aparentam afetar significativamente a frequ?ncia do estiramento C-H.
# O gr?fico normal, por sua vez, mostrou que os fatores A (Conjunto de base ) e D (C orrela??o eletr?nica)
# s?o significativos e, al?m disso, que a intera??o entre AxD e BxD s?o significativas tamb?m. 

modelo2 <- lm(freq ~ A*B*D, data=fatorial2)
summary(modelo2)

# Com o erro estimado podemos afirmar quais fatores e quais intera??es s?o significativas.
# Neste caso, observamos que os fatores A (Conjunto de base), D (Correla??o eletr?nica) e a intera??o entre
# BxD s?o significativos ao n?vel de 0.1%. Portanto, a melhor condi??o para obten??o de uma maior frequ?ncia
# do estiramento C-H ocorre quando o conjunto de base e a correla??o eletr?nica est?o no n?vel inferior,
# isto ?, 6-31G e Hartree-Fock, respectivamente.
# Para a intera??o BxD observamos pelo gr?fico da Figura 3 que quando o fator B e D est?o em seu n?vel
# inferior obtemos a maior frequ?ncia. Por isso, o fator B (Fun??es de polariza??o) deve ser ausente.

