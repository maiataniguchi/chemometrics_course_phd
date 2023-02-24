#3A.S Melhorando funções de onda

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

# Como a variável C não apresentou ser significativa tanto individualmente como interagindo com outros fatores,
#um novo modelo contendo somente A, B e D foi criado para obter-se o erro.

DanielPlot(fatorial2)
MEPlot(fatorial2)
IAPlot(fatorial2)
# O gráfico dos efeitos principais (Figura 2) mostrou que os fatores B e C, funções de polarizaçãoe funções
# difusas, respectivamente, não aparentam afetar significativamente a frequência do estiramento C-H.
# O gráfico normal, por sua vez, mostrou que os fatores A (Conjunto de base ) e D (C orrelação eletrônica)
# são significativos e, além disso, que a interação entre AxD e BxD são significativas também. 

modelo2 <- lm(freq ~ A*B*D, data=fatorial2)
summary(modelo2)

# Com o erro estimado podemos afirmar quais fatores e quais interações são significativas.
# Neste caso, observamos que os fatores A (Conjunto de base), D (Correlação eletrônica) e a interação entre
# BxD são significativos ao nível de 0.1%. Portanto, a melhor condição para obtenção de uma maior frequência
# do estiramento C-H ocorre quando o conjunto de base e a correlação eletrônica estão no nível inferior,
# isto é, 6-31G e Hartree-Fock, respectivamente.
# Para a interação BxD observamos pelo gráfico da Figura 3 que quando o fator B e D estão em seu nível
# inferior obtemos a maior frequência. Por isso, o fator B (Funções de polarização) deve ser ausente.

