library(FrF2)

#Criando um planejamento fatorial:

fatorial0 <- FrF2(4,2)
fatorial0

#onde 4 é o número de ensaios e 2 é o número de fatores. Em ordem aleatória

fatorial1 <- FrF2(nruns = 4, nfactors=2, replications = 2,
              randomize=FALSE, factor.names=list( T=c(-1,1),C=c(-1,1)))

fatorial1

#onde nruns = numero de ensaior; nfactors = número de fatores; reaplications = numero de replicação.
#randomize= desabilita a ordem aleatoria dos valores

#Incluindo os dados no planejamento:

rend <- c(57,92,55,66,61,88,53,70)

#Combinando a programação da ordem dos experimentos de fatorial1 com os rendimentos:

fatorial3 <- add.response(fatorial1,rend)

summary(fatorial3)

#add.response: combina os dados do planejamnto fatorial e rendimento.
#Criando um modelo linear de interação do rendimento com a temperatura e o catalisador:

modelo1 <- lm(rend ~ T*C, data=fatorial3)

summary(modelo1)
#Os valores resultantes serão /2.
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1






