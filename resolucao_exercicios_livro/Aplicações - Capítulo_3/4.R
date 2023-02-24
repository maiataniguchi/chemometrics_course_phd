#3A.4 Separação de gases por adsorção para avaliar a Produtividade do adsorvente (P)
# Planejamento 2^4

library(FrF2)

fatorial1 <- FrF2(nruns = 16, nfactors=4, replications = 1,
                  randomize=FALSE, factor.names=list( A=c(-1,1),B=c(-1,1),C=c(-1,1), D=c(-1,1)))
summary(fatorial1)

prod <- c(2.75,3.15,2.85,3.55,4.65,5.85,5.40,6.30,5.95,6.55,5.60,6.75,11.50,13.00,12.50,14.00)
fatorial2 <- add.response(fatorial1,prod)
summary(fatorial2)

# Com o objetivo de avaliar influência de quatro fatores sobre a produtividade do adsorvente (P)
# em uma separação de gases foi realizado um planejamento fatorial 24.  A partir dos dados do grafico normal
# e do grafico dos efeitos principais pode-se verificar que a Pressão de adsorção (A), Vazão de alimentação (C)
# e o Tempo de adsorção (D) são fatores significativos para o processo. Como tal análise não foi realizada em
# replicata, não podemos estimar o erro. Para resolver este problema, podemos criar um novo modelo linear que
# não envolva o fator Pressão de dessorção (B), visto que este se mostrou pouco significativo no processo de
#separação. Assim, podemos obter o valor do erro:

modelo1 <- lm(prod ~ A*B*C*D, data=fatorial2)
summary(modelo1)

DanielPlot(fatorial2)
MEPlot(fatorial2)
IAPlot(fatorial2)

modelo2 <- lm(prod ~ A*C*D, data=fatorial2)
summary(modelo2)

# Diante dos dados acima, podemos afirmar que os fatores de Pressão de adsorção (A), Vazão de alimentação (C)
# e o Tempo de adsorção (D) são de fato significativos. Porém, em graus de significância distintos.
# A Vazão de alimentação (C) e o Tempo de adsorção (D) são fatores significativos ao nível de 0.1% enquanto o
# fator Pressão de adsorção (A) é significativo a nível de 1%.
