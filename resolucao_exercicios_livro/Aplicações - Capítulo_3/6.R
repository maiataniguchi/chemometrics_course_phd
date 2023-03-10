#3A.6 Desempenho de eletrodos de Ti/Ti02
#Fatores:
#A = Solu??o precursora; B = N?mero de camadas; C = Temperatura; D= Concentra??o de nitrobenzeno
# E = Velocidade de varredura

library(FrF2)

fatorial1 <- FrF2(nruns = 32, nfactors=5, replications = 1,
                  randomize=FALSE, factor.names=list( A=c(-1,1),B=c(-1,1),C=c(-1,1), D=c(-1,1), E=c(-1,1)))
summary(fatorial1)

desemp <- c(2.07,2.03,4.71,7.01,1.71,2.10,4.36,3.71,7.15,4.87,8.96,12.25,4.28,3.13,9.42,8.68,1.70,1.39,4.50,
          5.92,0.73,0.77,3.20,3.08,2.51,1.82,5.60,7.61,1.55,1.05,4.25,4.38)
fatorial2 <- add.response(fatorial1,desemp)
summary(fatorial2)

DanielPlot(fatorial2)
MEPlot(fatorial2)
IAPlot(fatorial2)

# Pode-se observar a partir da Figura 2 que o fator A (Solu??o precursora) apresenta-se pouco efetivo
# no desempenho dos eletrodos. Essa observa??o pode ser corroborada pelo gr?fico normal apresentado na Figura 1,
# em que a vari?vel A n?o ? significativa.
# J? os fatores B,C,D e E apresentaram certa signific?ncia bem como a intera??o entre alguns fatores. 

modelo1 <- lm(desemp ~ A*B*C*D*E, data=fatorial2)
summary(modelo1)

# Para estimar o erro e o grau de signific?ncia dos fatores, podemos excluir o fator A do planejamento, visto
# que este n?o mostrou influenciar no processo de desempenho do eletrodo. Assim, obtemos os valores:

modelo2 <- lm(desemp ~ B*C*D*E, data=fatorial2)
summary(modelo2)

# Como previsto por meio da interpreta??o dos gr?ficos, os fatores B, C, D e E s?o significativos.
# Todos mostraram signific?ncia estat?stica a n?vel de 0.1%. Como o objetivo deste trabalho ? maximizar o
# desempenho os eletrodos os fatores devem ser utilizados nas condi??es em que o maior rendimento foi obtido.
# S?o eles: N?mero de camadas (B) em n?vel superior = 10; Temperatura (C) em n?vel inferior = 450 ?C;
# Concentra??o de nitrobenzeno (D) em n?vel superior = 8 mM  e Velocidade de varredura (E) em n?vel inferior = 50 mV.
