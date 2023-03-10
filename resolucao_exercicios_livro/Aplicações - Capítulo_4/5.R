# 4A.S Escoamento de ?xidos na ind?stria sider?rgica

library(FrF2)
fatorial1 <- FrF2(nruns = 8, nfactors=4, replications = 1,
                  randomize=FALSE)
tempo <- c(32.5,26.0,76.0,38.5,74.0,35.5,23.0,42.0)
fatorial2 <- add.response(fatorial1,tempo)
summary(fatorial2)

#Rela??o geradora D=ABC
#Estrutura de confundimento = AB=CD AC=BD AD=BC

modelo1 <- lm(tempo ~ A*B*C*D, data=fatorial2)
summary(modelo1)

DanielPlot(modelo1) #D e B:C apresentaram ser significativos
MEPlot(modelo1)

# C ? o menos siginificativo e por isso podemos criar um novo modelo sem C e suas intera??es
# Com isso eliminamos os confundimentos, visto que todas as interacoes confundidas envolvem o fator C. 

modelo2 <- lm(tempo ~ A*B*D, data=fatorial2)
summary(modelo2)

modelo3 <- lm(tempo ~ A+B+D+A*B+A*D+B*D, data=fatorial2)
summary(modelo3)

# Como o objetivo nesse trabalho ? diminuir o tempo de escoamento,
# o planejamento mostra que a melhor condi??o para isso se d? quando:
# 1:Solvente=Alcool; 2:Aditivo=1%; 3:Catalisador=sem; 4:Tempo de estufa= 5 min

# Sobre o resultado do planejamento, observamos que os fatores A e D, Solvente e Tempo mostraram
# signific?ncia ao n?vel de 5%. Ou seja 95% de probabilidade de que os dados sejam produzidos ao acaso.
