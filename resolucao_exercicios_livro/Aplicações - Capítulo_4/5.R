# 4A.S Escoamento de óxidos na indústria siderúrgica

library(FrF2)
fatorial1 <- FrF2(nruns = 8, nfactors=4, replications = 1,
                  randomize=FALSE)
tempo <- c(32.5,26.0,76.0,38.5,74.0,35.5,23.0,42.0)
fatorial2 <- add.response(fatorial1,tempo)
summary(fatorial2)

#Relação geradora D=ABC
#Estrutura de confundimento = AB=CD AC=BD AD=BC

modelo1 <- lm(tempo ~ A*B*C*D, data=fatorial2)
summary(modelo1)

DanielPlot(modelo1) #D e B:C apresentaram ser significativos
MEPlot(modelo1)

# C é o menos siginificativo e por isso podemos criar um novo modelo sem C e suas interações
# Com isso eliminamos os confundimentos, visto que todas as interacoes confundidas envolvem o fator C. 

modelo2 <- lm(tempo ~ A*B*D, data=fatorial2)
summary(modelo2)

modelo3 <- lm(tempo ~ A+B+D+A*B+A*D+B*D, data=fatorial2)
summary(modelo3)

# Como o objetivo nesse trabalho é diminuir o tempo de escoamento,
# o planejamento mostra que a melhor condição para isso se dá quando:
# 1:Solvente=Alcool; 2:Aditivo=1%; 3:Catalisador=sem; 4:Tempo de estufa= 5 min

# Sobre o resultado do planejamento, observamos que os fatores A e D, Solvente e Tempo mostraram
# significância ao nível de 5%. Ou seja 95% de probabilidade de que os dados sejam produzidos ao acaso.
