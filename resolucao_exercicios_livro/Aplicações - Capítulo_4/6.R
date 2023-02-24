#4A.6 Produção de violaceína por bactérias

library(FrF2)
fatorial1 <- FrF2(nruns = 16, nfactors=11, replications = 1,
                  randomize=FALSE, generators=c("AB", "AC", "AD", "BC", "BD", "CD", "ABC"))



resposta <- c(0.39,0.19,0.26,0.14,0.26,0.15,0.31,0,0.09,0,0.24,0.26,0.17,0.14,0.26,0.31)
fatorial2 <- add.response(fatorial1,resposta)
summary(fatorial2)

#Estrutura de confundimento = AB=CD AC=BD AD=BC

modelo1 <- lm(resposta ~ A*B*C*D*E*F*G*H*J*K*L, data=fatorial2)
summary(modelo1)

DanielPlot(modelo1) #D e B:C apresentaram ser significativos
MEPlot(modelo1)

# C é o menos siginificativo e por isso podemos criar um novo modelo sem C e suas interações
# Com isso eliminamos os confundimentos, visto que todas as interacoes confundidas envolvem o fator C. 

modelo2 <- lm(resposta ~ A*B*D*G*J*K*L, data=fatorial2)
summary(modelo2)

DanielPlot(modelo2) #D e B:C apresentaram ser significativos
MEPlot(modelo2)

modelo3 <- lm(resposta ~ A+B+G+J+K+L+A*B+B*G+A*K+B*K+J*K+D*L, data=fatorial2)
summary(modelo3)
