#Box-Behnken: 3 n?veis para os fatores

library(rsm)
planejamento <- bbd(3,5, randomize = FALSE, coding = list(x1~(?cido-20)/3,
                                                         x2~(Temperaura-50)/10,
                                                         x3~(Tempo-4)/1 ))
planejamento

# transformando x1 x2 e x3 nos fatores reais)
#bbd(k fatores, n0 pontos centrais)

par(mfrow=c(1,2)) # numero de gr?ficos por imagem
varfcn(planejamento, ~ SO(x1,x2,x3))
varfcn(planejamento, ~SO(x1,x2,x3), contour = TRUE) # comportamento da variancia dos valores previstos no modelo
# buscamos um planejamento rotacionario

# Gr?fico: a variancia est? rotacionario
# as varaiancas dependem da distancia do ponto central
resposta <- c(10,10,12,8,10,13,12,14,12,18,18,7,8,18,14,10,13)
ajuste <- rsm(resposta ~ FO(x1,x2,x3),
              data = planejamento)
# SO = Modelo de segunda ordem (Second order) envolve:
# FO = Lineares (First order) &
# PQ = Puramente quadraticos &
# TWI = intera??o .

summary(ajuste)
---------------------------------------------------------
ajuste1 <- update(ajuste0, . ~ . + PQ(x1,x2,x3),
                    data = planejamento)
summary(ajuste1)

ajuste2 <- update(ajuste1, .~. + TWI(x2,x3))
summary(ajuste2)
---------------------------------------------------------
# Para saber se o modelo foi bom iremos plotar um gr?fico de res?duos:

par(mfrow=c(1,1))

plot(fitted(ajuste),resid(ajuste),xlab = "Valores ajustados",
     ylab = "Res?duos", main = "Res?duos x Valores ajustados")
abline(h=0,lty=2,col=2)

# Gr?fico de contorno: 
par(mfrow=c(1,3))
contour (ajuste, ~x1+x2+x3, image= TRUE,
         
         img.col = terrain.colors(50))
# Gr?fico de perspectiva (cada corte ? um gr?fico):
persp(ajuste, ~x1+x2+x3, col= heat.colors(50), contours = "colors")

# Para fixar o n?vel em outro valor:

vetor<- xs(ajuste)
vetor["x1"]=0
vetor["x2"]=0
vetor["x3"]=1
par(mfrow=c(1,3))
contour(ajuste, ~x1+x2+x3, at=vetor,
        image= TRUE, img.col = terrain.colors(50))


vetor<- xs(ajuste)
vetor["x1"]=0 # n?vel intermedi?rio
vetor["x2"]=1 # n?vel superior
vetor["x3"]=0 # n?vel intermediario
par(mfrow=c(1,3))
contour(ajuste, ~x1+x2+x3, at=vetor,
        image= TRUE, img.col = terrain.colors(50))

persp(ajuste, ~x1+x2+x3, at=vetor,
      col= topo.colors(50), contours = "colors")

# Cores poss?veis: cm.colors; topo.colors; terrain.colors; heat.colors; rainbow

