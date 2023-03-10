# Exerc?cio 6A-2
# metodologia de superf?cie de reposta: t?cnica de otimiza??o baseada em planejamentos fatoriais
#A metodologia de superf?cies de resposta tem duas etapas distintas -
# modelagem e deslocamento-, que s?o repetidas tantas vezes quantas forem necess?rias, com o objetivo de
# atingir uma regi?o ?tima da superf?cie investigada
library(rsm)
x1 <- c(-1,1,-1,1,-1,1,-1,1,-1.682,1.682,0,0,0,0,0,0,0,0,0)
x2 <- c(-1,-1,1,1,-1,-1,1,1,0,0,-1.682,1.682,0,0,0,0,0,0,0)
x3 <- c(-1,-1,-1,-1,1,1,1,1,0,0,0,0,-1.682,1.682,0,0,0,0,0)
y <- c(47.34,53,53.64,54.28,48.85,53.73,55.19,58.31,51.9,57.34,47.62,57.35,50.73,57.68,56.24,55.74,57.23,56.85,55.42)

ajuste <- rsm (y~SO(x1,x2,x3))
summary(ajuste)

## GR?FICO ###

#par(mfrow =c(1,1))
contour(ajuste,~x1+x2)
# obtivemos um grafico em curvas de n?vel
contour(ajuste,~x1+x2+x3, image=TRUE) #deixa colorido
contour(ajuste,~x1+x2, image=TRUE, img.col = terrain.colors(50),xlabs=c("Tempo","Temperatura"))
contour(ajuste,~x1+x3, image=TRUE, img.col = terrain.colors(50),xlabs=c("Tempo","Concentra??o"))
contour(ajuste,~x2+x3, image=TRUE, img.col = terrain.colors(50),xlabs=c("Temperatura","Concentra??o"))


persp(ajuste,~x1+x2+x3, col=terrain.colors(50),contours = "colors")               

persp(ajuste,~x1+x2, col=rainbow(50),contours = "colors", theta = -30, phi= 40, xlabs=c("Tempo","Temperatura"),zlab="Resposta")   
persp(ajuste,~x2+x3, col=rainbow(50),contours = "colors", theta = -30, phi= 40, xlabs=c("Temperatura","Concentra??o"),zlab="Resposta") 
                # x1 e x2 s?o independentes e Y a dependente.                               
                





