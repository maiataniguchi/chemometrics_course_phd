# Exercício 6A-3 Diminuindo o colesterol

# Aplicação 6A-3-diminuindo o colesterol
x1 <- c(-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,0,0,0,-2,2,0,0,0,0,0,0,0,0)
x2 <- c(-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,0,0,0,0,0,-2,2,0,0,0,0,0,0)
x3 <- c(-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,0,0,0,0,0,0,0,-2,2,0,0,0,0)
x4 <- c(-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,-2,2,0,0)
y <- c(1.701,1.120,1.607,0.881,1.860,0.965,1.786,0.933,2.131,2.072,2.095,2.002,2.101,2.055,2.017,
       1.972,1.763,1.840,1.935,1.713,1.089,1.643,1.601,1.691,1.648,0.675,2.049,1.783,1.983)

library(rsm)

ajuste <- rsm (y~SO(x1,x2,x3,x4))
summary(ajuste)

contour(ajuste, ~x1+x2+x3+x4, image= TRUE, img.col = terrain.colors(50))

# Para fixar o nível em outro valor:
vetor<- xs(ajuste)
vetor["x1"]=1
vetor["x2"]=0
vetor["x3"]=0
vetor["x4"]=0
contour(ajuste, ~x1+x2+x3+x4, at=vetor,
        image= TRUE, img.col = terrain.colors(50))

#x1=conc x2=qtd diatomácia x3=temperatura e x4=pH
# obtivemos um grafico em curvas de nível
contour(ajuste,~x1+x2, image=TRUE, img.col = terrain.colors(50),xlabs=c("Concentração","Quantidade"))
contour(ajuste,~x1+x3, image=TRUE, img.col = terrain.colors(50),xlabs=c("Concentração","Temperatura"))
contour(ajuste,~x1+x4, image=TRUE, img.col = terrain.colors(50),xlabs=c("Concentração","pH"))
contour(ajuste,~x2+x3, image=TRUE, img.col = terrain.colors(50),xlabs=c("Quantidade","Temperatura"))
contour(ajuste,~x2+x4, image=TRUE, img.col = terrain.colors(50),xlabs=c("Quantidade","pH"))
contour(ajuste,~x3+x4, image=TRUE, img.col = terrain.colors(50),xlabs=c("Temperatura","pH"))



persp(ajuste,~x1+x2, col=rainbow(50),contours = "colors", theta = -30, phi= 40, xlabs=c("Concentração","Quantidade"),zlab="Resposta")   
persp(ajuste,~x1+x3, col=rainbow(50),contours = "colors", theta = -30, phi= 40, xlabs=c("Concentração","Temperatura"),zlab="Resposta")   
persp(ajuste,~x1+x4, col=rainbow(50),contours = "colors", theta = -30, phi= 40, xlabs=c("Concentração","pH"),zlab="Resposta")   
persp(ajuste,~x2+x3, col=rainbow(50),contours = "colors", theta = -30, phi= 40, xlabs=c("Quantidade","Temperatura"),zlab="Resposta")   
persp(ajuste,~x2+x4, col=rainbow(50),contours = "colors", theta = -60, phi= 40, xlabs=c("Quantidade","pH"),zlab="Resposta")   
persp(ajuste,~x3+x4, col=rainbow(50),contours = "colors", theta = -30, phi= 10, xlabs=c("Temperatura","pH"),zlab="Resposta") 

