# 6A.4 Produção de lacase

x1 <- c(-1,-1,1,1,-1,-1,1,1,0,0,0,0,-1.41,-1.41,0,0,1.41,1.41,0,0)
x2 <- c(-1,-1,-1,-1,1,1,1,1,0,0,0,0,0,0,1.41,1.41,0,0,-1.41,-1.41)
y <- c(3.50,3.20,1.17,1.70,4.10,5.40,1.90,2.10,4.80,5.00,4.70,5.20,5.25,5.41,6.00,3.20,2.30,1.60,0.50,0.50)

ajuste <- rsm (y~SO(x1,x2))
summary(ajuste)
# y =4,93-1,18x1 + 0,97X2 -O,70x1^2 -1,25x2^2 -0,21x1x2

### Gráficos ####
# Curva de nível
contour(ajuste,~x1+x2, image=TRUE, img.col = terrain.colors(50),
        xlabs=c("Concentração de álcool (mM)","Tempo de cultivo (dias)"))

persp(ajuste,~x1+x2, col=rainbow(50), contours = "colors",
      xlabs=c("Concentração de álcool (mM)","Tempo de cultivo (dias)"),zlab="Resposta")

plot(fitted(ajuste),resid(ajuste),xlab = "Valores ajustados",
     ylab = "Resíduos", main = "Resíduos x Valores ajustados")
abline(h=0,lty=2,col=2)
