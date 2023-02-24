# 6.A5 Aumentando o oxigenio do ar

x1 <- c(-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,
        -0.8,-0.8,-0.4,0,0,0,0,0.2,0.4,0.4,0.6,0.6,0.8,0.8,0.8,1,0,0,0)
x2 <- c(-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,0,
        0.6,0.87,-0.33,-0.07,-0.33,0,-1,-0.07,-0.47,0.33,-0.73,0.6,-0.73,-0.87,0.8,-1,0,0,0)
x3 <- c(-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,
        1,1,1,-1,-0.7,0.9,-0.4,0,-0.2,-1,1,0,-0.2,0.3,0.6,0.6,-0.8,-0.9,0.8,1,0,0,0)
x4 <- c(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        -0.09,-0.91,-0.36,0.55,-0.09,-1,1,0.09,0.09,0.27,-0.73,0.55,-0.82,0.09,0.82,0,0,0,0)
y <- c(50.1,48.1,50.7,49.5,39.4,39.6,42.1,41.7,34.5,33.8,40.3,39.9,30.3,30.2,35.6,
       35.2,37.0,37.3,43.4,42.1,32.0,32.0,37.6,36.9,26.6,26.3,30.4,30.1,24.6,24.0,27.9,
       27.6,32.8,33.5,24.7,38.0,31.2,34.6,44.0,27.1,33.3,35.8,30.7,37.2,29.5,45.3,42.8,
       28.0,32.4,33.6,33.6,33.4)

ajuste <- rsm (y~SO(x1,x2,x3,x4))
summary(ajuste)

contour(ajuste,~x2+x3, image=TRUE, img.col = terrain.colors(60),
        xlabs=c("Pressão de adsorção","Vazão de alimentação"))
contour(ajuste,~x2+x4, image=TRUE, img.col = terrain.colors(60),
        xlabs=c("Pressão de adsorção","Tempo de adsorção"))

persp(ajuste,~x2+x3, col=heat.colors(50), contours = "colors",
      xlabs=c("Pressão de dessorção","Vazão de alimentação"),zlab="Resposta", theta = -50, 
      phi= 10)
persp(ajuste,~x2+x4, col=heat.colors(50), contours = "colors",
      xlabs=c("Pressão de adsorção","Tempo de adsorção"),zlab="Resposta", theta = -30, 
      phi= 10)

plot(fitted(ajuste),resid(ajuste),xlab = "Valores ajustados",
     ylab = "Resíduos", main = "Resíduos x Valores ajustados")
abline(h=0,lty=2,col=2)

# Cores possíveis: cm.colors; topo.colors; terrain.colors; heat.colors; rainbow
