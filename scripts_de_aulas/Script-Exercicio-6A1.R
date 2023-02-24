x1 <- c(-1,1,-1,1,-1.4,1.4,0,0,0,0,0,0,0)
x2 <- c(-1,-1,1,1,0,0,-1.4,1.4,0,0,0,0,0)
y <- c(0.373,0.497,0.483,0.615,0.308,0.555,0.465,0.628,0.538,0.549,0.536,0.549,0.538)

#ajustar o modelo linear

mod1 <- lm(y ~ x1 + x2)
summary(mod1)

library(alr3)
pureErrorAnova(mod1)

mod2 <- lm(y ~ polym(x1, x2, degree=2, raw=TRUE))
summary(mod2)

pureErrorAnova(mod1)

#cria uma sequencia de valores para as variáveis x e y
H=seq(-1.4,1.4,0.1)
K=seq(-1.4,1.4,0.1)

#cria uma funcao
modelo=function(a,b){0.542+0.076*a+0.058*b-0.055*a^2}

#cria a matriz de valores para a variavel dependente
R=outer(H,K,modelo); R

#cria a superficie

persp(H,K,R, theta = -40, phi = 35,ticktype = "detailed",xlab="H2SO4", ylab="KI",zlab = "Sinal")

contour(H,K,R,nlevels=12, xlab="H2SO4", ylab="KI")

col.pal<-colorRampPalette(c("blue","green", "yellow", "red"))
colors<-col.pal(100)
z.facet.center <- (R[-1, -1] + R[-1, -ncol(R)] + R[-nrow(R), -1] + R[-nrow(R), -ncol(R)])/4
z.facet.range<-cut(z.facet.center, 100)

persp(H,K,R, theta = -40, phi = 35,ticktype = "detailed",xlab="H2SO4", ylab="KI",zlab = "Sinal",col=colors[z.facet.range],
      shade = 0.3,expand = 0.9)


cols<-rev(colorRampPalette(c("blue","green", "yellow", "red"))(24))
filled.contour(H,K,R, xlab="H2SO4", ylab="KI",col=cols)

filled.contour(H,K,R, xlab="H2SO4", ylab="KI",color.palette=terrain.colors)
filled.contour(H,K,R, nlevels=30, xlab="H2SO4", ylab="KI",color.palette=cm.colors)
filled.contour(H,K,R, nlevels=30, xlab="H2SO4", ylab="KI",color.palette=heat.colors)
filled.contour(H,K,R, nlevels=24, xlab="H2SO4", ylab="KI",color.palette=terrain.colors)
filled.contour(H,K,R, nlevels=30, xlab="H2SO4", ylab="KI",color.palette=topo.colors)



