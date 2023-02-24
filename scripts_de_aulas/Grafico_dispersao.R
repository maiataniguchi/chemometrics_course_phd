#Dispersão com ajuste
# Grafico de dispersão dos valores médios

temp <-c(30,35,40,45,50,55,60,65,70)
rend1 <-c(24,40,60,70,77,86,91,86,84)
rend2 <-c(20,43,57,72,80,89,88,89,80)
#Média
md=(rend1+rend2)/2
md
# Gerando o gráfico:
plot(temp,md,ylab = "Rendimento(%)",
     xlab = expression("Temperatura" ~ (""^{o}*C)))

# Ajustando o modelo quadratico:
temp <-c(30,35,40,45,50,55,60,65,70,30,35,40,45,50,55,60,65,70)
rend <-c(24,40,60,70,77,86,91,86,84,20,43,57,72,80,89,88,89,80)

tempquad <- temp^2
modelo2 <- lm(rend~temp+tempquad)
summary(modelo2)

# Inserindo uma linha sobre o gráfico:
xv<-seq(min(temp), max(temp), 0.01) # onde 0.01 é o intervalo entre um ponto e outro no eixo X
yv<-predict(modelo2, list(temp=xv, tempquad=xv^2))
lines(xv,yv, col= "deeppink", lty="dashed", lwd=2)

plot(modelo2)

# Resíduos

residuos <- resid(modelo2)
plot(residuos, ylim=c(-10,10), col= "deeppink")
abline(0,0,) #traçar uma linha onde os coeficientes angulares e lineares são iguais a zero. Por isso 0,0