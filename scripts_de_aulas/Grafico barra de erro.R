###############################################
library(Hmisc)

temp <-c(30,35,40,45,50,55,60,65,70)
rend1 <-c(24,40,60,70,77,86,91,86,84)
rend2 <-c(20,43,57,72,80,89,88,89,80)
#Média
md=(rend1+rend2)/2
md

desvio=sqrt( ( (rend1-md)^2 + (rend2-md)^2) /(2-1)) # numero total de pontos para calcular o desvpad (2-1)
desvio

#Gráfico barra de erros onde md-desvio,md+desvio é a largura da barra de erro.
errbar(temp,md,md-desvio,md+desvio, type="p", ylab = "Rendimento (%)",
xlab=expression("Temperatura (°C)"))

temp <-c(30,35,40,45,50,55,60,65,70,30,35,40,45,50,55,60,65,70)
rend <-c(24,40,60,70,77,86,91,86,84,20,43,57,72,80,89,88,89,80)

tempquad <- temp^2
modelo2 <- lm(rend~temp+tempquad)
summary(modelo2)

# Inserindo uma linha sobre o gráfico:
xv<-seq(min(temp), max(temp), 0.01) # onde 0.01 é o intervalo entre um ponto e outro no eixo X
yv<-predict(modelo2, list(temp=xv, tempquad=xv^2))
lines(xv,yv, col= "deeppink", lty="dashed", lwd=2)
