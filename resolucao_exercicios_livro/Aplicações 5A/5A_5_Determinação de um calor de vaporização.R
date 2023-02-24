# Exercício 5A.5: calor de vaporização:

# x = T (K); y = P_vap (torr)
x = c(273, 283, 293, 303, 313, 323, 333, 343, 353) #X
y = c(0.044, 0.075, 0.122, 0.190, 0.288, 0.422, 0.601, 0.829, 1.124) #Y

# Calculando: x = 1/T; y= lnpvap
log(y)
x2 = 1/x

# Novos dados:
# x = 1/T; y= lnpvap
y <- c(-3.123565,-2.590267,-2.103734,-1.660731,-1.24479,-0.86274996,-0.509160,-0.1875351,0.11689375)
x <- c(0.003663,0.003533,0.003413,0.003300,0.003195,0.00309597,0.0030030,0.0029154,0.00283286)


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# Ajuste linear 
modelo1<-lm(y~x)
summary(modelo1)


# ANOVA para o Modelo linear:
library(alr3)
pureErrorAnova(modelo1)
#os resultados mostrarão y=regressão SQR e resíduos SQr. SQT=SQR+SQr. 
#SQR/SQT=R^2

# Gráfico de resíduos modelo linear:
residuos <- resid(modelo1)
plot(residuos)
abline(0,0) 
# (0,0) os coeficientes angulares e lineares são iguais a zero

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# Ajuste quadrático:
xquad <- x^2 
modelo2<-lm(y~x+xquad)
summary(modelo2)

# ANOVA para o Modelo quadrático:
pureErrorAnova(modelo2)

# Gráfico de resíduos modelo quadrático:
residuos <- resid(modelo2)
plot(residuos)
plot(residuos, ylim=c(-1,1), col= "blue")
abline(0,0)