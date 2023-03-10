# Exerc?cio 5A.1
# A flexibiliza??o do ar

# y = p = press?o (atm)
# x = v = volume
x <- c(48,46,44,42,40,38,36,34,32,30,28,26,24,23,22,21,20,19,18,17,16,15,14,13,12)
y <- c(1,1.049,1.097,1.15,1.212,1.27,1.35,1.429,1.517,1.616,1.727,1.865,2.019,2.105,2.199,2.302,2.427,2.545,2.674,2.841,3.017,3.195,3.449,3.702,4.036)

# Ajuste linear pelo m?todo dos m?nimos quadrados
modelo1 <-lm(y~x)
summary(modelo1)
#MQR/MQr= quanto maior melhor. Serve para comparar os modelos ajustados.
#A observa??o dos gr?ficos para avaliar o ajuste do modelo bem como a 
#distribui??o de res?duos ? obrigat?ria em qualquer situa??o.  Quando os 
#dados possuem replicatas podemos estimar o erro aleat?rio e quantificar
#se o modelo escolhido ? uma boa representa??o das observa??es.
# SQr= SQep+SQfaj

# % de varia??o explicada: R2 = 16,565/18,893 = 0,8768 = 87,68%
# MQR/MQr =  16,565/0,101 = 163,69
# Equa??o Pi =  4,125 - 0,074Vi

# Gerando o gr?fico de dispers?o dos pontos:
plot(x,y, xlab = "Volume", 
     ylab = "Press?o") # primeiro o valor de X e depois o de Y
abline(modelo1) # tra?a uma reta

# ANOVA para o Modelo linear:
library(alr3)
pureErrorAnova(modelo1)
# Analysis of Variance Table
# Response: y
# Df Sum Sq Mean Sq F value    Pr(>F)    
# x          1 16.565 16.5653  163.66 6.099e-12 ***
# Residuals 23  2.328  0.1012
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Gr?fico de res?duos modelo linear:
residuos <- resid(modelo1)
plot(residuos)
plot(residuos, ylim=c(-0.5,1), col= "deeppink")
abline(0,0) #tra?ar uma linha onde os coeficientes angulares e lineares s?o iguais a zero. Por isso 0,0

# Podemos observar pelo R2 que 87,68% da varia??o total ? explicada pela regress?o e o gr?fico de res?duos
# mostra que h? um comportamento n?o aleat?rio dos dados. Por isso, um outro modelo de ser utilizado.

##################################################################################################
# Ajuste quadr?tico:
xquad <- x^2
modelo2 <- lm(y~x+xquad)
summary(modelo2)
# MODELO QUADR?TICO
# Pi = 6.1832152 - 0.2380615Vi + 0.0027Vi^
# % de varia??o explicada: R20,9834 = 98,34%2


# Gerando o gr?fico de dispers?o dos pontos:
plot(x,y, ylab="Pressao (atm)", xlab=expression ("Volume"))
xv<-seq(min(x), max(x), 0.01)
yv<-predict(modelo2, list(x=xv, xquad=xv^2))
lines(xv,yv)

# ANOVA para o Modelo quadr?tico:
pureErrorAnova(modelo2)

# Gr?fico de res?duos modelo quadr?tico:
residuos <- resid(modelo2)
plot(residuos)
plot(residuos, ylim=c(-1,1), col= "deeppink")
abline(0,0)

#################################################################################################
# MODELO C?BICO
# Pi= 8.253e+00 - 4.910e-01V1 + 1.210e-02V2 - -1.051e-04V3

xquad<-x^2
xcub<-x^3
modelo3<-lm(y~x+xquad+xcub)
summary(modelo3)
# Residual standard error: 0.04403 on 21 degrees of freedom
# Multiple R-squared:  0.9978,	Adjusted R-squared:  0.9975 

# ANOVA para o Modelo c?bico:
library(alr3)
pureErrorAnova(modelo3)

# Gr?fico de dispers?o com uma curva c?bica
plot(x,y, ylab="Pressao (atm)", xlab=expression ("Volume"))
xv<-seq(min(x), max(x), 0.01)
yv<-predict(modelo3, list(x=xv, xquad=xv^2, xcub=xv^3))
lines(xv,yv)

# Gr?fico de dispers?o c?bico
residuos <- resid(modelo3)
plot(x,residuos,ylim = c (-0.1,0.15))
abline(0,0)

