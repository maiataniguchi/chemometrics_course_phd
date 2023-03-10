#Exerc?cio 5A-2 - Calibra??o em cromatografia
# x = concentra??o  y = altura do pico   
x <- c(0.0133,0.0665,0.3325,0.6650,0.9975,1.3300,0.0133,0.0665,0.3325,0.6650,
       0.9975,1.3300,0.0133,0.0665,0.3325,0.6650,0.9975,1.3300,0.0133,1.3300,0.0133)
y <- c(0.1836,0.9373,4.6227,9.6905,14.7607,21.0033,0.1787,0.9177,4.7812,9.9405,
       15.0113,20.2700,0.1837,0.9224,4.6256,9.5754,14.9641,20.5719,0.1806,20.0915,0.1861)

#### Modelo linear ####
modelo1 <- lm(y~x)
summary(modelo1)
#Ajuste linear: A = -0.19057 + 15.34879C
# 99,88%

# Gr?fico de dispers?o dos pontos mostrando a aqdequa??o do modelo as respostas:
plot(x,y, xlab = "Concentra??o (mg/L", ylab = "Altura do pico (cm)") 
abline(modelo1)

# gr?fico dos res?duos
residuos <- resid(modelo1)
plot(y,residuos)
abline(0,0)

#ANOVA
library(alr3)
pureErrorAnova(modelo1)

##################################################################
## Modelo quadr?tico:
xquad<-x^2
modelo2<-lm(y~x+xquad)
summary(modelo2)

# Ajuste quadr?tico : A = -0.001038 + 13.745572C + 1.241834C2
plot(x,y)
xv<-seq(min(x), max(x), 0.01)
yv<-predict(modelo2, list(x=xv, xquad=xv^2))
lines(xv,yv)

# Gr?fico dos res?duos
residuos <- resid(modelo2)
plot(y,residuos)
abline(0,0)

#ANOVA - modelo quadr?tico
library(alr3)
pureErrorAnova(modelo2)

#####################################################################
# MODELO C?BICO

xquad<-x^2
xcub<-x^3
modelo3<-lm(y~x+xquad+xcub)
summary(modelo3)

# ANOVA para o Modelo c?bico:
library(alr3)
pureErrorAnova(modelo3)

# Gr?fico de dispers?o com uma curva c?bica
plot(x,y, ylab="Pressao (atm)", xlab=expression ("Volume"))
xv<-seq(min(x), max(x), 0.01)
yv<-predict(modelo3, list(x=xv, xquad=xv^2, xcub=xv^3))
lines(xv,yv)

# Gr?fico de res?duos para modelo c?bico:
residuos <- resid(modelo3)
plot(x,residuos,ylim = c (-0.1,0.15))
abline(0,0)
