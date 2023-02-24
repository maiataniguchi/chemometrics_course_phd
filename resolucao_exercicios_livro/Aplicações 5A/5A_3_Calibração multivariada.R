## 5A.3 Calibração multivariada
# y = volume
# x1 = A530
# x2 = A440
# x3 = A410
y <- c(7.50,6.60,5.00,3.40,2.50,1.70,5.00,5.00,4.00)
x1 <- c(0.084,0.108,0.149,0.182,0.205,0.159,0.149,0.113,0.149)
x2 <- c(0.506,0.479,0.426,0.375,0.342,0.409,0.426,0.488,0.432)
x3 <- c(0.602,0.561,0.472,0.385,0.334,0.375,0.472,0.523,0.456)

#### Modelo linear #####
modelo1 <- lm(y ~ x1 + x2 +x3)
summary(modelo1)
# R2 = 0.9974 = 99,74%
# Vi = 2.642 - 3.658x1 - 37.088x2 + 39.636x3

#### Gráfico de dispersão ####
# Gráfico de dispersão dos pontos mostrando a aqdequação do modelo as respostas:
plot(x1+x2+x3,y, xlab = "Absorbância", ylab = "Volume", col= "deeppink")
abline(modelo1)

#### Gráfico de resíduos ####
residuos <- resid(modelo1)
plot(y,residuos, col= "deeppink")
abline(0,0)
# Apresenta bom aspecto, isto é, não parece ter estrutura.

#ANOVA
library(alr3)
pureErrorAnova(modelo1)
# Response: y
#               Df  Sum Sq  Mean Sq    F value    Pr(>F)    
#  x1            1 19.8822  19.8822 7.9656e+31 < 2.2e-16 ***
#  x2            1  0.8576   0.8576 3.4361e+30 3.434e-16 ***
#  x3            1  6.6438   6.6438 2.6618e+31 < 2.2e-16 ***
#  Residuals     5  0.0719   0.0144                         
#  Lack of fit   4  0.0719   0.0180 7.2060e+28 2.794e-15 ***
#  Pure Error    1  0.0000   0.0000 

# % da variação explicada: SQR/SQT = 27,3836/27,4555 = 0,9974 = 99,74%
# MQR/MQr = = (27.3836/3)/0.0144 =633.88

# O modelo explica 99,74% da variação total e o valor de MQR/MQr = 633,88 é bastante alto.
# O termo constante não é estatisticamente significativo, assim como x1 , o que indica que esse comprimento
# de onda não ajuda na determinação do íon permanganato.

############################################################################
#### Modelo quadrático #####
xquad1 <- x1^2
xquad2 <- x2^2
xquad3 <- x3^2
modelo2<-lm(y~x1+x2+x3+xquad1+xquad2+xquad3)
summary(modelo2)
