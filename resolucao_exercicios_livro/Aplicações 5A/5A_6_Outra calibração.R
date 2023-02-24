# Exercício 5A_6 : Outra calibração

y <- c(0.000,0.500,1.000,2.000,3.000,0.000,0.500,1.000,2.000,3.000,0.000,0.500,1.000,2.000,3.000)
x <- c(0.696,7.632,14.804,28.895,43.993,0.696,7.688,14.861,29.156,43.574,0.706,7.603,14.731,29.322,44.699)

modelo1 <- lm(y~x)
summary(modelo1)
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.0320430  0.0091637  -3.497  0.00394 ** 
# x            0.0691238  0.0003698 186.919  < 2e-16 ***

#### Gráfico de dispersão ####
# Gráfico de dispersão dos pontos mostrando a aqdequação do modelo as respostas:
plot(x,y, xlab = "Absorbância", ylab = "Volume", col= "deeppink")
abline(modelo1)

#### Gráfico de resíduos ####
residuos <- resid(modelo1)
plot(y,residuos, col= "deeppink")
abline(0,0)

#ANOVA
library(alr3)
pureErrorAnova(modelo1)
# Analysis of Variance Table
# Response: y
#Df Sum Sq Mean Sq    F value   Pr(>F)   
# x1         1 5.7986  5.7986 2.3702e+05 0.001308 **
# x2         1 0.0014  0.0014 5.6936e+01 0.083881 . 
# x3         1 0.0000  0.0000 1.2288e+00 0.467262   
# Residuals  1 0.0000  0.0000

###################################################################################

