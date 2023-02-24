## Exercício 5A.4 Intervalo de energias proibidas em semicondutores

# x = 1/T, 10^-3 k^-1
# Y = ln (sigma/omega^-1.m^-1)

x <- c(3.19,3.09,3.00,2.91,2.83,2.75,2.68,2.61,2.54,3.19,3.09,3.00,2.91,2.83,2.75,2.68,2.61,2.54)
y <- c(2.24,2.74,3.19,3.60,3.95,4.38,4.62,4.92,5.21,2.29,2.81,3.22,3.61,4.01,4.33,4.62,4.93,5.21)

##### Modelo linear ####
modelo1 <- lm(y~x)
summary(modelo1)
# ln(sigma) = 16.72749 - 4.51591*(1/T)
## Os dois parâmetros são altamente significativos.

##### Gráfico de dispersão #####
plot(x,y, xlab = "1/T (K^-1)", ylab = "ln(sigma/omega.m^-1)", col= "deeppink")
abline(modelo1)

##### ANOVA para o ajuste linear #####
library(alr3)
pureErrorAnova(modelo1)
#Analysis of Variance Table
#Response: y
#Df  Sum Sq Mean Sq    F value    Pr(>F)    
#x             1 15.9894 15.9894 19712.9572 2.397e-16 ***
#Residuals    16  0.0271  0.0017                         
#Lack of fit  7  0.0198  0.0028     3.4895   0.04263 *  
# Pure Error   9  0.0073  0.0008                         

#### Gráfico de resíduos ####
residuos <- resid(modelo1)
plot(y,residuos, col= "deeppink")
abline(0,0)
## Observa-se falta de ajuste, sugerindo que o modelo quadrático possa ser melhor.

################################################################################
##### Modelo quadrático ######
xquad <- x^2
modelo2<-lm(y~x+xquad)
summary(modelo2)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  10.0648     1.2390   8.124 7.12e-07 ***
#  x             0.1652     0.8691   0.190    0.852    
#xquad        -0.8178     0.1518  -5.389 7.53e-05 ***

# R-squared:  0.9994,	Adjusted R-squared:  0.9993 

##### ANOVA para o ajuste quadrático #####
library(alr3)
pureErrorAnova(modelo2)

# Gráfico de resíduos modelo quadrático:
residuos <- resid(modelo2)
plot(residuos)
plot(residuos, ylim=c(-0.1,0.1), col= "blue")
abline(0,0)
### O gráfico de resíduos ficou muito bom