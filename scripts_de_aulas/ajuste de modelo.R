#Regressão linear

temp <-c(30,35,40,45,50,55,60,65,70,30,35,40,45,50,55,60,65,70)
rend <-c(24,40,60,70,77,86,91,86,84,20,43,57,72,80,89,88,89,80)

modelo1 <- lm(rend ~ temp)
modelo1 # Coefficients (x)= -7.417; Temp (y) = 1.522 
summary(modelo1)

library(alr3)
pureErrorAnova(modelo1)
plot(temp,rend, xlab = "Temperatura (°C)",
     ylab = "Rendimento (%)")

#primeiro o valor de X e depois o de Y
abline(modelo1) # traça uma reta

# Sabendo que o erro relacionado ao ajuste do modelo é alto, iremos ajustar o modelo.
# Lack of fit  7 1938.6 

# Modelo quadrático
tempquad <- temp^2
modelo2 <- lm(rend~temp+tempquad)
summary(modelo2)
pureErrorAnova(modelo2)
