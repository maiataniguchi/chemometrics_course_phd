#Regressão linear

temp <-c(40,45,50,55,60)
rend <-c(60,70,77,86,91)

modelo1 <- lm(rend ~ temp)
modelo1
summary(modelo1)

#Coefficients: (Intercept)-1.20; temp:1.56

library(alr3)
pureErrorAnova(modelo1)
plot(temp,rend, xlab = "Temperatura (°C)",
     ylab = "Rendimento (%)")
     
     #primeiro o valor de X e depois o de Y
abline(modelo1) # traça uma reta