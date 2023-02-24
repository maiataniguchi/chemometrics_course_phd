# Exercício 5A.2: A calibração em cromatografia
#inserir dados:

# y = altura x = conc 
x <- c(0.0133,0.0665,0.3325,0.6650,0.9975,1.3300,0.0133,0.0665,0.3325,0.6650,
       0.9975,1.3300,0.0133,0.0665,0.3325,0.6650,0.9975,1.3300,0.0133,1.3300,0.0133)
y <- c(0.1836,0.9373,4.6227,9.6905,14.7607,21.0033,0.1787,0.9177,4.7812,9.9405,
       15.0113,20.2700,0.1837,0.9224,4.6256,9.5754,14.9641,20.5719,0.1806,20.0915,0.1861)
       

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# Ajuste linear 
modelo1<-lm(y~x)
summary(modelo1)
#A relação entre a soma quadrática da regressão (SQR) e a soma quadrática 
#total (SQT) nos mostra o valor que R2 (0.9988). Sua interpretação diz 
#que 99.88 % dos dados foram ajustados pelo modelo linear. O gráfico de 
#dispersão de pontos com abline mostra uma boa correlação entre a reta e 
#as respostas experimentais.




# Gráfico de dispersão dos pontos mostrando a aqdequação do modelo as respostas:
plot(x,y, xlab = "Concentração (mg/L", ylab = "Altura do pico (cm)") 
abline(modelo1)

# ANOVA para o Modelo linear:
library(alr3)
pureErrorAnova(modelo1)
#os resultados mostrarão y=regressão SQR e resíduos SQr. SQT=SQR+SQr. 
#SQR/SQT=R^2. Encontramos a porcentagem explicada, o restante fica para os resíduos.
#O erro puro é algo inerente dos dados e não pode ser melhorado. 
#O erro por falta de ajuste (Lack of fit) é relacionado ao modelo 
#e pode ser melhorado ao inserir termos quadráticos/cúbicos no modelo 
#de ajuste. Assim, a redução dos resíduos pode ser ocasionada somente 
#por uma melhora no ajuste, que por sua vez reduzirá o "lack of fit".
#O gráfico de resíduos mostra uma boa dispersão dos pontos em torno de zero. 
#Situações dessa natureza estão relacionadas a presença de aleatoriedade na 
#distribuição dos erros, ou seja, a variância dos erros é constante e os pontos 
#não estão correlacionados entre si.


#Df  Sum Sq Mean Sq    F value    Pr(>F)    
#x               1       1262.15 1262.15 31601.5996 < 2.2e-16 ***
#Residuals       19      1.58    0.08                         
#Lack of fit     4       0.98    0.24     6.1114  0.004005 ** 
#Pure Error      15      0.60    0.04 

#A porcentagem máxima de variação explicável: (SQT-SQep)/SQT = 99,95%
#porcentagem explicada 99,87


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

#A partir dos dados pode-se chegar a equação de ajuste:
#Altpico=-0.001038+13.7456Conc+1.2418Conc2


# Gerando o gráfico de dispersão dos pontos:
plot(x,y, xlab = "Concentração (mg/mL)", 
     ylab = "Altura (cm)") 
xv <-seq (min(x),max(x),0.01)
yv<-predict(modelo2, list(x=xv, xquad=xv^2))
lines(xv,yv)


# ANOVA para o Modelo quadrático:
pureErrorAnova(modelo2)
#Como temos replicata, temos os resíduos desmembrados em erro puro e 
#falta de ajuste. Pode-se verificar que a falta de ajuste foi menor 
#(0.01) comparado ao ajuste linear, enquanto o erro puro (característica 
#das respostas) se manteve constante. 

# Gráfico de resíduos modelo quadrático:
residuos <- resid(modelo2)
plot(residuos)
plot(residuos, ylim=c(-1,1), col= "blue")
abline(0,0)

#O gráfico de resíduos manteve o perfil aleatório dos erros, 
#porém agora os dados se concentraram mais próximo do zero. 


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# MODELO CÚBICO

xquad<-x^2
xcub<-x^3
modelo3<-lm(y~x+xquad+xcub)
summary(modelo3)

# ANOVA para o Modelo cúbico:
library(alr3)
pureErrorAnova(modelo3)

# Gráfico de dispersão com uma curva cúbica
plot(x,y, ylab="Altura", xlab=expression ("Concentração"))
xv<-seq(min(x), max(x), 0.01)
yv<-predict(modelo3, list(x=xv, xquad=xv^2, xcub=xv^3))
lines(xv,yv)

# Gráfico de dispersão cúbico
residuos <- resid(modelo3)
plot(x,residuos,ylim = c (-0.1,0.15))
abline(0,0)

#A partir dos dados pode-se chegar a equação de ajuste, com um valor de R2 de 0.999:
#Altpico=-0.00282+13.7833Conc+1.1633Conc2+0.03907Conc3
#A análise de variância mostra que a qualidade do ajuste não é melhorada ao 
#inserir o termo cúbico. A falta de ajuste se mantêm a mesma do ajuste quadrático 
#e o coeficiente do termo cúbico fica 0.00. Além disso, não houve melhora 
#no valor de R2.Nesse caso, o ajuste quadrático foi mais eficiente. 
