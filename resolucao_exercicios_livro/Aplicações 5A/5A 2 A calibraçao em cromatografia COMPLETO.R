# Exerc?cio 5A.2: A calibra??o em cromatografia
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
#A rela??o entre a soma quadr?tica da regress?o (SQR) e a soma quadr?tica 
#total (SQT) nos mostra o valor que R2 (0.9988). Sua interpreta??o diz 
#que 99.88 % dos dados foram ajustados pelo modelo linear. O gr?fico de 
#dispers?o de pontos com abline mostra uma boa correla??o entre a reta e 
#as respostas experimentais.




# Gr?fico de dispers?o dos pontos mostrando a aqdequa??o do modelo as respostas:
plot(x,y, xlab = "Concentra??o (mg/L", ylab = "Altura do pico (cm)") 
abline(modelo1)

# ANOVA para o Modelo linear:
library(alr3)
pureErrorAnova(modelo1)
#os resultados mostrar?o y=regress?o SQR e res?duos SQr. SQT=SQR+SQr. 
#SQR/SQT=R^2. Encontramos a porcentagem explicada, o restante fica para os res?duos.
#O erro puro ? algo inerente dos dados e n?o pode ser melhorado. 
#O erro por falta de ajuste (Lack of fit) ? relacionado ao modelo 
#e pode ser melhorado ao inserir termos quadr?ticos/c?bicos no modelo 
#de ajuste. Assim, a redu??o dos res?duos pode ser ocasionada somente 
#por uma melhora no ajuste, que por sua vez reduzir? o "lack of fit".
#O gr?fico de res?duos mostra uma boa dispers?o dos pontos em torno de zero. 
#Situa??es dessa natureza est?o relacionadas a presen?a de aleatoriedade na 
#distribui??o dos erros, ou seja, a vari?ncia dos erros ? constante e os pontos 
#n?o est?o correlacionados entre si.


#Df  Sum Sq Mean Sq    F value    Pr(>F)    
#x               1       1262.15 1262.15 31601.5996 < 2.2e-16 ***
#Residuals       19      1.58    0.08                         
#Lack of fit     4       0.98    0.24     6.1114  0.004005 ** 
#Pure Error      15      0.60    0.04 

#A porcentagem m?xima de varia??o explic?vel: (SQT-SQep)/SQT = 99,95%
#porcentagem explicada 99,87


# Gr?fico de res?duos modelo linear:
residuos <- resid(modelo1)
plot(residuos)
abline(0,0) 
# (0,0) os coeficientes angulares e lineares s?o iguais a zero





#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# Ajuste quadr?tico:
xquad <- x^2
modelo2<-lm(y~x+xquad)
summary(modelo2)

#A partir dos dados pode-se chegar a equa??o de ajuste:
#Altpico=-0.001038+13.7456Conc+1.2418Conc2


# Gerando o gr?fico de dispers?o dos pontos:
plot(x,y, xlab = "Concentra??o (mg/mL)", 
     ylab = "Altura (cm)") 
xv <-seq (min(x),max(x),0.01)
yv<-predict(modelo2, list(x=xv, xquad=xv^2))
lines(xv,yv)


# ANOVA para o Modelo quadr?tico:
pureErrorAnova(modelo2)
#Como temos replicata, temos os res?duos desmembrados em erro puro e 
#falta de ajuste. Pode-se verificar que a falta de ajuste foi menor 
#(0.01) comparado ao ajuste linear, enquanto o erro puro (caracter?stica 
#das respostas) se manteve constante. 

# Gr?fico de res?duos modelo quadr?tico:
residuos <- resid(modelo2)
plot(residuos)
plot(residuos, ylim=c(-1,1), col= "blue")
abline(0,0)

#O gr?fico de res?duos manteve o perfil aleat?rio dos erros, 
#por?m agora os dados se concentraram mais pr?ximo do zero. 


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# MODELO C?BICO

xquad<-x^2
xcub<-x^3
modelo3<-lm(y~x+xquad+xcub)
summary(modelo3)

# ANOVA para o Modelo c?bico:
library(alr3)
pureErrorAnova(modelo3)

# Gr?fico de dispers?o com uma curva c?bica
plot(x,y, ylab="Altura", xlab=expression ("Concentra??o"))
xv<-seq(min(x), max(x), 0.01)
yv<-predict(modelo3, list(x=xv, xquad=xv^2, xcub=xv^3))
lines(xv,yv)

# Gr?fico de dispers?o c?bico
residuos <- resid(modelo3)
plot(x,residuos,ylim = c (-0.1,0.15))
abline(0,0)

#A partir dos dados pode-se chegar a equa??o de ajuste, com um valor de R2 de 0.999:
#Altpico=-0.00282+13.7833Conc+1.1633Conc2+0.03907Conc3
#A an?lise de vari?ncia mostra que a qualidade do ajuste n?o ? melhorada ao 
#inserir o termo c?bico. A falta de ajuste se mant?m a mesma do ajuste quadr?tico 
#e o coeficiente do termo c?bico fica 0.00. Al?m disso, n?o houve melhora 
#no valor de R2.Nesse caso, o ajuste quadr?tico foi mais eficiente. 
