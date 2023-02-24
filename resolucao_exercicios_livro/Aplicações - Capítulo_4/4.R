# 4A.4 Resposta catalítica da Mn-porfirina

library(FrF2)

# Inicialmente, precisamos avaliar se os sinais codificados gerados conferem com o previsto,
# considerando a relação geradora

fatorial1 <- FrF2(nruns = 8, nfactors=7, replications = 1,
                  randomize=FALSE, generators=c("AB", "AC", "BC", "ABC"))
summary(fatorial1)
qtde <- c(34.3,5.6,3.6,2.9,19.8,19.6,4.4,3.85)
fatorial2 <- add.response(fatorial1,qtde)
summary(fatorial2)

# Relação geradora D=AB  E=AC  F=BC  G=ABC
# Estrutura de confundimento:
# A=BD=CE=FG B=AD=CF=EG C=AE=BF=DG D=AB=CG=EF E=AC=BG=DF F=AG=BC=DE G=AF=BE=CD


modelo1 <- lm(qtde ~ A*B*C*D*E*F*G, data=fatorial2)
summary(modelo1)

DanielPlot(modelo1) #nenhum fator principal ou interação se mostrou significativo
MEPlot(modelo1) # C e F foram pouco significativos e serão desconsiderados
# e B é o fator mais significativo


#agora a estrtura de confundimento ficou:
#  A=BD B=AD=EG AE=DG D=AB E=BG AG=DE G=BE
modelo2 <- lm(qtde ~ A*B*D*E*G, data=fatorial2)
summary(modelo2)

modelo3 <- lm(qtde ~ A+B+D+E+G, data=fatorial2)
summary(modelo3)


#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  11.7562     0.2275   51.67 0.000374 ***
#  A1           -3.7687     0.2275  -16.57 0.003624 ** 
#  B1           -8.0687     0.2275  -35.47 0.000794 ***
#  D1            3.4562     0.2275   15.19 0.004305 ** 
#  E1            3.5812     0.2275   15.74 0.004011 ** 
#  G1           -3.5437     0.2275  -15.58 0.004096 ** 