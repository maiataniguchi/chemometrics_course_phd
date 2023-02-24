# 4A.l Adsorção em sílicas organofuncionalizadas
# O planejamento fracionário permite a previsão do comportamento de sistemas com um número menor de ensaios.
# de um planejamento. Neste caso, por exemplo, temos 4 fatores o que geraria um planejamento 2^4, isto é, 16 ensaios.
# No entanto, um planejamento fracionário 2^4-1 permite avaliar o comportamento dos 4 fatores com somente 8 experimentos.

library(FrF2)
fatorial1 <- FrF2(nruns = 8, nfactors=4, replications = 1,
                  randomize=FALSE, factor.names=list( TSil=c(-1,1),Sal=c(-1,1),
                                                      Solv=c(-1,1), QSil=c(-1,1)))

summary(fatorial1)
y <- c(0.39,1.74,1.37,1.68,4.66,6.12,6.09,2.61)
fatorial2 <- add.response(fatorial1,y)
summary(fatorial2)

modelo1 <- lm(y ~ TSil*Sal*Solv*QSil, data=fatorial2)
summary(modelo1)

DanielPlot(fatorial2)
# Analisando o gráfico normal podemos observar que apenas o solvente mostra-se significativo.
# Para avaliar melhor, podemos observar o gráfico dos efeitos principais.
MEPlot(fatorial2)
# Notamos que de fato o solvente é o fator mais significativo no processo e que o Tipo de sílica não afeta efetivamente o resultado.
# Assim, podemos criar um modelo linear sem considerar esse fator que pode ser comparado a um modelo 2^3 completo:

# Eliminando o fator TSil
modelo2 <- lm(y ~ Sal*Solv*QSil, data=fatorial2)
summary(modelo2)
# Como esperado, os valores dos efeitos mostram que o fator solvente é o mais significativo nesse experimento.
# Podemos observar que interação entre alguns fatores podem ser significativos. São elas:
#Sal1:Solv1        -0.3750
#Sal1:QSil1        -0.4600
#Solv1:QSil1       -0.7475 

# Como o planejamento 2^4-1 possui Relação geradora D= ABC, sabemos que os fatores principais são confudidos com a interação de 
# 3 fatores, no entanto, normalmente interações entre 3 fatores não constumam ser significativos. Por isso, não iremos considerá-las.
# O problema maior para a relação geradora A = BCD é que temos confundimento entre interações de dois fatores. São elas:
# AD = BC; BD = AC; CD = AB.

# No entanto, observamos pelo gráfico normal e pelos efeitos dos fatores principais que o fator A=TSil (Tipo de sílica) não
# se mostrou significativo. Assim, podemos desconsiderar as interações de dois fatores que envolvem A (Tsil). Então, temos que
# apenas as interações entre dois fatores BC, BD e CD podem ser significativas.

# Diante disso, podemos criar um modelo linear que envolva apenas os fatores principais B, C e D e as interações entre dois fatores
# que não envolvam o fator A = TSil, visto que este não é significativo:

modelo2 <- lm(y ~ Sal+Solv+QSil+Sal*Solv+Sal*QSil+Solv*QSil, data=fatorial2)
summary(modelo2)
# Agora, temos um modelo capaz de gerar o erro padrão.
#Resultado:

#Estimate Std. Error t value Pr(>|t|)   
#Solv1         1.7875     0.0450  39.722  0.01602 * 
#Solv1:QSil1  -0.7475     0.0450 -16.611  0.03828 * 

#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 0.1273 on 1 degrees of freedom
#Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9968 
#F-statistic: 359.2 on 6 and 1 DF,  p-value: 0.04036

# t value = compara diferença entre as médias (t de Student), necessário para gerar o p-valor
# p valor é a probabilidade de que um outro grupo amostral nas mesma condições produzam este mesmo resultado

