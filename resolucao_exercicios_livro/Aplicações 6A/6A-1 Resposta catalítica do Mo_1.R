# Exercício 6A-1
# metodologia de superfície de reposta: técnica de otimização baseada em planejamentos fatoriais
#A metodologia de superfícies de resposta tem duas etapas distintas -
# modelagem e deslocamento-, que são repetidas tantas vezes quantas forem necessárias, com o objetivo de
# atingir uma região ótima da superfície investigada

# x1 e x2 = valores codificados das concentrações de H2SO4 e KI; x3 = concentração da solução osmótica; y = perda de peso
## Podemos digitar o planejamento manualmente:
x1 <- c(-1,+1,-1,+1,-1.4,+1.4,0,0,0,0,0,0,0)
x2 <- c(-1,-1,+1,+1,0,0,-1.4,+1.4,0,0,0,0,0)

library(rsm) # biblioteca para a superfície de resposta
## Ou podemos usar o comando ccd para gerar o planejamento:
des1 <- ccd(~ x1+x2, randomize = FALSE, n0=c(0,5),alpha="rotatable")
des1
#Onde:
#n0=numero de repetiçoes no ponto central
#num de repetiçoes = 0
#num de pontos centrais = 5

## Resposta:
y <- c(0.373,0.497,0.483,0.615,0.308,0.555,0.465,0.628,0.538,0.549,0.536,0.549,0.538)

# Calculando a ANOVA e encontrando a função que melhor se adequa aos dados: 
ajuste <- rsm (y~SO(x1,x2),data=des1)
summary(ajuste)
# Y = 0.541967 + 0.075985 X1 + 0.057601 x2 -0.055320 X1^2+ 0.003353 X2^2+0.002000 X1X2
# R² = 0.9856
#verifica-se que xquad2 e x1:x2  não foram significativos.

#### ANOVA:
#Df   Sum Sq  Mean Sq  F value    Pr(>F)
#FO(x1, x2)   2 0.072080 0.036040 198.1532 6.888e-07
#TWI(x1, x2)  1 0.000016 0.000016   0.0880   0.77538
#PQ(x1, x2)   2 0.021386 0.010693  58.7912 4.205e-05
#Residuals    7 0.001273 0.000182                   
#Lack of fit  3 0.001107 0.000369   8.8928   0.03045
#Pure error   4 0.000166 0.000042  
### FO = Primeira ordem, TWI = interações, PQ = puramente quadrático.
#par(mfrow =c(1,1)) <- usar para inserir mais de um gráfico por pagina
# Gráfico de contorno:curvas de nível
contour(ajuste,~x1+x2, image=TRUE, img.col = terrain.colors(50),xlabs=c("H2SO4","KI"))
### A curva de nível mostra que a região de melhor resposta para KI = +1 w H2SO4 entre 0.5 e 1.0

# Cria a superficie, grafico de superficie
persp(ajuste,~x1+x2, col=rainbow(50),contours = "colors", theta = -20, phi= 20, xlabs=c("x1","x2"),zlab="Resposta")   
# x1 e x2 são independentes e Y a dependente.                               
              