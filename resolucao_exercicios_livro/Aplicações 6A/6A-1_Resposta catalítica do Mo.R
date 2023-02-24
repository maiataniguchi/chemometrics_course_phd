# Exercício 6A-1
# metodologia de superfície de reposta: técnica de otimização baseada em planejamentos fatoriais
#A metodologia de superfícies de resposta tem duas etapas distintas -
# modelagem e deslocamento-, que são repetidas tantas vezes quantas forem necessárias, com o objetivo de
# atingir uma região ótima da superfície investigada

# x1 e x2 = valores codificados das concentrações de H2SO4 e KI; x3 = concentração da solução osmótica; y = perda de peso   
x1 <- c(-1,+1,-1,+1,-1.4,+1.4,0,0,0,0,0,0,0)
x2 <- c(-1,-1,+1,+1,0,0,-1.4,+1.4,0,0,0,0,0)
y <- c(0.373,0.497,0.483,0.615,0.308,0.555,0.465,0.628,0.538,0.549,0.536,0.549,0.538)

#ajustar o modelo linear
modelo1 <- lm(y ~ x1 + x2)
summary(modelo1)
# modelo linear ajustado: y = 0.51031 + 0.07598X1 + 0.05760x2

# ANOVA - modelo linear
library(alr3)
pureErrorAnova(modelo1)
# Resultado ANOVA
#Df   Sum Sq  Mean Sq  F value    Pr(>F)    
#x1            1 0.045728 0.045728 1101.872 4.912e-06 ***
#x2            1 0.026278 0.026278  633.195 1.481e-05 ***
#Residuals    10 0.022750 0.002275                       
#Lack of fit  6 0.022584 0.003764   90.697 0.0003164 ***
# Pure Error   4 0.000166 0.000042                       
# MQfaj/MQep = 89.62
# % da variação explicada = 0.7599
# % máxima de variação explicável = SQT - SQep /SQT =

# gráfico dos resíduos modelo linear
residuos <- resid(modelo1)
plot(y,residuos,ylim = c(-0.2,0.2))
abline(0,0)
### Aparenta distribuição aleatória

####################################################################
#### Modelo quadrático #####
xquad1<-x1^2
xquad2<-x2^2
modelo2<-lm(y~x1+x2+xquad1+xquad2+x1*x2)
summary(modelo2)
# Y = 0.541967 + 0.075985 + 0.057601
# R² = 0.9855

#ANOVA
library(alr3)
pureErrorAnova(modelo2)
### Aparenta distribuição aleatória

# Gráfico de dispersão quadrático
residuos <- resid(modelo2)
plot(y,residuos, ylim = c(-0.1,0.1))
abline(0,0)

#####################################################################
#### Superfíce de resposta #####