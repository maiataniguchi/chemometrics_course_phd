# 4A.2 Termogravimetria do oxalato de cálcio

library(FrF2)

# Inicialmente, precisamos avaliar se os sinais codificados gerados conferem com o previsto,
# considerando a relação geradora

fatorial1 <- FrF2(nruns = 8, nfactors=5, replications = 1,
                  randomize=FALSE, generators=c("ABC", "AB"))
summary(fatorial1)
ponto <- c(726.4,695.4,734.7,738.4,780.8,768.9,822.8,856.1)
fatorial2 <- add.response(fatorial1,ponto)
summary(fatorial2)

# Relação geradora : D=ABC E=AB 
# Estrutura de confundimento principal: A=BE; B=AE; C=DE;  D=CE; E=AB=CD
# Isso ocorre porque I = ABCD = ABE = CDE. Para a variável A, temos:
# A = BCD = BE = ACDE

modelo1 <- lm(ponto ~ A*B*C*D*E, data=fatorial2)
summary(modelo1)

MEPlot(modelo1) # Nota-se que o fluxo foi uma variável pouco significativa na decomposição térmica
DanielPlot(modelo1)
# pode-se verificar que somente a taxa de aquecimento exerceu efeito estatisticamente significativo.
# Pelo fato do fluxo e tipo de cadinho não exercer efeito apreciável, os mesmos foram
# desconsiderados do modelo linear.

modelo2 <- lm(ponto ~ B*C*E, data=fatorial2)
summary(modelo2)

modelo3 <- lm(ponto ~ B+C+E+B*C+B*E+C*E, data=fatorial2)
summary(modelo3)

# Apenas o fator C (Taxa de aquecimento) apresentou ser significativo ao nível de 5%,
# A taxa de aquecimento, única variável com significância, mostrou que existe uma probabilidade de aproximadamente 10% do efeito ser ocasionado por erros aleatórios. 
# Taxa de aquecimento de  50 min-1 ( nível superior) para obter melhor decomposicao termica
 