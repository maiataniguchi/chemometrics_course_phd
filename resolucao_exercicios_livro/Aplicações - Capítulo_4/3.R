# 4A.3 An?lise cromatogr?fica de gases
library(FrF2)
fatorial1 <- FrF2(nruns = 8, nfactors=5, replications = 1,
                  randomize=FALSE, generators=c("AB", "BC"))

summary(fatorial1)
sinal <- c(49,21,15,1,42,2,25,32)
fatorial2 <- add.response(fatorial1,sinal)
summary(fatorial2)

# Rela??o geradora: D=AB E=BC
# Estrutura de confundimento
# A=BD; B=AD=CE; C=BE; D=AB; E=BC
# AC=DE AE=CD

# Criando o modelo linear:
modelo1 <- lm(sinal ~ A*B*C*D*E, data=fatorial2)
summary(modelo1)

DanielPlot(modelo1) # O gr?fico mostra nenhum fator como significativo
MEPlot(modelo1) # Mostra que o fator C ? o menos significativo e pelos valores dos efeitos temos que
# A1=-9.375; D1=7.625 e E1=8.375 s?o mais significativos.

# Excluindo C do modelo:
modelo2 <- lm(sinal ~ A*B*D*E, data=fatorial2)
summary(modelo2)
# No livro o autor s? discute os efeitos principais sobre o MEPlot

modelo3 <- lm(sinal ~ A+B+D+E+A*E+D*E, data=fatorial2)
summary(modelo3)
# exclui o B com E pq se confundia com o fator C.

# A unica informa??o que podemos afirmar nesse exercicio ? que o fator E (Pressuriza??o da amostra)
# deve ser trabalho em seu nivel superior, ou seja, 11 psi.