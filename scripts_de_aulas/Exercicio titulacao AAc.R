#Exercicio titulação AAc (apenas com os oito primeios valores)
tit1 <- c(3.91,4.01,3.61,3.83,3.75,3.91,3.82,3.70,3.50,3.77,3.96,3.85,3.67,3.83,3.77,3.51,3.85,4.04,3.74,3.97)
mean(tit1)
sd(tit1)

t.test(tit1)

hist(tit1,
     main = "Distribuição da [AAc]", xlab = "Concentração", breaks=5, border="red", col="pink")

#onde break é o número de barras do histograma.
# É possível observar pelo histograma que a maior concentração de graõs de feijões encontram-se em 0,20 g.
# Além disso os pesos dos feijões seguem uma distribuição normal, ou seja, é representado por uma gaussina.

#Frequencia relativa:

curve(dnorm(x,mean = mean(tit1), sd = sd(tit1)),
      add = TRUE, col= "darkgreen", lwd=3 )

#Estética: onde col é a cor escolhida e lwd é a largura da linha.