#Exercicio titula??o AAc (apenas com os oito primeios valores)
tit1 <- c(3.91,4.01,3.61,3.83,3.75,3.91,3.82,3.70,3.50,3.77,3.96,3.85,3.67,3.83,3.77,3.51,3.85,4.04,3.74,3.97)
mean(tit1)
sd(tit1)

t.test(tit1)

hist(tit1,
     main = "Distribui??o da [AAc]", xlab = "Concentra??o", breaks=5, border="red", col="pink")

#onde break ? o n?mero de barras do histograma.
# ? poss?vel observar pelo histograma que a maior concentra??o de gra?s de feij?es encontram-se em 0,20 g.
# Al?m disso os pesos dos feij?es seguem uma distribui??o normal, ou seja, ? representado por uma gaussina.

#Frequencia relativa:

curve(dnorm(x,mean = mean(tit1), sd = sd(tit1)),
      add = TRUE, col= "darkgreen", lwd=3 )

#Est?tica: onde col ? a cor escolhida e lwd ? a largura da linha.