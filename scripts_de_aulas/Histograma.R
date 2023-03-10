#Histograma

#Executando apenas uma vari?vel (pesos):

hist(feijoes2$pesos)

#Quando houver mais de uma vari?vel, pode-se executar todas de uma vez utilizando:

attach(feijoes2)
hist(pesos,
     main = "Distribui??o do peso dos feij?es", xlab = "Pesos", breaks=11, border="red", col="pink")

#onde break ? o n?mero de barras do histograma.
# ? poss?vel observar pelo histograma que a maior concentra??o de gra?s de feij?es encontram-se em 0,20 g.
# Al?m disso os pesos dos feij?es seguem uma distribui??o normal, ou seja, ? representado por uma gaussina.

#Frequencia relativa:

curve(dnorm(x,mean = mean(pesos), sd = sd(pesos)),
      add = TRUE, col= "darkblue", lwd=2 )

#Est?tica: onde col ? a cor escolhida e lwd ? a largura da linha.
#######################################################