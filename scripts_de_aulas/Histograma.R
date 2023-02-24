#Histograma

#Executando apenas uma variável (pesos):

hist(feijoes2$pesos)

#Quando houver mais de uma variável, pode-se executar todas de uma vez utilizando:

attach(feijoes2)
hist(pesos,
     main = "Distribuição do peso dos feijões", xlab = "Pesos", breaks=11, border="red", col="pink")

#onde break é o número de barras do histograma.
# É possível observar pelo histograma que a maior concentração de graõs de feijões encontram-se em 0,20 g.
# Além disso os pesos dos feijões seguem uma distribuição normal, ou seja, é representado por uma gaussina.

#Frequencia relativa:

curve(dnorm(x,mean = mean(pesos), sd = sd(pesos)),
      add = TRUE, col= "darkblue", lwd=2 )

#Estética: onde col é a cor escolhida e lwd é a largura da linha.
#######################################################