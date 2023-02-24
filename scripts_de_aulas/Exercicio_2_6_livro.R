##Exemplo 2.4 (livro): Use os dados da Tabela 2.3 para confirmar que 54,3% dos caroços observados têm peso
# entre 0,18 e 0,24 g.


#Opção 1:

r <- hist(pesos,
          main = "Distribuição do peso dos feijões", xlab = "Pesos", breaks=11, border="red", col="pink")

attach(feijoes2)

h=hist(pesos)

c=h[[2]] #refere-se aos counts
b=h[[1]] #refere=se aos breaks

# Para chamar o elemento da lista deve-se usar "[[]]", ou seja, cria-se um vetor.

N =  length(b) #varredura dos valores entre 0,18-0,24.
s = 0 #será a soma
for (i in 1:N)
{
  x = b[i]
  if (x>0.18 & x<0.24)
  {
    s = s + c[i]
  }
  
}

f = s/sum(c)

#Opção 2:

y>0.18 & y<0.24
F = sum(c[L])/sum(c)
y=b
L = y>0.18 & y<0.24
F = sum(c[L])/sum(c)
L = b>0.18 & b<0.24
F = sum(c[L])/sum(c)
c[L]
sum(c[L])