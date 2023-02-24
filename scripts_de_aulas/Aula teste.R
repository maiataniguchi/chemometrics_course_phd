a=c(2,3,5,6,7,8) #lista de números para o exemplo
b=c(3,4,7,9,1,2) #inserir comentario aqui
c=a+b
c

#Probabilidades. Funçao pnorm(valor,media,desvio) = dá o valor da integral da parte esquerda do gráfico
#Exemplo:
#Primeira opção
pnorm(75,72,6)

1-pnorm(75,72,6)
#Segunda opção
pnorm(75,72,6,lower.tail = F)

#Para um intervalo:
pnorm(74,72,6)-pnorm(68,72,6)


