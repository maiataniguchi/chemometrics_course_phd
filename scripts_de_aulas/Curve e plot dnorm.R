#Criando um planejamento fatorial 2^4 (16 experimentos) de ordem aleatoria:
# 2 níveis (variação de cada fator,i.e., diferentes temperaturas)
# 4 fatores (neste caso serão a temperatura T; catalisador C; concentração M e pH)

# Fornecendo os valores de x, média e desvpad a função retorna o valor da densidade de probabilidade

dnorm(0.201,0.2,0.015)

curve(dnorm(x,0.2,0.015),xlim = c(0.12,0.3),ylab = "F(x)", xlab = "x")

ndiv <-c(1,2,3,4,5,6,7,8,9,10)

pdiv = (ndiv-0.5)/10
pdiv

#pvid= pontos centrais de cada uma das dez partes de densidade de probabilidade.

#Percentis= qual o valor da fção de densi. de probabl. correspondente aos pontos centrais
#da fção de probabilidade)

qnorm(pdiv,0.2,0.015)
# qnorm percentis = qnorm(prob,média,desvio)
# Resultado: [1] 0.1753272 0.1844535 0.1898827 0.1942202 0.1981151 0.2018849 0.2057798 0.2101173 0.2155465 0.2246728
# ou seja, a esquerda do grafico 


x=qnorm(pdiv,0.2,0.015)

z=(x-0.2)/0.015
z

plot(x,z)


