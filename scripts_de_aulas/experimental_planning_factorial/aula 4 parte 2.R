dnorm(0.201,0.2,0.015) # (valor, média, desvio) te fornece os dados de densidade de probabilidade (função densidade por intervalo de eixo x) em determinado ponto.

curve(dnorm(x,0.2,0.015),xlim = c(0.12,0.3),ylab = "F(x)", xlab = "x")

ndiv <-c(1,2,3,4,5,6,7,8,9,10)

pdiv = (ndiv-0.5)/10
pdiv

x= qnorm(pdiv,0.2,0.015)
#pnorm percentis (valores de x correspondente a cada probabilidade):(prob,media,desvio da populaçã)
#agora vamos calcular os valores de Z (variável padronizada)

z= (x-0.2)/0.015
z

plot(x,z)

#complemento aula 4 parte 4
#graficos dos efeitos para avaliar o que é significativo qdo n temos replicata.

ndiv <-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)

pdiv = (ndiv-0.5)/15
pdiv

z=qnorm(pdiv,0,1) #0,1 corresponde a média e desvio padrão, que corresponde a dist. normal
z

efeitos <-c(-14.125,-8.625,-0.625,-0.625,-0.625,-0.125,0.375,0.375,0.375,0.875,0.875,0.875,0.875,8.875,22.875)

#organizados do menos negativo para os mais positivos. Esses são os efeitos multiplicados por 2. 
#multiplica por dois pela definição do efeito

plot(efeitos,z)
#fornece dados de quem é significativo mesmo não tendo replicata. Os pontos fora da tendencia mostra os valores significativos.