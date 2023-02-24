#Comparação de médias
arrhenius <- c(3.77,3.85,4.07,4.83,5.05)
berzelius <-c(3.62,3.69,4.10,4.70,4.89)

#Médias
mean(arrhenius)
mean(berzelius)

#Desvio padrão
sd(arrhenius)
sd(berzelius)

#Comparação de medias (padrão, 95%)
t.test(arrhenius,berzelius) # intervalo de segurança para a amostras independentes (não pareadas)

#Comparação entre médias com emparelhamento
t.test(arrhenius,berzelius,paired = T)


# Para saber se existe variação com relação a técnica de Arrhenius e Berzélius,
# devemos comparar amostras do mesmo lote (blocagem), isto é, as duas análises do lote 1 (3.77 e 3.62),
# lote 2 (3.85 e 3.69) e assim por diante. Faremos isso para não gerar conflito de efeitos relacionados as
# variações entre os lotes e as variações entre as médias. Se Arrhenius e Berzelius tiverem a mesma técnica,
# o intervalo de confiança não passará pelo zero. 

# Como podemos notar, o intervalo vai de um valor negativo para um valor positivo, passando por zero.
# Essa situação nos permite dizer que as médias são iguais e que não existe diferença significativa entre
# as técnicas.
