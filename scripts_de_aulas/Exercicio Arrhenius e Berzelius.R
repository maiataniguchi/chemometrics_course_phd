#Compara??o de m?dias
arrhenius <- c(3.77,3.85,4.07,4.83,5.05)
berzelius <-c(3.62,3.69,4.10,4.70,4.89)

#M?dias
mean(arrhenius)
mean(berzelius)

#Desvio padr?o
sd(arrhenius)
sd(berzelius)

#Compara??o de medias (padr?o, 95%)
t.test(arrhenius,berzelius) # intervalo de seguran?a para a amostras independentes (n?o pareadas)

#Compara??o entre m?dias com emparelhamento
t.test(arrhenius,berzelius,paired = T)


# Para saber se existe varia??o com rela??o a t?cnica de Arrhenius e Berz?lius,
# devemos comparar amostras do mesmo lote (blocagem), isto ?, as duas an?lises do lote 1 (3.77 e 3.62),
# lote 2 (3.85 e 3.69) e assim por diante. Faremos isso para n?o gerar conflito de efeitos relacionados as
# varia??es entre os lotes e as varia??es entre as m?dias. Se Arrhenius e Berzelius tiverem a mesma t?cnica,
# o intervalo de confian?a n?o passar? pelo zero. 

# Como podemos notar, o intervalo vai de um valor negativo para um valor positivo, passando por zero.
# Essa situa??o nos permite dizer que as m?dias s?o iguais e que n?o existe diferen?a significativa entre
# as t?cnicas.
