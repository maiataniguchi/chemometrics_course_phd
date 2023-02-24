#Exemplo 8.4, Montgomery, p. 336

# Planejamento é 2IV6-2 com resolução de 4, pois E=ABC ??? (E)E=ABC(E) ??? I =ABCE e F=BCD ??? (F)F=BCD(F) ??? I=BCDF

library(FrF2)

fat <- FrF2(16,6, randomize = FALSE, generators=c("ABC", "BCD")) #agora as colunas geradoras foram definidas

# Relação geradora: D=ABC E=AB 
# Nesse planejamento os efeitos principais estão confundidos com interações de 3 fatores.
# A=BCE; B=ACE; B=CDF; C=ABE; C=BDF; D=BCF; E=ABC e F=BCD.


summary(fat)
resp <- c(6,10,32,60,4,15,26,60,8,12,34,60,16,5,37,52)
fat2 <- add.response(fat,resp)
summary(fat2)

MEPlot(fat2) #Aqui notamos que as variaveis C, D, E e F não são significativas.
DanielPlot(fat2) # A e B são significativos bem como a interação AB, AD e ABF
IAPlot(fat2) #As interações BC e BD aparentemente são significativas.

modelo1 <- lm(resp ~ A*B*C*D*E*F, data=fat2) # Não sobra exp. para calcular o grau de liberdade ainda
# por conta de todas as interações realizadas
summary(modelo1)

modelo2 <- lm(resp ~ A*B, data=fat2) 
summary(modelo2)

# Por meio da análise do gráfico normal podemos observar que os efeitos principais B e A foram significativos,
# assim como as interações de dois fatores A:B e A:D e a interação de 3 fatores ABF. Os efeitos significativos
# B, A e a interação AB aumentam a resposta, ou seja, aumentam a contração observada, enquanto que os efeitos
# das interações A:D e A:B:F diminuem a resposta, ou seja, a diminuem a contração observada.
# Como os efeitos principais estão confundidos com interações de 3 fatores que geralmente não são significativas,
# podemos considerar que o aumento da contração observada ao passar do nível inferior (-) para o superior (+)
# dos fatores A e B são referentes a temperatura do molde e a velocidade de prensa, respectivamente.
# O efeito significativo da interação A:D mostra que há uma dimuinuição da contração observada ao passar do
# nível inferior (-) para o superior (+), assim como o objetivo do experimento é a redução da contração,
# os níveis superiores da interação A:D fornecerão uma maior reduçãoda contração. Além disso, nesse planejamento
# A:D está confundido com EF, no entanto, como E e F não foram significativos essa interação provavelmente é
# referente a interação da temperatura do molde (A) e do tempo do ciclo (D).
