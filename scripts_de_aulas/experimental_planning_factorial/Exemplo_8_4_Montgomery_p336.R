#Exemplo 8.4, Montgomery, p. 336

# Planejamento ? 2IV6-2 com resolu??o de 4, pois E=ABC ??? (E)E=ABC(E) ??? I =ABCE e F=BCD ??? (F)F=BCD(F) ??? I=BCDF

library(FrF2)

fat <- FrF2(16,6, randomize = FALSE, generators=c("ABC", "BCD")) #agora as colunas geradoras foram definidas

# Rela??o geradora: D=ABC E=AB 
# Nesse planejamento os efeitos principais est?o confundidos com intera??es de 3 fatores.
# A=BCE; B=ACE; B=CDF; C=ABE; C=BDF; D=BCF; E=ABC e F=BCD.


summary(fat)
resp <- c(6,10,32,60,4,15,26,60,8,12,34,60,16,5,37,52)
fat2 <- add.response(fat,resp)
summary(fat2)

MEPlot(fat2) #Aqui notamos que as variaveis C, D, E e F n?o s?o significativas.
DanielPlot(fat2) # A e B s?o significativos bem como a intera??o AB, AD e ABF
IAPlot(fat2) #As intera??es BC e BD aparentemente s?o significativas.

modelo1 <- lm(resp ~ A*B*C*D*E*F, data=fat2) # N?o sobra exp. para calcular o grau de liberdade ainda
# por conta de todas as intera??es realizadas
summary(modelo1)

modelo2 <- lm(resp ~ A*B, data=fat2) 
summary(modelo2)

# Por meio da an?lise do gr?fico normal podemos observar que os efeitos principais B e A foram significativos,
# assim como as intera??es de dois fatores A:B e A:D e a intera??o de 3 fatores ABF. Os efeitos significativos
# B, A e a intera??o AB aumentam a resposta, ou seja, aumentam a contra??o observada, enquanto que os efeitos
# das intera??es A:D e A:B:F diminuem a resposta, ou seja, a diminuem a contra??o observada.
# Como os efeitos principais est?o confundidos com intera??es de 3 fatores que geralmente n?o s?o significativas,
# podemos considerar que o aumento da contra??o observada ao passar do n?vel inferior (-) para o superior (+)
# dos fatores A e B s?o referentes a temperatura do molde e a velocidade de prensa, respectivamente.
# O efeito significativo da intera??o A:D mostra que h? uma dimuinui??o da contra??o observada ao passar do
# n?vel inferior (-) para o superior (+), assim como o objetivo do experimento ? a redu??o da contra??o,
# os n?veis superiores da intera??o A:D fornecer?o uma maior redu??oda contra??o. Al?m disso, nesse planejamento
# A:D est? confundido com EF, no entanto, como E e F n?o foram significativos essa intera??o provavelmente ?
# referente a intera??o da temperatura do molde (A) e do tempo do ciclo (D).
