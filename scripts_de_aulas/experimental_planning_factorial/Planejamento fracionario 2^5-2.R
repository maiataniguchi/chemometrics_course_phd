#Fatorial fracionario de resolução 3
# 5 fatores em 8 ensaios 2^5-2

library(FrF2)
fatorial1 <- FrF2(nruns = 8, nfactors=5, replications = 1,
                  randomize=FALSE)
summary(fatorial1) #Colunas D e E não batem com o dos slides.Os geradores foram D=AB e E=AC (resolução 3)
resp <- c(52,92,198,113,122,76,189,286)
fatorial2 <- add.response(fatorial1,resp)
summary(fatorial2)


fat <- FrF2(8,5, randomize = FALSE, generators=c("ABC", "AB")) #agora as colunas geradoras foram definidas
#[1] D=ABC E=AB 
summary(fat)
resp <- c(52,92,198,113,122,76,189,286)
fat2 <- add.response(fat,resp)
summary(fat2)

MEPlot(fat2) #Aqui  percebemos que os fatores A e E não são significativas. Logo, as interações que envolvem
# essas variaveis podem ser desconsideradas.

IAPlot(fat2) #As interações BC e BD aparentemente são significativas.

modelo1 <- lm(resp ~ A*B*C*D*E, data=fat2)
summary(modelo1)
# São significativos: B1 = 55.50, C1 = 27.25, D1 = 33.50, A1:C1 = 12.00 , B1:C1 = 13.75
# Logo, um novo modelo sem as variáveis A e E é feito para separar essas interações confundidas:

modelo2 <- lm(resp ~ B*C*D, data = fat2 )
summary(modelo2)
#Sabendo que a interação entre 3 fatores não são significativas, um novo modelo excluindo essas interações
#(B1:C1:D1) devem ser feitos:

modelo3 <- lm(resp ~ B+C+D+B*C+B*D+C*D, data=fat2)
summary(modelo3)

#Agora, como resposta temos um planejamento 2^3 completo com estimativa de erro:
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)   
#(Intercept)   141.00       0.75  188.00  0.00339 **
#  B1             55.50       0.75   74.00  0.00860 **
#  C1             27.25       0.75   36.33  0.01752 * 
#  D1             33.50       0.75   44.67  0.01425 * 
#  B1:C1          13.75       0.75   18.33  0.03469 * 
#  B1:D1          12.00       0.75   16.00  0.03974 * 
#  C1:D1           2.25       0.75    3.00  0.20483   
# Conclusão: CD não é significativo.