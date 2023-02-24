library(FrF2)
fatorial1 <- FrF2(nruns = 16, nfactors=5, replications = 1,
                  randomize=FALSE)

summary(fatorial1)
resp <- c(52,61,124,113,85,66,185,192,98,86,201,194,122,139,289,286)
fatorial2 <- add.response(fatorial1,resp)
summary(fatorial2)

# Alias structure: [[1]]
# [1] no aliasing among main effects and 2fis

MEPlot(fatorial2) # Os fatores A e E são poucos significtivos aparentemente
DanielPlot(fatorial2)

modelo1 <- lm(resp ~ A*B*C*D*E, data=fatorial2)
summary(modelo1)

# Ajustando o modelo sem a variavel A (que nao é signif):

modelo2 <- lm(resp ~ B*C*D*E, data=fatorial2) # Não sobra exp. para calcular o grau de liberdade ainda
# por conta de todas as interações realizadas
summary(modelo2)

modelo3 <- lm(resp ~ B + C + D + E + B*C + B*D + C*D + B*E + C*E + D*E, data=fatorial2) # Não sobra exp. para calcular o grau de liberdade ainda
# por conta de todas as interações realizadas
summary(modelo3)

MEPlot(fatorial2)
