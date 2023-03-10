### Testes de normalidade
# Pacote com alguns testes
library(nortest)
# Dados. Velocidades de carros em um tese (em mph)

A <- c(0.409, 0.401, 0.439, 0.650, 0.808, 0.931,
       0.394, 0.399, 0.409, 0.647, 0.919, 0.873,
       0.406, 0.431, 0.401, 0.654, 0.901, 0.915,
       0.346, 0.371, 0.388, 0.678, 0.933, 0.910)


## 1. Testes de normalidade
# Gr?fico de probabilidade (QQ)
qqnorm(A, main = "", xlab = "Quantis te?ricos N(0,1)", pch = 20,
       ylab = "Velocidade (km/m)")
qqline(dados, lty = 2, col = "red")

# Estimativas dos par?metros
xb <- mean(A) # mu
sx <- sd(A) # sigma
cat("\n M?dia amostral =", xb, "\n Desvio padr?o amostral =", sx)

# Testes
t1 <- ks.test(A, "pnorm", xb, sx) # KS
t2 <- lillie.test(A) # Lilliefors
t3 <- cvm.test(A) # Cram?r-von Mises
t4 <- shapiro.test(A) # Shapiro-Wilk
t5 <- sf.test(A) # Shapiro-Francia
t6 <- ad.test(A) # Anderson-Darling
# Tabela de resultados
testes <- c(t1$method, t2$method, t3$method, t4$method, t5$method,
            t6$method)
estt <- as.numeric(c(t1$statistic, t2$statistic, t3$statistic,
                     t4$statistic, t5$statistic, t6$statistic))
valorp <- c(t1$p.value, t2$p.value, t3$p.value, t4$p.value, t5$p.value,
            t6$p.value)
resultados <- cbind(estt, valorp)
rownames(resultados) <- testes
colnames(resultados) <- c("Estat?stica", "p")
print(resultados, digits = 4)

## 2. Gr?fico QQ com envelope
nsim <- 100 # N?mero de simula??es
conf <- 0.95 # Coef. de confian?a
# Dados simulados ~ normal
dadossim <- matrix(rnorm(n * nsim, mean = xb, sd = sx), nrow = n)
dadossim <- apply(dadossim, 2, sort)
# Limites da banda e m?dia
infsup <- apply(dadossim, 1, quantile, probs = c((1 - conf) / 2,
                                                 (1 + conf) / 2))
xbsim <- rowMeans(dadossim)
# Gr?fico
faixay <- range(dados, dadossim)
qq0 <- qqnorm(dados, main = "", xlab = "Quantis te?ricos N(0,1)", pch = 20,
              ylab = "Velocidade (km/m)", ylim = faixay)
eixox <- sort(qq0$x)
lines(eixox, xbsim)
lines(eixox, infsup[1,])
lines(eixox, infsup[2,])
