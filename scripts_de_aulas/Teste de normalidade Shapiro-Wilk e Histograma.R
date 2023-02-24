## Teste de normalidade (Shapiro-Wilk)


## Dados 
branco <- c(0.409, 0.394, 0.406, 0.346)
canal1 <- c(0.401, 0.399, 0.431, 0.371)
canal2 <- c(0.439, 0.409, 0.401, 0.388)
canal3 <- c(0.650, 0.647, 0.654, 0.678)
canal4 <- c(0.808, 0.919, 0.901, 0.933)
canal5 <- c(0.931, 0.873, 0.915, 0.910)

## 0.409, 0.401, 0.439, 0.650, 0.808, 0.931,
## 0.394, 0.399, 0.409, 0.647, 0.919, 0.873,
## 0.406, 0.431, 0.401, 0.654, 0.901, 0.915,
## 0.346, 0.371, 0.388, 0.678, 0.933, 0.910

#  Teste de normalidade para o vetor A
shapiro.test(branco)
shapiro.test(canal1)
shapiro.test(canal2)
shapiro.test(canal3)
shapiro.test(canal4)
shapiro.test(canal5)

# Análise Gráfica: Histogramas:

hist(branco)
hist(canal1)
hist(canal2)
hist(canal3)
hist(canal4)
hist(canal5)

# Histogram with density plot

library(ggplot2)

hist(canal3, density=20, prob=TRUE, 
     main="Histogram with normal curve")
curve(dnorm(x, mean=m, sd=std), add=TRUE)

hist(beaver1$temp, # histogram
     col="peachpuff", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "temp",
     main = "Beaver #1")
lines(density(beaver1$temp), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")