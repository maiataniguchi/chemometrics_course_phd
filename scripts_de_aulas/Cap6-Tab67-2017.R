fat <- FrF2(nruns= 4 ,nfactors= 2 , blocks= 1 , alias.block.2fis = FALSE , ncenter= 3 , MaxC2 = FALSE , 
                 resolution = NULL ,replications= 1 ,repeat.only= FALSE ,randomize= FALSE ,seed= 5491 , factor.names=list( A=c(-1,
                                                                                                                               1),B=c(-1,1) ) )
summary(fat)
fat <- ccd.augment( fat , alpha= 1.414214 , ncenter=c( 3,0 ) ,randomize= FALSE ,seed= 11706 )
summary(fat)
y <- c(86,85,78,84,90,88,89,81,86,87,80)
fatorial <- add.response(fat,y)
summary(fatorial)

modelo <- lm(y ~ polym(x1, x2, degree=2, raw=TRUE))
summary(modelo)

library(alr3)
pureErrorAnova(mod2)

#cria uma sequ??ncia de valores para as vari??veis x e y
C=seq(-1.5,1.5,0.1)
P=seq(-1.5,1.5,0.1)

#cria uma fun????o
modelo=function(a,b){0.86+0.74*a-0.16*b+0.44*a^2-0.18*a*b}

#cria a matriz de valores para a vari??vel dependente
M=outer(C,P,modelo)

#apresentar os valores de z
M

#alternativamente, poderia ter sido usado z=outer(x,y,model);z

#cria a superficie
persp(C,P,M)

persp(C,P,M,theta = -30,phi = 30,ticktype = "detailed")

#cria uma superf??cie bidimensional
contour(C,P,M,nlevels=12)

#Gr??fico para o rendimento

C=seq(-1.0,1.0,0.1)
P=seq(-1.0,1.0,0.1)

#cria uma fun????o
modelo=function(a,b){11.17+8.07*a-1.47*b+1.28*a^2}

#cria a matriz de valores para a vari??vel dependente
R=outer(C,P,modelo); R

#cria a superf??cie

persp(C,P,R,theta = -30,phi = 30,ticktype = "detailed")

contour(C,P,M,nlevels=12, xlab="C", ylab="P")