library(mixexp)
#lc=lower constraints, uc=upper constraints
help("Xvert")
des = Xvert(3,lc=c(0.10,0.10,0.10),uc=c(0.8,0.8,0.8), pseudo=F, ndm=1)
des

#Aumentando o número de pontos:
des2 = Fillv(3,des)
des2
DesignPoints(des2)
