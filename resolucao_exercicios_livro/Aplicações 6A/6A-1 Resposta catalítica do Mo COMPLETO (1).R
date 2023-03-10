# Exerc?cio 6A-1
# metodologia de superf?cie de reposta: t?cnica de otimiza??o baseada em planejamentos fatoriais
#A metodologia de superf?cies de resposta tem duas etapas distintas -
# modelagem e deslocamento-, que s?o repetidas tantas vezes quantas forem necess?rias, com o objetivo de
# atingir uma regi?o ?tima da superf?cie investigada

library(rsm)
# x1 e x2 = valores codificados das concentra??es de H2SO4 e KI; x3 = concentra??o da solu??o osm?tica; y = perda de peso   
x1 <- c(-1,+1,-1,+1,-1.4,+1.4,0,0,0,0,0,0,0)
x2 <- c(-1,-1,+1,+1,0,0,-1.4,+1.4,0,0,0,0,0)
y <- c(0.373,0.497,0.483,0.615,0.308,0.555,0.465,0.628,0.538,0.549,0.536,0.549,0.538)

#podemos digitar os dados, como feito acima e carregar a bibliotema rsm para gerar
#o planejamento. Gerando o planejamento, basta inserir a coluna de respostas.


#n0= (0,5) numero de repeti?oes no ponto central (0) e (5) pontos centrais.
des1 <- ccd(~ x1+x2, randomize = FALSE, n0=c(0,5),alpha="rotatable")
des1
#basta conferir o planejamento e adicionar os dados na mesma ordem do planejamento
#gerado. As vezes os dados podem aparecer trocados de ordem, devemos tomar cuidado
#e inseri-los na ordem correta gerada pelo R. Caso n?o queira explorar a biblioteca
#rsm, basta inserir os dados de x1,x2 e a resposta e carregar tais dados. 
#nessa situa??o, n?o precisa carregar o rsm. 

y <- c(0.373,0.497,0.483,0.615,0.308,0.555,0.465,0.628,0.538,0.549,0.536,0.549,0.538)

#se digitar tudo manualmente
ajuste <- rsm(y~SO(x1,x2),data=des1)
summary(ajuste)

#ou

#se usar o des1
ajuste <- lm(y~SO(x1,x2),data=des1)
summary(ajuste)

#da na msm, qualquer caminho que escolha p ajuste.


#par(mfrow =c(1,1))
contour(ajuste,~x1+x2)

# obtivemos um grafico em curvas de n?vel
contour(ajuste,~x1+x2, image=TRUE) #deixa colorido
contour(ajuste,~x1+x2, image=TRUE, img.col = terrain.colors(50),xlabs=c("H2SO4","KI"))
# olhamos nos dados inseridos (respostas y) e percebemos qual a melhor condi??o no
#gr?fico. Regi?o pr?xima a 0.65
    
persp(ajuste,~x1+x2, col=terrain.colors(50),contours = "colors")               

persp(ajuste,~x1+x2, col=rainbow(50),contours = "colors", theta = -30, phi= 40, xlabs=c("H2SO4","KI"),zlab="Resposta")   
                # x1 e x2 s?o independentes e Y a dependente.                               
                
# nesse caso avaliamos o perfil de modo visual mesmo. As vari?veis codificadas
#poder?o ser convertidas em termos experimentais pela equa??o
vari?vel codificada= VALORREAL-(valor experimental intermedi?rio de determinado par?metro)/varia??o desse par?metro

Ex: 50 - 60 - 70 de temperatura

equa??o fica

x= (y-60)/10