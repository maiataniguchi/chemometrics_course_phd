# Exercício 6A-1
# metodologia de superfície de reposta: técnica de otimização baseada em planejamentos fatoriais
#A metodologia de superfícies de resposta tem duas etapas distintas -
# modelagem e deslocamento-, que são repetidas tantas vezes quantas forem necessárias, com o objetivo de
# atingir uma região ótima da superfície investigada

library(rsm)
# x1 e x2 = valores codificados das concentrações de H2SO4 e KI; x3 = concentração da solução osmótica; y = perda de peso   
x1 <- c(-1,+1,-1,+1,-1.4,+1.4,0,0,0,0,0,0,0)
x2 <- c(-1,-1,+1,+1,0,0,-1.4,+1.4,0,0,0,0,0)
y <- c(0.373,0.497,0.483,0.615,0.308,0.555,0.465,0.628,0.538,0.549,0.536,0.549,0.538)

#podemos digitar os dados, como feito acima e carregar a bibliotema rsm para gerar
#o planejamento. Gerando o planejamento, basta inserir a coluna de respostas.


#n0= (0,5) numero de repetiçoes no ponto central (0) e (5) pontos centrais.
des1 <- ccd(~ x1+x2, randomize = FALSE, n0=c(0,5),alpha="rotatable")
des1
#basta conferir o planejamento e adicionar os dados na mesma ordem do planejamento
#gerado. As vezes os dados podem aparecer trocados de ordem, devemos tomar cuidado
#e inseri-los na ordem correta gerada pelo R. Caso não queira explorar a biblioteca
#rsm, basta inserir os dados de x1,x2 e a resposta e carregar tais dados. 
#nessa situação, não precisa carregar o rsm. 

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

# obtivemos um grafico em curvas de nível
contour(ajuste,~x1+x2, image=TRUE) #deixa colorido
contour(ajuste,~x1+x2, image=TRUE, img.col = terrain.colors(50),xlabs=c("H2SO4","KI"))
# olhamos nos dados inseridos (respostas y) e percebemos qual a melhor condição no
#gráfico. Região próxima a 0.65
    
persp(ajuste,~x1+x2, col=terrain.colors(50),contours = "colors")               

persp(ajuste,~x1+x2, col=rainbow(50),contours = "colors", theta = -30, phi= 40, xlabs=c("H2SO4","KI"),zlab="Resposta")   
                # x1 e x2 são independentes e Y a dependente.                               
                
# nesse caso avaliamos o perfil de modo visual mesmo. As variáveis codificadas
#poderão ser convertidas em termos experimentais pela equação
variável codificada= VALORREAL-(valor experimental intermediário de determinado parâmetro)/variação desse parâmetro

Ex: 50 - 60 - 70 de temperatura

equação fica

x= (y-60)/10