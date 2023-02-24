#Construindo uma tabela de dados:

x=c(0.1188,0.2673,0.1795,0.2369,0.1826,0.186,0.2045)
y=c(0.108,0.214,0.143,0.195,0.148,0.144,0.174)
y2=c(0.0141,0.0714,0.0322,0.0561,0.0333,0.0346,0.0418)

#Construindo um gráfico:

plot(x,y)

#Calculando a covariância:

cov(x,y)

#Calculando a correlação (mais utilizado para avaliar a relação entre variáveis):

cor(x,y)

#Testando com outro grupo de dados (y2):

plot(x,y2)
cov(x,y2)
cor(x,y2)
