# Otimização simplex

B = c(80,21)
W = c(68,0)
N = c(56,21)

# Movimento de reflexãp
# 1) Calcular o ponto P:

P = (B + N)/2 
P

# Calculando o valor de R:

R = P + (P-W)
R

# Movimento de expansão:

S = P + 2*(P-W)
S

# Movimento V:
V = P + 0.5*(P-W)
V

# Movimento T:
T = P - 0.5*(P-W)
T

##################################################################################