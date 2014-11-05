## Simulando p-valor

# número de obs
n <- 10

# data X ~ N(mu, sigma^2)

# valores de mu
vecMu <- c(0, .5, 1, 1.5, 2)

# Número de simulações
nSim <- 10000

# Matriz pra guardar resultados de simulações
x <- matrix(0, nrow=nSim, ncol=n)
pvalor <- matrix(0, nrow=nSim, ncol=length(vecMu))
# hipóteses 
# H0: mu = 0
# H1: mu >= 0

# T = xbar/ s/sqrt(n)

stat_T <- function(x, n) {mean(x)/(sd(x)/sqrt(n))}


# simulação
for ( j in 1:length(vecMu)) {
  for ( i in 1:nSim) {
    x[i,] <- rnorm(n, vecMu[j], 2)
    pvalor[i, j] <- 1 - pt(stat_T(x[i,], n), df=n-1)
  } 
}

head(pvalor)
hist((pvalor[,1]), freq=F)
hist(pvalor[,2], freq=F)
hist(pvalor[,3], freq=F)
hist(pvalor[,4], freq=F)
hist(pvalor[,5], freq=F)






