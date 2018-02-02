# Problem B: Dirichlet distribution
library(MASS)
setwd("/home/shomea/m/marcusae/Documents/git/Tma4300-Kode/proj_1")
source("sampleGamma.R")

# ----- Utilities --------#
meanDir <- function(alpha, K){
  sum_alpha = sum(alpha)
  mean_out <-  matrix(0, K, 1)
  for (it in 1:K){
    mean_out[it] <- alpha[it] / sum_alpha
  }  
  return (mean_out)
}

covDir <- function(alpha, K){
  alpha_0 <- sum(alpha)
  cov_out <- matrix(0, K, K)
  print(alpha_0)
  for (i in 1:K) {
    for (j in 1:K){
      if (i != j) {
        cov_out[i,j] <- -(alpha[i] * alpha[j]) / (alpha_0^2 * (alpha_0 + 1))
      }
    }
  }
  return (cov_out)
}

n <- 1000
K <- 3
alpha <- runif(K)*10

zSample <- matrix(0,K,n)
dirSample <- matrix(0,K,n)
dirMean <- matrix(0,K,1)

for (i in 1:n) {
  for (j in 1:K){
    zSample[j, i] <- sampleGamma(alpha[j], 1, 1)
  }
  for (j in 1:K){
    dirSample[j, i] <- zSample[j,i]/(sum(zSample[,i]))
  }
}

for (j in 1:K) {
  dirMean[j] <- mean(dirSample[j,])
}
truehist(dirSample[2,])

# Print true mean 
print("Estimated mean")
print(dirMean)
print("Mean True")
print(meanDir(alpha, K))

