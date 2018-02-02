# Sample from generic gamma distribution 
setwd("/home/shomea/m/marcusae/Documents/git/Tma4300-Kode/proj_1")
source("sampleGamma.R")

alpha <- runif(1)
beta <- 1
n = 1

xSample <- sampleGamma(alpha, beta, n)

hist(xSample, xlim = range(0:200))
