# TMA4300 
# Exercise 1
setwd("/home/shomea/g/ginama/V2018/git/Tma4300-Kode/proj_1")
source("sampleGammaRejection.R")
# PROBLEM A, Part 1

# Libraries
library(MASS)
library(ggplot2)

# Initialize
n <- 100000
alpha <- 0.5
x <- sort(runif(n,0.001,8))

# #------Probability integral transform--------#

# Sample
xsample2 <- gsample(n, alpha) 


# Check sampling
gfunc <- matrix(0,n,1)

for (i in 1:n){
  if(x[i] <= 0){
    gfunc[i] <- 0
  }
  else if (x[i] < 1) {
    gfunc[i] <- ((exp(1)*alpha)/(exp(1)+alpha))*(x[i]^(alpha-1))
  }
  else {
    gfunc[i] <- ((exp(1)*alpha)/(exp(1)+alpha))*exp(-x[i])
  }
}

# Plot
dataf <- data.frame(x, gfunc, xsample2)
# g <- ggplot(dataf)
# g <- g + geom_histogram(aes(xsample2, y = ..density..), binwidth = 0.1, col = "black", fill = "cyan")
# g <- g + geom_line(aes(x, gfunc), col = "red")
# g <- g + coord_cartesian(xlim = range(0:5)) + labs("hei")
# g 
truehist(xsample2, xlab = "Samples", main = "Probability integral transform", xlim = range(0:4))
lines(x,gfunc, col = "red", lwd = 3)


<<<<<<< HEAD

# #-------- Rejection sampling----------#

# Sample
xsample <- sampleGammaRejection(alpha, n)


#Check sampling
gammaf <- function(alpha,x){
  res <- (1/gamma(alpha))*(x^(alpha-1))*exp(-x)
  return(res)
}
y <- gammaf(alpha, x)
check <- data.frame(mean = mean(xsample), truemean = alpha, var = var(xsample), truevar = alpha)
print(check)

#Plot
truehist(xsample, main = "Rejection sampling", xlim = range(0:3), col = "red")
lines(x,y, col = "red", lwd = 2)



