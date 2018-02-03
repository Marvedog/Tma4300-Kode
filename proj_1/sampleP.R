setwd("/home/shomea/m/marcusae/Documents/git/Tma4300-Kode/proj_1")
source("importanceSample.R")

calcP <- function(N, K) {
  temp <- 0
  for (i in 1:length(N)) {
    temp <- temp + choose(K[i], N[i]) * factorial(N[i])/factorial(K[i])
  }
  return (1 - temp)
}

samplePosterior <- function(p, students, draws, monthInSeason) {
  0
  post <- matrix(0, draws, 1)
  out <- matrix(1, length(p),1)
    
  # For efficiency
  p <- sort(p)
  
  for (i in 1:draws){
    out <- importanceSample(p, students)
    post[i] <- calcP(out, monthInSeason)
  }
  
  return (post)
}

computePosteriorMeans <- function(p, students, draws, monthInSeason) {
  
  post <- samplePosterior(p, students, draws, monthInSeason)
  
  return (mean(post))
}