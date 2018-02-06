# -------------- Importance sampling -----------------# 
library(MCMCpack)
setwd("/home/shomea/m/marcusae/Documents/git/Tma4300-Kode/proj_1")
source("sampleMultinomial.R")
source("exercise1dirichlet.R")
source("sampleP.R")
computeWeights <- function(alpha_new, alpha_prop, x, q) {
	weights_1 <- rep(0, 4)
	weights_2 <- rep(0, 4)
	weights <- rep(0, 4)
	
	k <- length(q)
  for (i in 1:k) {
    weights_1[i] <- ddirichlet(q, alpha_new + x)
    weights_2[i] <- ddirichlet(q, alpha_prop + x)
    weights[i] <- weights[1] / weights[2]
  }
	
	return (weights)
}

importanceSampling <- function(prior, students, draws, daysInSeason, alpha_new, alpha_prop) {

	# Calculate priors
	student_distr <- sampleMultinomial(prior, students)

	q_prop <- matrix(0, 4, draws)
	q_prop_mean <- matrix(0,4,1)
	
	# Calculate previous q_i
	for (i in 1:draws){
	  q_prop[,i] <- sampleDirichlet(alpha_prop, 4) 	
	}
	for (i in 1:4) {
	  q_prop_mean[i] <- mean(q_prop[i,]) 
	}
	print(q_prop_mean)
	# Compute weighted average
	weights <- computeWeights(alpha_new, alpha_prop, student_distr, q_prop_mean)
	post <- samplePosterior(prior, students, draws, daysInSeason)

	return (weights*post/sum(weights))
}

# Test functionality 
alpha_prop <- matrix(c(0.5, 0.5, 0.5, 0.5))
alpha_new <- matrix(c(50, 50, 50, 50))

students <- 35
draws <- 10

daysInSeason <- matrix(c(92, 92, 91, 90))

prior <- matrix(c(92/365, 92/365, 91/365, 90/365))

student_distr <- importanceSampling(prior, students, draws, daysInSeason, alpha_new, alpha_prop)
print(student_distr)
