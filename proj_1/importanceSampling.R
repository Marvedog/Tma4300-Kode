# -------------- Importance sampling -----------------# 
setwd("/home/shomea/m/marcusae/Documents/git/Tma4300-Kode/proj_1")
source("sampleMultinomial.R")
source("sampleDirichlet.R")
computeWeights <- function(alpha_new, alpha_prop, x, q) {
	weights <- matrix(0, 4, 1)
	k <- length(q)
	# Using log scale
	weights <- lgamma(sum(alpha_prop + x)) - lgamma(sum(alpha_new + x)) +
	       	   sum(lgamma(alpha_new + x) - lgamma(alpha_prop + z)) + 
		   sum(((alpha_new - alpha_prop) * q)[-K]) +
		   (alpha_new[K] - alpha_prop[K]) * log(1 - sum(q[-K])) 
	# Return in regular scale
	return (exp(weights))
}

importanceSampling <- function(prior, m, alpha_new, alpha_prop) {

	# Calculate priors
	student_distr <- sampleMultinomial(prior, m)

	# Calculate previous q_i
	q_prop <- sampleDirichlet(alpha_prop, 4) 	

	# Compute weighted average
	weights <- computeWeights(alpha_new, alpha_prop, student_distr, q_prop)
	
	return (weights*prior/sum(weights))
}

# Test functionality 
