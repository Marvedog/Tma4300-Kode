# -------------- Importance sampling -----------------# 

computeWeights <- function(alpha_new, alpha_prop, q) {
	weights <- matrix(0, 4, 1)
	for (i in 1:4) {
		weights[i] <- gamma(alpha_new[i])
	}
	return weights
}

importanceSampling <- function(prior, alpha_new, alpha_prop) {
	# Calculate priors

}