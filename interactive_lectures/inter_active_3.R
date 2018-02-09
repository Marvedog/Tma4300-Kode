simMC <- function(transition_matrix, time_steps) {
    current_state <- 0
    sample <- c(current_state)
    for (i in 1:time_steps) {
        
        sample <- c(sample, rbinom(1, 1, transition_matrix[current_state+1, current_state+1]))
        current_state <- sample[i+1]
    }
    return (sample)
}

time_steps <- 100000
alpha <- 0.3
p01 <- 0.3
p00 <- 1-alpha/(1-alpha) * p01

transition_matrix <- t(matrix(c(p00, 1-p00, p01, 1-p01),2,2))
print(transition_matrix)
out <- simMC(transition_matrix, time_steps)
print(mean(out[1000:length(out)]))
plot(out)