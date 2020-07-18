# performance of built-in generators - uniform distribution
Nsim = 10^4 # number of random numbers
x = runif(Nsim)
x1 = x[-Nsim] # vectors to plot
x2 = x[-1] # adjacent pairs
par(mfrow = c(1, 3))
hist(x, cex.lab = 2, cex.axis = 2, cex.main = 2, cex.sub = 2)
plot(x1, x2, cex.lab = 2, cex.axis = 2, cex.main = 2, cex.sub = 2)
acf(x, cex.lab = 2, cex.axis = 2, cex.main = 2, cex.sub = 2)

# deterministic sequence based on the seed
set.seed(1)
runif(5)
set.seed(1)
runif(5)
set.seed(2)
runif(5)

# inverse transform to generate non-standard distributions
Nsim = 10^4 # number of random variables
U = runif(Nsim)
X = -log(1-U) # transforms of uniforms
Y = rexp(Nsim) # exponentials from R
par(mfrow = c(1, 2)) # plots
hist(X, freq = F, main = "Exp from Uniform")
hist(Y, freq = F, main = "Exp from R")

# Evaluating monthly payments with the new plan
usage_1000 <- rnorm(1000, mean = 23, sd = 5)
payment_1000 <- 160 + 15 * pmax((usage_1000 - 20), 0)
mean(payment_1000)
par(mfrow = c(1, 2)) # plots
hist(usage_1000, breaks = 20, freq = F, main = "Monthly Usage")
hist(payment_1000, breaks = 20, freq = F, main = "Monthly Payment")