p_y <- function(y){
  0.5*dbinom(y, 10, 0.2) + 0.5*dbinom(y, 10, 0.6)
}
y <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
plot(y, p_y(y), "h", ylab="Probabilities")

tinytex::install_tinytex()
#Prior(theta = 1) = 0.5
#Likelihood(y = 3 | theta = 1):
dbinom(3, 10, 0.2)
#With bayesian formula:
#P(theta = 1 | y = 3) = Likelihood * Prior / Marginal probability
0.5 * 0.2013266 / p.y(3)

posterior_y <- function(y){
  0.83*dbinom(y, 10, 0.2) + 0.17*dbinom(y, 10, 0.6)
}

plot(y, posterior_y(y), "h", ylab="Posterior predictive p")

#and the posterior predictive probability of y~=3 is:
posterior_y(3)

p_y(3)
