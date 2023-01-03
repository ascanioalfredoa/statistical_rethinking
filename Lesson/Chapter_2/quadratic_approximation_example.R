################################# Chapter 2 ####################################

#### Quadratic  Approximation Example ####
library(rethinking)

globe.qa <- quap(
    alist(
        W ~ dbinom(W+L, p), # binomial likelihood
        p ~ dunif(0, 1) # uniform prior
    ),
    data = list(W = 6, L = 3)
)

# display summary of quadratic approximation
precis(globe.qa)

#### Analytical calculations ####
W <- 6
L <- 3

curve(dbeta(x, W+1, L+1), from = 0, to = 1)

#quadratic approximation
curve(dnorm(x, 0.67, 0.16), lty = 2, add = TRUE)
