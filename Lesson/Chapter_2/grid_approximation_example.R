################################# Chapter 2 ####################################

#### Grid Approximation Example ####
# Define a grid (or list) of parameter values to address

# define grid
p_grid <- round(seq(from = 0, to = 1, length.out = 20), 3)

# define prior
prior <- rep(1, 20)

# compute likelihood at each value in grid
likelihood <- dbinom(6, size = 9, prob = p_grid)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior/sum(unstd.posterior)

# Plots

par(mfrow = c(3, 3))
plot(y = prior, x = p_grid, type = "b")
plot(y = likelihood, x = p_grid, type = "b")
plot(y = posterior, x = p_grid, type = "b")

## Second prior ##

# define prior
prior <- ifelse(p_grid < 0.5, 0, 1)

# compute likelihood at each value in grid
likelihood <- dbinom(6, size = 9, prob = p_grid)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior/sum(unstd.posterior)

# Plots

#par(mfrow = c(1, 3))
plot(y = prior, x = p_grid, type = "b")
plot(y = likelihood, x = p_grid, type = "b")
plot(y = posterior, x = p_grid, type = "b")

## Third prior ##

# define prior
prior <- exp(-5 * abs(p_grid - 0.5))

# compute likelihood at each value in grid
likelihood <- dbinom(6, size = 9, prob = p_grid)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior/sum(unstd.posterior)

# Plots

#par(mfrow = c(1, 3))
plot(y = prior, x = p_grid, type = "b")
plot(y = likelihood, x = p_grid, type = "b")
plot(y = posterior, x = p_grid, type = "b")
