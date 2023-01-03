################################# Chapter 2 ####################################

#### Exercise 2M1 - Grid Approximation 1 ####
# Define a grid (or list) of parameter values to address

# define grid
p_grid <- round(seq(from = 0, to = 1, length.out = 20), 3)

# define prior
prior <- rep(1, 20)

# compute likelihood at each value in grid
likelihood <- dbinom(3, size = 3, prob = p_grid)

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
prior <- rep(1, 20)

# compute likelihood at each value in grid
likelihood <- dbinom(3, size = 4, prob = p_grid)

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
prior <- rep(1, 20)

# compute likelihood at each value in grid
likelihood <- dbinom(5, size = 7, prob = p_grid)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior/sum(unstd.posterior)

# Plots

#par(mfrow = c(1, 3))
plot(y = prior, x = p_grid, type = "b")
plot(y = likelihood, x = p_grid, type = "b")
plot(y = posterior, x = p_grid, type = "b")

#### tidy solution ####
library(tidyverse)

tibble(p_grid = round(seq(0, 1, length.out = 20), 3), prior = rep(1, 20)) %>%
    mutate(likelihood_1 = dbinom(x = 3, size = 3, prob = p_grid),
           likelihood_2 = dbinom(x = 3, size = 5, prob = p_grid),
           likelihood_3 = dbinom(x = 5, size = 7, prob = p_grid),
           across(starts_with("likelihood"), .fns = ~ .x * prior,
                  .names = gsub("likelihood", "", "posterior_{.col}")),
          across(starts_with("posterior"), ~ .x / sum(.x))
           ) %>%
    select(1, 6:8) %>%
    pivot_longer(cols = 2:4, names_to = "Posterior", values_to = "Probability") %>%
    mutate(Posterior = gsub("likelihood_", "", Posterior)) %>%
    ggplot(aes(x = p_grid, y = Probability, color = Posterior)) +
    geom_line(size = 2) +
    geom_point(size = 4) +
    theme_bw()
