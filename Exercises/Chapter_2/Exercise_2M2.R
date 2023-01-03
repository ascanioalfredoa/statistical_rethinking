################################# Chapter 2 ####################################

#### Exercise 2M2 - Grid Approximation 1 ####
# Define a grid (or list) of parameter values to address

#### tidy solution ####
library(tidyverse)

tibble(p_grid = round(seq(0, 1, length.out = 20), 3),
       prior_1 = rep(1, 20),
       prior_2 = ifelse(p_grid < 0.5, 0, 1)) %>%
    mutate(likelihood_1 = dbinom(x = 3, size = 3, prob = p_grid),
           likelihood_2 = dbinom(x = 3, size = 5, prob = p_grid),
           likelihood_3 = dbinom(x = 5, size = 7, prob = p_grid),
           across(starts_with("likelihood"), .fns = ~ .x * prior_1,
                  .names = gsub("likelihood", "", "posterior_{.col}_prior_1")),
           across(starts_with("posterior_1"), ~ .x / sum(.x)),
           across(starts_with("likelihood"), .fns = ~ .x * prior_2,
                  .names = gsub("likelihood", "", "posterior_{.col}_prior_2")),
           across(starts_with("posterior_1"), ~ .x / sum(.x))
    ) %>%
    select(1, 7:12) %>%
    pivot_longer(cols = 2:7, names_to = "Posterior", values_to = "Probability") %>%
    mutate(Prior = gsub("posterior_likelihood_[0-9]_", "", Posterior),
           Posterior = gsub("likelihood_|_prior_[0-9]", "", Posterior)) %>%
    ggplot(aes(x = p_grid, y = Probability, color = Posterior)) +
    geom_line(size = 2) +
    geom_point(size = 4) +
    facet_grid(~ Prior) +
    theme_bw()
