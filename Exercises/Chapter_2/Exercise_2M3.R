################################# Chapter 2 ####################################

#### Exercise 2M3 - Grid Approximation 3 ####
# Define a grid (or list) of parameter values to address

library(tidyverse)

p <- 0.5 # Probability of earth globe being tossed

p_l_e <- 0.3 # Probability of land|earth

p_l_m <- 1 # Probability of land|mars

p_l <- p_l_e*p + p_l_m*(1-p) # Probability of land

p_e_l <- p_l_e*p/p_l

round(p_e_l, 2)
