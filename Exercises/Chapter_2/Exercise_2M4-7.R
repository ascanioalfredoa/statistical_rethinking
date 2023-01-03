################################# Chapter 2 ####################################

#### Exercise 2M4 - Counting Cards ####

# Card with two black sides
# Card with two white sides
# Card with one side each
## If my first draw of a card shows a black side,
## what's the probability of the other side being black?
### Use counting method

tibble(first_card = c("bb", "bw", "ww"), # Names of each card
       ways_black = c(2, 1, 0), # Ways for each card to be drawn black side first
       prior = c(1, 1, 1)) %>% # Ways to draw each card
    mutate(likelihood = ways_black * prior,
           posterior = likelihood*prior,
           posterior = posterior/sum(posterior)) %>%
    filter(likelihood == 2) %>%
    pull(posterior)

tibble(card_ways = c(1, 1, 1),
       black_sides = c(2, 1, 0)) %>%
    mutate(likelihood = black_sides,
           posterior = likelihood*card_ways/sum(likelihood*card_ways)) %>%
    filter(black_sides == 2) %>%
    pull(posterior)


#### Exercise 2M5 - Counting Cards ####

# TWO Cards with two black sides
# Card with two white sides
# Card with one side each
## If my first draw of a card shows a black side,
## what's the probability of the other side being black?
### Use counting method

tibble(card_ways = c(1, 1, 1, 1),
       black_sides = c(2, 2, 1, 0)) %>%
    mutate(likelihood = black_sides,
           posterior = likelihood*card_ways/sum(likelihood*card_ways)) %>%
    filter(black_sides == 2) %>%
    pull(posterior) %>%
    sum()

tibble(first_card = c("bb", "bw", "ww"), # Names of each card
       ways_black = c(2*2, 1, 0), # Ways for each card to be drawn black side first
       prior = c(1, 1, 1)) %>% # Ways to draw each card
    mutate(likelihood = ways_black*prior,
           posterior = likelihood*prior,
           posterior = posterior/sum(posterior)) %>%
    filter(likelihood == 2*2) %>%
    pull(posterior)



#### Exercise 2M6 - Counting Cards ####

# Card with two black sides - 1 way to draw
# Card with two white sides - 2 ways to draw
# Card with one side each - 3 ways to draw
## If my first draw of a card shows a black side,
## what's the probability of the other side being black?
### Use counting method

tibble(card_ways = c(1, 2, 3),
       black_sides = c(2, 1, 0)) %>%
    mutate(likelihood = black_sides,
           posterior = likelihood*card_ways/sum(likelihood*card_ways)) %>%
    filter(black_sides == 2) %>%
    pull(posterior)



#### Exercise 2M7 - Counting Cards ####

# Card with two black sides
# Card with two white sides
# Card with one side each
## If my first draw of a card shows a black side,
## If my second draw of a card shows a white side,
## what's the probability of the other side being black?
### Use counting method

tibble(card_ways = c(1, 1, 1),
       black_sides = c(2, 1, 0)) %>%
    mutate(likelihood = black_sides*0.5,
           posterior = likelihood*card_ways/sum(likelihood*card_ways)) %>%
    filter(black_sides == 2) %>%
    pull(posterior)
