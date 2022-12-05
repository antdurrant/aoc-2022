tictoc::tic()
library(tidyverse)

base <- read_delim("./data/day_2_data.txt", col_names = c("opp", "you"), delim = " ")

item_points <-
    tibble::tribble(
        ~you,      ~you_item, ~points,
        "X",     "rock",      1L,
        "Y",    "paper",      2L,
        "Z", "scissors",      3L
    )

opp_item <- 
    tibble::tribble(
        ~opp,      ~opp_item, 
        "A",     "rock",
        "B",    "paper",
        "C", "scissors", 
    )

matchup_points <-
    tibble::tribble(
        ~opp, ~you, ~score,
        "A",  "X",     3L,
        "B",  "X",     0L,
        "C",  "X",     6L,
        "A",  "Y",     6L,
        "B",  "Y",     3L,
        "C",  "Y",     0L,
        "A",  "Z",     0L,
        "B",  "Z",     6L,
        "C",  "Z",     3L
    )

base %>%
    left_join(opp_item) %>%
    left_join(item_points) %>%
    left_join(matchup_points) %>%
    mutate(round = points + score) %>%
    summarise(score = sum(round))


# test <- 
#     tibble::tribble(
#         ~opp, ~you,
#         "A",  "Y",
#         "B",  "X",
#         "C",  "Z"
#     )
# 
# test %>%
#     left_join(item_points) %>%
#     left_join(matchup_points) %>%
#     mutate(round = points + score) %>%
#     summarise(score = sum(round))
# 

# part_2 ----

opp_lookup <- 
    tibble::tribble(
        ~opp,   ~opp_item,  ~you_item, ~score, ~points,
        "A",      "rock",     "rock",     3L,      1L,
        "A",      "rock",    "paper",     6L,      2L,
        "A",      "rock", "scissors",     0L,      3L,
        "B",     "paper",     "rock",     0L,      1L,
        "B",     "paper",    "paper",     3L,      2L,
        "B",     "paper", "scissors",     6L,      3L,
        "C",  "scissors",     "rock",     6L,      1L,
        "C",  "scissors",    "paper",     0L,      2L,
        "C", "scissors", "scissors",     3L,      3L
    )




you_lookup <-
    tibble::tribble(
        ~you, ~score,
        "X",     0L,
        "Y",     3L,
        "Z",     6L
    )


test %>%
    left_join(you_lookup) %>%
    left_join(opp_lookup) %>%
    mutate(round = score + points)


base %>%
    left_join(you_lookup) %>%
    left_join(opp_lookup) %>%
    mutate(round = score + points) %>%
    summarise(sum(round))

tictoc::toc()
