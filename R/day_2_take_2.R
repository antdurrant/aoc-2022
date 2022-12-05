tictoc::tic()
# read data ----
base <- read.table("./data/day_2_data.txt", col.names = c("opp", "you"))
# refs ----
moves <- c(levels = c("rock", "paper", "scissors"))

opponent <- data.frame(opp = LETTERS[1:3], opp_move = moves)

you <- data.frame(you = LETTERS[24:26], you_move = moves)

you_points <- data.frame(you_move = moves, you_points = 1:3)

win_lose_ref <- data.frame(outcome = c("win", "draw", "lose"), score = c(6,3,0))

win_lose <- transform(
    expand.grid(opp_move = moves, you_move = moves), 
    outcome = c("draw", "lose", "win", "win", "draw", "lose", "lose", "win", "draw")
    )

# part 1 ----
res_1 <- 
    Reduce(f = merge, 
           x = list(opponent, you, you_points, win_lose, win_lose_ref), 
           init = base
           )

sum(res_1$you_points, res_1$score)

# part 2 ---
# change what `you` means

you_2 <- 
    data.frame(
        you = LETTERS[24:26],
        outcome = c("lose", "draw", "win")
    )

res_2 <- 
    Reduce(f = merge, 
           x = list(opponent, you_2,  you_points, win_lose, win_lose_ref), 
           init = base
    )

sum(res_2$you_points, res_2$score)
tictoc::toc()
