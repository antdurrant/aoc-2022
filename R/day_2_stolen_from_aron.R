tictoc::tic()
input <- read.table("data/day_2_data.txt", sep=" ")

first = setNames(1:3, LETTERS[1:3])[input[[1]]]
second = setNames(1:3, LETTERS[24:26])[input[[2]]]

# part 1
sum(c(3, 6, 0)[1 + (second - first) %% 3] + second)

# part 2
sum(c(0, 3, 6)[second] + (1 + (first + second) %% 3))
tictoc::toc()

