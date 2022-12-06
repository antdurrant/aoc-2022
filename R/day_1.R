# tidyverse ----
tictoc::tic()

library(tidyverse)

elf_cals <- 
    tibble(data = read_lines("./data/day_1_data.txt")) %>%
    mutate(elf = cumsum(nchar(data) == 0),
           calories = parse_number(data))  %>%
    group_by(elf) %>%
    summarise(calories = sum(calories, na.rm = TRUE)) 

# part 1 ----
elf_cals %>%
    filter(calories == max(calories))

# part 2 ----
elf_cals %>% 
    slice_max(calories, n = 3) %>%
    summarise(cals = sum(calories))

tictoc::toc()


# base ----
tictoc::tic()

base_data <- as.numeric(readLines("./data/day_1_data.txt"))
elf <- cumsum(is.na(base_data))
calories <- unlist(Map(sum, split(base_data, elf), na.rm = TRUE))

## part 1 ----
max(calories)

## part 2 ----
sum(tail(sort(calories), 3))

tictoc::toc()



# treat this as needing documentation

# background ----
# numbers are grouped by empty lines
# groups represent elves carrying bags
# individual numbers represent calories per snack in bag

# problem 1 ----
# find the group with the highest total after adding all the numbers in the group

# problem 2 ----
# find the top three groups and the total between them

# read
base_data <- as.numeric(readLines("./data/day_1_data.txt"))
# groups
elf <- cumsum(is.na(base_data))
# sums
calories <- sort(mapply(sum, split(base_data, elf), na.rm = TRUE, SIMPLIFY = TRUE))

## part 1 ----
# which group & how much
# so elves can go there first
tail(calories, 1)

## part 2 ----
# which groups & how much
# just in case #1 has run out since the last count
who <- tail(calories, 3)
who
sum(who)


