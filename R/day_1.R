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

