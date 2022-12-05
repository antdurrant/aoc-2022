library(tidyverse)
# problem ----
# find the end state:
# what letter is at the top of each stack of crates
# start: crates are stacked on top of one another
# crane moves {n} crates from {x} stack to {y} stack

# problem 1 = one at a time
# problem 2 = {n} all at once


# data ---
# yuck
input <- readLines("./data/day_5_data.txt")

crates <- 
    read_delim("./data/day_5_data.txt", "\\s+", col_names = "col_1") %>%
    filter(!str_detect(col_1, "move")) %>%
    separate(col_1,
             into = paste0("col_", 1:9), 
             sep = c(seq(from = 4, to = 36, by = 4))) %>%
    mutate(across(everything(), ~str_extract(.x, "[A-Z]"))) %>%
    as.list() %>%
    # map(~.x[grepl("[A-Z]", .x)])
    map(na.omit)


instructions <- 
    read_delim("./data/day_5_data.txt", 
               skip = 10, 
               col_names = c("x","index", "f", "from", "t", "to")
               ) %>%
    select(index, from, to) %>%
    mutate(across(c(from, to), ~paste0("col_", .x)))



# functions ----
follow_instruction_1 <- function(x, inst){
    from <- inst$from
    to <- inst$to
    n <- inst$index
        
    for(i in seq(n)) {  
        x[[to]] <- c(x[[from]][1], x[[to]])
        x[[from]]<- tail(x[[from]], length(x[[from]])-1)
    }
    x
}

follow_instruction_2 <- function(x, inst){
    from <- inst$from
    to <- inst$to
    n <- inst$index
    
    x[[to]] <- c(x[[from]][seq(n)], x[[to]])
    x[[from]]<- tail(x[[from]], length(x[[from]])-n)
    x
}

all_instructions <- function(data, inst, fn){
    reduce(rlist::list.parse(inst), fn, .init = data) %>%
        map_chr(~.x[1]) %>%
        paste(collapse = "")
}


# test
# x <- list(col_1 = c("A", "B"), col_2 = c("C", "D"))
# follow_instruction_1(x, list(index = 1,from = "col_1", to = "col_2"))
#
# test
# test_crates <- 
#     list(
#         col_1 = c("N", "Z"),
#         col_2 = c("D", "C", "M"),
#         col_3 = c("P")
#     )
# 
# test_instructions <- 
#     tibble(index = c(1,3,2,1),
#            from = paste0("col_", c(2, 1, 2,1)),
#            to = paste0("col_", c(1,3,1,2))
#     )
# follow_instruction_2(test_crates, list(from = "col_2", to = "col_1", index = 1))
# all_instructions(test_crates, test_instructions, follow_instruction_1)
# all_instructions(test_crates, test_instructions, follow_instruction_2)

all_instructions(crates, instructions, follow_instruction_1) 
all_instructions(crates, instructions, follow_instruction_2)
