library(tidyverse)
test <- 
    "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]"

input <- strsplit(test, "\n\n")[[1]] %>% strsplit(split = "\n")

input[[1]]
parse_input <- function(input) {
    
    strsplit(input, split = "\n\n")[[1]] %>% 
        strsplit(split = "\n") %>%
        map(
            ~.x  %>%
                str_replace_all("\\[", "list(c(") %>%
                str_replace_all("\\]", "))") %>%
                as.list() %>%
                map(~eval(parse(text = .x)))
        ) 
}
lists <- parse_input(test)
map(lists, ~map(.x, ~eval(parse(text = . )))) %>% View
lst <- lists[[2]]

test_pairs <- function(lst){
    left <- lst[[1]]
    right <- lst[[2]]
    
    
    test_num <- function(l, r){
        l <- if(is.list(l)) unlist(l, recursive = FALSE)
        r <- if(is.list(r)) unlist(r, recursive = FALSE)
        
        for(i in seq_along(l)){
            if(is.numeric(l[[i]]) &  is.numeric(r[[i]])) {
                if(length(r) > length(l)) return(FALSE)
                any(l[1:length(r)] > r)
                
            } else test_num(l = l, r = r)
        }
    }
    test_num(left, right)
    
}



test_pairs(input[[1]])

parse_list <- function(x){
    
    left <- x[1]
    right <- x[2]
    
    # the lists don't actually matter at all, right?
    # right must be longer
    # right must always be bigger
    
    # are there more numbers on the right?
    l_nums <- str_count(left, "\\d+") 
    r_nums <- str_count(right, "\\d+")
    # if(!l_nums <= r_nums) return(FALSE)
    # if no numbers, compare string length
    if(sum(l_nums & r_nums) == 0 & nchar(left) > nchar(right)) return(FALSE)
    
    
    # if yes, continue
    # get the numbers
    l <- as.numeric(unlist(str_extract_all(left, "\\d+")))
    r <- as.numeric(unlist(str_extract_all(right, "\\d+")))
    diff <- which(l[1:length(r)] != r)
    if(length(diff) == 0) return( l_nums <= r_nums)
    all(l[diff] < r[diff])
}

parse_list(x = input[[5]])

res <- mapply(parse_list, input)

sum(which(res == TRUE))




x <- read_file("./data/day_13_data")
input <- strsplit(x, "\n\n")[[1]] %>% strsplit(split = "\n")

x <- input[[1]]
res <- mapply(parse_list, input)

sum(which(res == TRUE))
x <- input[[2]]




left <- input[[1]][1]
right <- input[[1]][2]

input[[2]]

# the lists don't actually matter at all, right?
# right must be longer
# right must always be bigger

# are there more numbers on the right?
l_nums <- str_count(left, "\\d+") 
r_nums <- str_count(right, "\\d+")
if( r_nums > l_nums) return(FALSE)
# if no numbers, compare string length
if(sum(l_nums & r_nums) == 0 & nchar(left) > nchar(right)) return(FALSE)


# if yes, continue
# get the numbers
l <- as.numeric(unlist(str_extract_all(left, "\\d+")))
r <- as.numeric(unlist(str_extract_all(right, "\\d+")))

all(l <= r[1:length(l)])


# 

# check the number of lists is the same
(str_count(left, "\\[") == str_count(right, "\\["))

# for each list
# some more splitting here

# numeric tests
l = as.numeric(unlist(str_extract_all(left, "\\d+")))
r = as.numeric(unlist(str_extract_all(right, "\\d+")))
(length(l) >= length(r))
# AND


