library(tidyverse)

monkeys_pre <- 
    "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
  
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1"  %>% strsplit(split = "\n\n") %>% unlist()

day_11 <- read_file("./data/day_11_data") %>% strsplit(split = "\n\n") %>% unlist()

parse_monkey <- function(x){
    operator <- str_extract(x, "(?<=old )[\\+\\*-\\/].+")
    op_fn <- eval(parse(text = glue::glue("function(old) floor((old {operator})/3)")))
    
    test <- as.numeric(str_extract(x, "(?<= divisible by )\\d+"))
    if_t <- as.numeric(str_extract(x, "(?<=If true: throw to monkey )\\d+"))
    if_f <- as.numeric(str_extract(x, "(?<=If false: throw to monkey )\\d+"))
    test_fn <- eval(parse(text = glue::glue("function(x) if(x %% {test} == 0) items[[{if_t+1}]] <<- c(items[[{if_t+1}]], x)  else items[[{if_f+1}]] <<-  c(items[[{if_f+1}]], x)")))
    
    list(op_fn = op_fn, test_fn = test_fn)              
    
}


monkeys <- unname(Map(parse_monkey, day_11))

items <- str_extract_all(day_11, "(?<=Starting items: ).+") %>% 
    str_extract_all("\\d+") %>% 
    map(as.numeric)

totals <- rep(list(total = 0), length(items))



for(k in 1:20){
    for(i in seq_along(items)){
        
        for(j in seq_along(items[[i]])){
            item <- items[[i]][[j]]
            worry <- monkeys[[i]]$op_fn(item)
            monkeys[[i]]$test_fn(worry)
            
        }# close for j
        totals[[i]] <- totals[[i]] + length(items[[i]])
        items[[i]] <- numeric()
    }# close for i
    message("After round ", k, ": ")
    print(unlist(totals))
    print(items)
    
} # close for k

prod(head(sort(unlist(totals), d = TRUE), 2))


parse_test <- function(x) as.numeric(str_extract(x, "(?<= divisible by )\\d+"))


parse_monkey_2<- function(x){
    operator <- str_extract(x, "(?<=old )[\\+\\*-\\/].+")
    op_fn <- eval(parse(text = 
                            glue::glue("function(old) floor((old {operator})%%{tests})")
    ))
    
    test <- as.numeric(str_extract(x, "(?<= divisible by )\\d+"))
    
    if_t <- as.numeric(str_extract(x, "(?<=If true: throw to monkey )\\d+"))
    if_f <- as.numeric(str_extract(x, "(?<=If false: throw to monkey )\\d+"))
    test_fn <- eval(parse(text = 
                              glue::glue("function(x) if(x %% {test} == 0) items[[{if_t+1}]] <<- c(items[[{if_t+1}]], x)  else items[[{if_f+1}]] <<-  c(items[[{if_f+1}]], x)")
    ))
    
    list(op_fn = op_fn, test_fn = test_fn, test_num = test)              
}


tests <- prod(mapply(parse_test, monkeys_pre))
monkeys <- unname(Map(parse_monkey_2, monkeys_pre))


tests <- prod(mapply(parse_test, day_11))
monkeys <- unname(Map(parse_monkey_2, day_11))


items <- str_extract_all(day_11, "(?<=Starting items: ).+") %>% 
    str_extract_all("\\d+") %>% 
    map(as.numeric)

totals <- rep(list(total = 0), length(items))





for(k in 1:10000){
    for(i in seq_along(items)){
        
        for(j in seq_along(items[[i]])){
            item <- items[[i]][[j]]
            worry <- monkeys[[i]]$op_fn(item)
            monkeys[[i]]$test_fn(worry)
            
        }# close for j
        totals[[i]] <- totals[[i]] + length(items[[i]])
        items[[i]] <- numeric()
    }# close for i
    # print(k, " done")
    if(k %% 1000 == 0){
        message("Totals after round ", k, ": ")
        print(unlist(totals))
    }
    
} # close for k
items
prod(tail(sort(unlist(totals)), 2))
