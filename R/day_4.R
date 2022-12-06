nums <- function(x) list(a = x[1]:x[2], b = x[3]:x[4])
all_in <- function(x) all(x$b %in% x$a)|all(x$a %in% x$b)
any_in <- function(x) any(x$b %in% x$a)
x <- Map(nums, Map(as.numeric, strsplit(readLines("./data/day_4_data"),",|-")))

sum(sapply(x, all_in))
sum(sapply(x, any_in))
