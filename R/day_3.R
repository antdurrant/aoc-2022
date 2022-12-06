
#data
x <-strsplit(readLines("./data/day_3_data"), "")
hlf <- mapply(\(x) length(x)/2, x, SIMPLIFY = TRUE)
lvls <- setNames(1:52, c(letters, LETTERS))

# internal intersects
headtail_intersect <- function(x,y) intersect(head(x, n = y), tail(x, n = y))
intersects <- mapply(headtail_intersect, x, hlf, SIMPLIFY = TRUE )
sum(lvls[intersects])

# groups of three
# this split is gross
# x_pt2 <- split(x, sort(rep(seq(length(x)/3), 3)))
# stolen with thanks from Aron
x_pt2 <- split(x, seq(length(x) %/% 3))
reduce_intercects <- function(x) Reduce(intersect, x)
group_intersects <- mapply(red_int, x_pt2, SIMPLIFY = TRUE)
sum(lvl[group_intersects])

