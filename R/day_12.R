library(igraph)
library(tidyverse)
test<-
    "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"

# read ----
x <- read_file("./data/day_12_data.txt")

lvls <- setNames(1:52, c(letters, "E", "S"))

# base<- lvls[str_split(test, "")[[1]]]
base<- lvls[str_split(x, "")[[1]]]
nrows <- which.max(is.na(base))-1

# get locations ----
m <- matrix(base[!is.na(base)], nrow = nrows)

nodes <- cbind(
    as.data.frame(which(m == m, arr.ind = TRUE)), 
    val = base[!is.na(base)], 
    name = names(base[!is.na(base)])
) %>% 
    mutate(id = row_number()) 


# find allowable paths ----
get_edges <- function(row, col){
    min_row <- ifelse(row ==1, 1,row-1)
    max_row <- ifelse(row == nrow(m), nrow(m), row+1)
    min_col <- ifelse(col == 1, 1, col-1)
    max_col <- ifelse(col == ncol(m), ncol(m), col+1)
    expand.grid(nb_row = (min_row:max_row), nb_col = (min_col:max_col)) |>
        filter(!(nb_col == col & nb_row == row)) %>%
        filter(nb_col == col | nb_row == row)
}

edges <- 
    nodes %>%
    mutate(data = map2(row, col, get_edges)) %>%
    unnest(data) %>%
    select(val, from = id, nb_row, nb_col) %>%
    left_join(
        nodes %>%
            select(to = id, nb_row = row, nb_col = col, nb_val = val)
    ) %>%
    filter((nb_val-1) <= val)

# turn it into a graph ----
d <- edges %>%
    select(from, to) %>%
    mutate(weight = 1) %>%
    graph_from_data_frame(v = nodes %>% select(id, everything()))

# from where to where? ----
edges %>% filter(val > 26) %>% distinct(val, .keep_all = TRUE)
f = edges %>% filter(val == max(val)) %>% pull(from) %>% unique()
t = edges %>% filter(val > 26 & val < max(val)) %>% pull(from) %>% unique()

# part 1 ----
y <- 
    shortest_paths(
        d, 
        from = f, 
        to = t,
        mode = "out",
        output = "vpath"
        )
length(y$vpath[[1]])-1

# part 2 ----
f <- edges %>% 
    filter(val %in% c(1, 28)) %>%
    pull(from) %>% 
    unique()

# seems that "cannot find a path" gives weird results -
# this is suboptimal but seems to work
path <- function(x){
    p<-shortest_paths(
        d, 
        from = x, 
        to = t,
        mode = "out"
    )
    length(p$vpath[[1]])-1
}

# no path gives -1 as length(thing that doesn't exist) is 0
res <- mapply(path, f)
min(res[!res == -1])
