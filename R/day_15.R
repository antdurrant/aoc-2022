library(tidyverse)
test <- 
    "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3"
td <- 
    read_tsv(test, col_names = "x") %>%
    separate(x, into = c("f", "s_x", "s_y", "b_x", "b_y"), 
             sep = "=") %>%
    select(-f) %>%
    mutate(across(everything(), parse_number)) %>%
    mutate(dist = abs(s_x-b_x)+ abs(s_y-b_y)) 


parse_data <- function(path){
    read_tsv(path, col_names = "x") %>%
        separate(x, into = c("f", "s_x", "s_y", "b_x", "b_y"), 
                 sep = "=") %>%
        select(-f) %>%
        mutate(across(everything(), parse_number)) %>%
        mutate(dist = abs(s_x-b_x)+ abs(s_y-b_y)) 
}
data <- parse_data(test)

min_max_by_x_y <- function(x, y, d, y_val){
    tibble(x = c(x:(x+d),  (x+d):x,  (x-d):x,  (x-d):x), 
           y = c((y+d):y,  y:(y-d),  y:(y-d),  y:(y+d))
    ) %>%
        filter(y == y_val)
}
# 
# tibble(x = c(0:4, 4:0,-4:0,  -4:0),
#        y = c(4:0, 0:-4, 0:-4, 0:4)
# ) %>%
#     ggplot()+
#     geom_point(aes(x =x, y= y))

min_max_by_x_y(x = 0, y = 0, d = 3, y_val = 2)


get_beacons <- function(data, y_val){
    data %>% 
        filter(b_y == y_val) %>%
        pull(b_x) %>%
        unique()
}
y_val = 10
get_points <- function(data, y_val){
    
    data %>%
        # head(1) %>%
        mutate(possible_points = pmap(list(s_x, s_y, dist, y_val), min_max_by_x_y)) %>% 
        mutate(id = as_factor(row_number())) %>%
        unnest(possible_points) %>%
        # ggplot()+geom_point(aes(x = x, y = y))
        # filter(y == y_val) %>%
        select(id, x) %>%
        group_by(id) %>%
        mutate(ab = c("a", "b")) %>%
        pivot_wider(names_from = ab, values_from = x) %>%
        mutate(s = list(seq(a, b))) %>%
        pull(s) %>%
        unlist() %>%
        sort() %>%
        unique()
}

get_impossible_points <- function(data, y_val){
    beacons <- get_beacons(data = data, y_val = y_val)
    points <- get_points(data = data, y_val = y_val)
    length(points[points != beacons])
}


get_impossible_points(td, 10)

parse_data(test) %>% get_impossible_points(10)


x <- parse_data("./data/day_15_data") 
y_val = 2000000
data = x
get_impossible_points(data = x, y_val = 2000000)



# part 2 ---

min_max_by_x_y <- function(x, y, d, y_val){
    tibble(x = c(x:(x+d),  (x+d):x,  (x-d):x,  (x-d):x), 
           y = c((y+d):y,  y:(y-d),  y:(y-d),  y:(y+d))
    ) %>% 
        distinct()
}
cg <-
    data %>%
    mutate(possible_points = pmap(list(s_x, s_y, dist, 4e6), min_max_by_x_y)) %>% 
    mutate(id = as_factor(row_number())) %>%
    unnest(possible_points) 
    # ggplot()+geom_point(aes(x = x, y = y))
    # filter(y == y_val) %>%
    

max_min_udlr <- function(x, y, d){
    data.frame(
        x = c(x+d, x-d, y, y),
        y = c(y, y, y+d, y-d)
    )
}


data %>%
    mutate(data = pmap(list(s_x, s_y, dist), max_min_udlr)) %>%
    mutate(id = row_number()) %>%
    unnest(data) %>%
    ggplot(aes(x = x, y = y, colour = as.factor(id)))+
    geom_point(show.legend = FALSE)


library(duckdb)
drv <- duckdb()
con <- dbConnect(drv)
dbWriteTable(con, "loc", cg)
# dbRemoveTable(con, "loc")

tbl(con, "loc") %>%
    filter(x >=0 & x <= 4000000) %>%
    group_by(x) %>%
    summarise(minmax = list(y))


td


x_count <- 
    cg %>% 
    distinct(x, y) %>%
    count(x)

    select(id, x) %>%
    group_by(id) %>%
    mutate(ab = c("a", "b")) %>%
    pivot_wider(names_from = ab, values_from = x) %>%
    mutate(s = list(seq(a, b))) %>%
    pull(s) %>%
    unlist() %>%
    sort() %>%
    unique()

length(unique(x))

length(unique(y))


tst <- parse_data(test)


tst %>%
    # head(1) %>%
    mutate(possible_points = pmap(list(s_x, s_y, dist, 20), min_max_by_x_y)) %>% 
    mutate(id = as_factor(row_number())) %>%
    unnest(possible_points)  %>%
    select(x, y, id)


data

cg


cg %>% 
    filter(x < 4e6, x > 0)%>%
    select(x, y, id) %>%
    arrange(x, y) %>%
    filter(lead(id) ==id)
    
    
cd %>% filter(x >= 0, x <= 4e6, y >=0, y<=4e6)

points <- cg  %>% filter(x>=0, x<=4e6, y>=0, y<=4e6)

plot(x = cg$x, y = cg$y)


