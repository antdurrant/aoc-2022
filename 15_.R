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
library(tidyverse)

parse_data <- function(x){
    read_tsv(x,  col_names = "x") %>%
        separate(x, into = c("f", "s_x", "s_y", "b_x", "b_y"), sep = "=") %>%
        mutate(across(.fns = parse_number)) %>%
        select(-f) %>%
        mutate(d = abs(s_x-b_x)+abs(s_y-b_y))
}


parse_data(test)


get_perimeters <- function(x, y, d){
    tibble(x = c(x:(x+d),  (x+d):x,  (x-d):x,  (x-d):x),
           y = c((y+d):y,  y:(y-d),  y:(y-d),  y:(y+d)),
           line = c(rep(1, d+1), rep(2, d+1), rep(3, d+1), rep(4, d+1))
    ) %>%
        distinct()
    
    
}




get_perimeters(x = 10, y = 10, d = 5) %>% 
    ggplot(aes(x = x, y = y, subgroup = 0, alpha = .5))+
    geom_polygon()

tst <- parse_data(test) 

dat <- 
    parse_data("./data/day_15_data") %>%
    mutate(data = pmap(list(s_x, s_y, d), get_perimeters),
           poly = as.character(row_number())) %>%
    unnest(data) 



get_points <- function(x,y,d){
    tibble(
        x = c(x+d, x, x-d, x),
        y = c(y, y+d, y, y-d)
    )
}


plotly::ggplotly(
parse_data("./data/day_15_data") %>%
    mutate(data = pmap(list(s_x, s_y, d), get_points)) %>% 
    mutate(s = as.character(row_number())) %>%
    select(s, data) %>%
    unnest(data) %>%
ggplot(aes(x = x, y = y, fill = s, alpha = .5))+
    geom_path(show.legend = FALSE)+
    coord_cartesian(xlim = c(0,4e6), ylim = c(0,4e6))
)
coord_cartesian(xlim = c(1500000, 1700000), ylim =c(3500000, 3700000))
