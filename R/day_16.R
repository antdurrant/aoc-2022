test<-
"Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II"
library(tidyverse)
library(igraph)

parse_data <- function(x){
    read_tsv(x, col_names = "x") %>%
        extract(x, into = c("from", "rate", "to"), 
                regex = "Valve ([A-Z]{2}) has flow rate=(\\d+); tunnels? leads? to valves? (.+)",
                convert = TRUE) %>%
        separate_rows(to, sep = ", ") %>%
        select(from, to, rate)
}
x <- parse_data(test)
x <- parse_data("./data/day_16_data")
nodes = distinct(x, from, rate) %>% mutate(label = paste(from, rate))

edges <- x %>% select(from, to)

g <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
plot.igraph(g, 
            vertex.label = nodes$label, 
            vertex.shape = "none",
            vertex.size = 25,
            edge.width = 1,
            arrow.size = 0,
            arrow.width = 0)


(19*27)+
(20*23)+
    (11*19)+
    (14*16)+
    (6*8)+
    (5*9)+
    (2*18)

