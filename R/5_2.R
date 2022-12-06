input <- readLines("./data/day_5_data.txt")

l <-
    input[1:8] |>strsplit( "") |> 
    mapply(F=\(x) x[seq(from = 2, to = length(x), by = 4)]) |>
    t() |>
    as.data.frame() |>
    as.list() |>
    Map(f = \(x) x[x!=" "]) |>
    unname()

do <-
    input[11:length(input)] |>
    strsplit(" ") |>
    Map(f =as.integer) |>
    Map(f = na.omit)

inst <- function(x, i){
    a=i[1];b=i[2];c=i[3]
    d=switch(s, s = seq(a), r = rev(seq(a)))
    x[[c]] <- c(x[[b]][d], x[[c]])
    x[[b]]<- tail(x[[b]], length(x[[b]])-a)
    x
}
p=\(z) paste(mapply(\(x)x[1], Reduce(inst, do, l)), collapse="")
s="r"
p()
s="s"
p()
