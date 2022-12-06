x=utf8ToInt(readLines("6"))
f=\(s,o) all(rle(sort(x[s:(s+o)]))$l==1)
g=\(g)which(mapply(f,seq(x),g))[1]+g
g(3)
g(13)
