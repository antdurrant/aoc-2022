done <- function(){
    read_data <- function(path){
    x <- as.numeric(stringr::str_split(readr::read_file(path), "", simplify = TRUE))
    t(matrix(na.omit(x), ncol = which.max(is.na(x))-1))
}

# globals
m <- read_data("./data/8")
cs <- ncol(m)
rs <- nrow(m)

# don't need the edges
locs <- expand.grid(x = seq(2, (rs-1)), y = seq(2, (cs-1))) 

# functions
visible <- function(x, y){
    
    loc <- m[x,y]
    
    x_hidden_left <- any(m[x,1:(y-1)]>=loc)
    x_hidden_right <- any(m[x,(y+1):cs]>=loc)
    x_visible <- !all(x_hidden_left, x_hidden_right)
    
    y_hidden_left <- any(m[1:(x-1), y] >= loc)
    y_hidden_right <- any(m[(x+1):rs,y] >= loc)
    y_visible <- !all(y_hidden_left, y_hidden_right)
    
    x_visible | y_visible
}


scenic_score <- function(x,y){
    
    loc <- m[x,y]
    
    scene <- \(tf_vec) if(any(tf_vec)) which.max(tf_vec) else length(tf_vec)
    
    l <- m[x,(y-1):1] >= loc
    left <- if(x == 1) 0 else scene(l)
    r <- m[x,(y+1):cs] >= loc
    right <- if(x == rs) 0 else scene(r)
    u <- m[(x-1):1,y] >= loc
    up <- if(y == 1) 0 else scene(u)
    d <- m[(x+1):rs,y] >= loc
    down <- if(y == 1) 0 else scene(d)
    
    prod(up, down, left, right)
}

# part 1
sum(mapply(visible, x = locs$x, y = locs$y))+ (2*(cs+rs-2))
# part 2
max(mapply(scenic_score, locs$x, locs$y))
}
bench::mark(done())
