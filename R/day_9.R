
move_head <- function(move, x){
    hd <- x$hd
    tl <- x$tl
    
    hd <- switch(names(move),
                 R = c(hd[1], hd[2]+move),
                 L = c(hd[1], hd[2]-move),
                 U = c(hd[1]-move, hd[2]),
                 D = c(hd[1]+move, hd[2])
    )
    
    # diff and absolute diff
    diffs <- hd-tl
    ab_diffs <- abs(diffs)    
    
    is.neg <- \(x) x < 0
    
    # move along both
    if(all(ab_diffs > 0 & any(ab_diffs > 1))) {
            tl[2] <- if(is.neg(diffs[2])) tl[2]-1 else tl[2] +1
            tl[1] <- if(is.neg(diffs[1])) tl[1]-1 else tl[1] +1
            m[tl] <- m[tl]+1
            
            # redo diffs
            diffs <- hd-tl
            ab_diffs <- abs(diffs)  
    }
    
    # move along row
    if(diffs[1] == 0 & ab_diffs[2] > 1){
        
        if(is.neg(diffs[2])) {
            m[tl[1], (tl[2]-1):(hd[2]+1)] <-  m[tl[1], (tl[2]-1):(hd[2]+1)]+1
            tl[2] <- hd[2]+1
        } else {
            m[tl[1], (tl[2]+1):(hd[2]-1)] <-  m[tl[1], (tl[2]+1):(hd[2]-1)]+1
            tl[2] <- hd[2]-1
        }
    }
    # move along col
    if(diffs[2] == 0 & ab_diffs[1] > 1){
        if(is.neg(diffs[1])){
            m[tl[1]:(hd[1]+1), tl[2]] <- m[tl[1]:(hd[1]+1), tl[2]] +1
            tl[1] <- hd[1]+1
        } else{
            m[tl[1]:(hd[1]-1), tl[2]] <- m[tl[1]:(hd[1]-1), tl[2]] +1
            tl[2] <- hd[1]-1
        }
    }
   
}



    
moves <- parse_move(test_data)
n_row <- max(cumsum(moves[names(moves) %in% c("U", "D")]))
n_col <- max(cumsum(moves[names(moves) %in% c("L", "R")]))
m <- matrix(0, n_row, n_col)


start <- c(n_row, 1)

m

Reduce(f = move_head, x = moves, init = list(hd = start,  tl = start))

# ----------



test_data <- 
    c("R 4",
      "U 4",
      "L 3",
      "D 1",
      "R 4",
      "D 1",
      "L 5",
      "R 2")

parse_move <- function(x){
    mv <- as.numeric(stringr::str_extract(x, "\\d+"))
    names(mv) <-stringr:: str_extract(x, "[UDLR]")
    mv[names(mv) %in% c("U", "L")] <- -mv[names(mv) %in% c("U", "L")]
    mv
}

moves

moves <- parse_move(test_data)
n_row <- max(cumsum(moves[names(moves) %in% c("U", "D")]))
n_col <- max(cumsum(moves[names(moves) %in% c("L", "R")]))
m <- matrix(0, 6, 6)

hd <- c(6,1)
m[hd[1], hd[2]] <- 1
m
tl <- hd

for(i in seq_along(moves)){
    move <- moves[i]
   
    hd <- switch(names(move),
                 R = c(hd[1], hd[2]+move),
                 L = c(hd[1], hd[2]+move),
                 U = c(hd[1]+move, hd[2]),
                 D = c(hd[1]+move, hd[2]),
    )
    
    # diff and absolute diff
    diffs <- hd-tl
    ab_diffs <- abs(diffs)    
    
    is.neg <- \(x) x < 0
    
    # move along both
    if(all(ab_diffs > 0) & any(ab_diffs > 1)) {
        tl[2] <- if(is.neg(diffs[2])) tl[2]-1 else tl[2] +1
        tl[1] <- if(is.neg(diffs[1])) tl[1]-1 else tl[1] +1
        m[tl[1], tl[2]] <-1
        
        # redo diffs
        diffs <- hd-tl
        ab_diffs <- abs(diffs)  
    }
    
    # move along row
    if(diffs[1] == 0 & ab_diffs[2] > 1){
        
        if(is.neg(diffs[2])) {
            m[tl[1], (tl[2]-1):(hd[2]+1)] <-  1
            tl[2] <- hd[2]+1
        } else {
            m[tl[1], (tl[2]+1):(hd[2]-1)] <- 1
            tl[2] <- hd[2]-1
        }
    }
    # move along col
    if(diffs[2] == 0 & ab_diffs[1] > 1){
        if(is.neg(diffs[1])){
            m[tl[1]:(hd[1]+1), tl[2]] <- 1
            tl[1] <- hd[1]+1
        } else{
            m[tl[1]:(hd[1]-1), tl[2]] <- 1
            tl[2] <- hd[1]-1
        }
    }
    print(tl)
    print(hd)
    print(m)
}

sum(m[m>0])



