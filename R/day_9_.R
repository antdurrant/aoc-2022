
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
    mv <- as.numeric(substring(x, "3"))
    names(mv) <- ifelse(grepl("U|D", x), "row", "col")
    mv[grepl("[U|L]", x)] <- -mv[grepl("[U|L]", x)] 
    mv
}

is.neg <- \(x) x <0



# moves <- parse_move(test_data)
moves <- parse_move(readLines("data/day_9_data"))

start <- c(row = 300, col = 300)
tl <- start
hd <- start
m <- matrix(0, 1000,1000)

# move <- function(m, move){

for(i in seq_along(moves)){
    move  <- moves[i]
    
    hd <- switch(names(move),
                 row = c(hd["row"]+move, hd["col"]),
                 col = c(hd["row"], hd["col"]+move))
    
    diffs <- hd-tl
    abs_diffs <- abs(diffs)
    
    # do the diagonal
    if(all(abs_diffs >0) & any(abs_diffs >1)){
        tl["row"] <- if(is.neg(diffs["row"])) tl["row"] -1 else tl["row"]+1
        tl["col"] <- if(is.neg(diffs["col"])) tl["col"] -1 else tl["col"]+1
        m[tl["row"], tl["col"]] <- 1
        
        diffs <- hd-tl
        abs_diffs <- abs(diffs)
    }
    
    # move along row
    if(abs_diffs["row"] > 1){
        if(is.neg(diffs["row"])){
            m[tl["row"]:(hd["row"]+1), hd["col"]] <- 1 
            tl["row"] <- hd["row"]+1
        } else{
            m[tl["row"]:(hd["row"]-1), hd["col"]] <- 1
            tl["row"] <- hd["row"]-1
        }
    }
    
    if(abs_diffs["col"] > 1){
        if(is.neg(diffs["col"])){
            m[hd["row"], tl["col"]:(hd["col"]+1)] <- 1 
            tl["col"] <- hd["col"]+1
        } else{
            m[hd["row"], tl["col"]:(hd["col"]-1)] <- 1
            tl["col"] <- hd["col"]-1
        }
    }
    # print(m)
    # message(i, "done")
}

sum(m)
tictoc::toc()
# pt 2 
# recurse * 10, because _of course_


state <- matrix(0, 6,6)
visited <- state
visited[6,1] <- 1



test_data <- 
    c("R 4",
      "U 4",
      "L 3",
      "D 1",
      "R 4",
      "D 1",
      "L 5",
      "R 2")
m <- matrix(0, 6,6)

moves <- parse_move(test_data)
start <- c(row = 6, col = 1)
# tl <- start
hd <- start
# 
# 
# diffs <- hd-tl
# abs_diffs <- abs(diffs)

long_tail <- rep(list(start), 10)

moves
m[start["row"], start["col"]] <- 1
m
# for one move----
# get the new main head coords
# get the diff against the first on the tail
# run through the single move logic for all of the long tail
for(j in seq_along(moves)){
    j <- 2
    move = moves[j]
    main_hd <- switch(names(move),
                      row = c(hd["row"]+move, hd["col"]),
                      col = c(hd["row"], hd["col"]+move))
    
    
    .diffs <- main_hd-long_tail[[1]]
    .abs_diffs <- abs(.diffs)
    while(any(.abs_diffs >1)){
        
        hd <- main_hd
        
        for(i in seq_along(long_tail)){
            # i <- 1
            if(all(hd == start)) break
            
            tl <- long_tail[[i]]
            
            diffs <- hd-tl
            abs_diffs <- abs(diffs)
            
            # do the diagonal
            if(all(abs_diffs >0) & any(abs_diffs >1)){
                tl["row"] <- if(is.neg(diffs["row"])) tl["row"] -1 else tl["row"]+1
                tl["col"] <- if(is.neg(diffs["col"])) tl["col"] -1 else tl["col"]+1
                m[tl["row"], tl["col"]] <- 1
                
                diffs <- hd-tl
                abs_diffs <- abs(diffs)
            }
            m
            # move along row
            if(abs_diffs["row"] > 1){
                if(is.neg(diffs["row"])){
                    m[ (tl["row"]+1) , tl["col"] ] <- 1 
                    tl["row"] <- tl["row"] -1
                } else{
                    m[ (tl["row"]-1) , tl["col"] ] <- 1
                    tl["row"] <- tl["row"]-1
                }
                diffs <- hd-tl
                abs_diffs <- abs(diffs)
            }
            m
            if(abs_diffs["col"] > 1){
                if(is.neg(diffs["col"])){
                    m[ tl["row"] , (tl["col"]-1) ] <- 1 
                    tl[ "col"] <- tl["col"]-1
                } else{
                    m[ tl["row"] , (tl["col"]+1) ] <- 1
                    tl["col"] <- tl["col"]+1
                }
                diffs <- hd-tl
                abs_diffs <- abs(diffs)
            }
            
            long_tail[[i]] <- tl
            hd <- tl
            
            if(i == 1){
                .diffs <- main_hd-long_tail[[1]]
                .abs_diffs <- abs(diffs)
            }
        }# close inner for
        
    }# close while
    hd <- main_hd
} # close  outer for
m
