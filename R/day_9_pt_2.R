test_data <- 
    c("R 5",
      "U 8",
      "L 8",
      "D 3",
      "R 17",
      "D 10",
      "L 25",
      "U 20")
tictoc::tic()


parse_move <- function(x){
    inst <- as.numeric(substring(x, 3))
    nm <- substr(x, 1, 1)
    res <- rep(sign(inst), times = inst)
    names(res) <- rep(nm, times = length(res))
    if(nm %in% c("U", "L")) res <- -res
    
    res
}
parse_moves <- function(x) unlist(unname(Map(parse_move, x)))

moves <- parse_moves(readLines("./data/day_9_data"))
is.neg <- function(x) x<0

test_in <-  function(x, y) all(x == y)
any_in <- function(x, y) any(mapply(test_in, x = x, y = list(y)))


start <- c(row = 1, col = 1)
tl <- start
main_hd <- start

result <- rep(paste(start, collapse = "|"), length(moves))
long_tail <- rep(list(start), 9)

for(j in seq_along(moves)){
    move  <- moves[j]
    
    main_hd <- switch(names(move),
                      U = c(main_hd["row"]+move, main_hd["col"]),
                      D = c(main_hd["row"]+move, main_hd["col"]),
                      R = c(main_hd["row"], main_hd["col"]+unname(move)),
                      L = c(main_hd["row"], main_hd["col"]+move))
    
    
    hd = main_hd

 # i =1
    for(i in seq_along(long_tail)){
        
        tl <- long_tail[[i]]
        
        diffs <- hd-tl
        abs_diffs <- abs(diffs)
        
        
        # do the diagonal
        if(all(abs_diffs >0) & any(abs_diffs >1)){
            tl["row"] <- if(is.neg(diffs["row"])) tl["row"] -1 else tl["row"]+1
            tl["col"] <- if(is.neg(diffs["col"])) tl["col"] -1 else tl["col"]+1

            diffs <- hd-tl
            abs_diffs <- abs(diffs)
        }
        
        # move along col (up/down rows)
        if(abs_diffs["row"] > 1){
            if(is.neg(diffs["row"])){
                tl["row"] <- tl["row"]-1
            } else{
                tl["row"] <- tl["row"]+1
            }
        }
        
        # move along row (left/right cols)
        if(abs_diffs["col"] > 1){
            if(is.neg(diffs["col"])){
                tl["col"] <- tl["col"]-1
            } else{
                tl["col"] <- tl["col"]+1
            }
        }
        hd <- tl
        # update state of snake
        long_tail[[i]] <- hd
        
        result[j] <- paste0(tl, collapse = "|")

        
    }
}
length(unique(result))

tictoc::toc()

