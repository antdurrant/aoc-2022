library(tidyverse)

x <- c("$ cd /",
       "$ ls",
       "dir a",
       "14848514 b.txt",
       "8504156 c.dat",
       "dir d",
       "$ cd a",
       "$ ls",
       "dir e",
       "29116 f",
       "2557 g",
       "62596 h.lst",
       "$ cd e",
       "$ ls",
       "584 i",
       "$ cd ..",
       "$ cd ..",
       "$ cd d",
       "$ ls",
       "4060174 j",
       "8033020 d.log",
       "5626152 d.ext",
       "7214296 k")

# just make it a bash script
# my brain can't handle the parsing today
library(tidyverse)

# tempdirs to put the files in
tmp <- tempdir()
tmpd <- paste(tmp, "day7", sep = "/")
tmpf <- tempfile(fileext = ".sh")
dir.create(tmpd)

x <- readLines("./data/day_7_data")
# combine filenames and sizes into the filenames
y<- x[str_detect(x, "^\\$ cd |^\\d|^dir")] %>% 
    str_replace("(?<=\\d) ", "_") %>%
    str_replace("^(?=\\d)", "touch ") %>% 
    str_replace("^dir ", "mkdir ") %>%
    str_remove("^\\$ ") %>%
    str_replace("\\/", tmpd) 

# execute
writeLines(y, tmpf)

system(command = paste("sh", tmpf))
z <- paste0("./", list.files(tmpd, recursive = TRUE)) 

# get the recursive dirs from this and strsplit
get_dirs <- function(x){
    p = \(...) paste(..., sep = "/")
    # don't include the filename
    head(Reduce(p, x, accumulate = TRUE), -1)
}
# how big is each directory?
sizes <- 
    tibble(f = z)  %>%
    mutate(
        dirs = map(strsplit(f, "/"), get_dirs),
        filename = str_extract(f, "(?<=/)[[:alnum:]\\._]+$")
    ) %>% 
    unnest(dirs) %>%
    separate(filename, into = c("size", "text"), sep = "_", convert = TRUE) %>%
    group_by(dirs) %>%
    summarise(size = sum(size)) 
    
# part 1
sizes %>%
    filter(size <= 100000) %>%
    pull(size) %>%
    sum()

# part 2
total_used <- max(sizes$size)
total_needed <- 30000000
available  <- 70000000
remaining <- available - total_used
need_to_free <- total_needed - remaining

sizes %>%
    filter(size >= need_to_free) %>%
    filter(size == min(size)) %>% 
    pull(size)

