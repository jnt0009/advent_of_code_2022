if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  "readr",
  "dplyr",
  "tidyverse",
  "rvest"
)



processFile = function(filepath) {
  tot <- 0 ## Total calories per elf
  tmp <- c() ## List of cals being carried per elf
  max <- -1 ## most calories seen.  Starts at -1 to evaluate against 0
  con = file(filepath, "r")
  
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    if(line == "") {
      if(tot > max) max <- tot;  ## Keeps track of most cals
      tmp <- append(tot,tmp)  ## Keeps track of total cals
      tot <- 0 ## Reset
      
    } else {
      tot <- as.integer(line) + tot ## running tally of cals
    }
    
  }
  return(tmp)
  close(con)
}

y <- processFile("input.txt")

y |> order(decreasing = TRUE)
#### ANSWER 1 ####
y[143]
#### ANSWER 2 ####
y[c(143, 64, 122)] |> sum()
