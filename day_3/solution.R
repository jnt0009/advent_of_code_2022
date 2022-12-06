if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  "readr",
  "dplyr"
)

library(readr)

df <- read_csv("../day_3/input.txt", 
               col_names = FALSE)

colnames(df) <- "sack"

dict <- rbind(
  data.frame(
    key = letters,
    value = 1:26
  ),
  data.frame(
    key = LETTERS,
    value = 27:52
  )
)

#### Solution 2 ####
dupe_list <- c()
final_count <- c()
for (i in df$sack) {
  
  
  chars <-  nchar(i) |> print()
  
  comp1 <- substr(i, 1, (chars/2)) |> print()
  comp2 <- substr(i, (chars/2) + 1, chars) |> print()
  
  comp1 <- comp1 |> strsplit("") 
  comp2 <- comp2 |> strsplit("") 
  
  comp1 <- comp1[[1]] |> unique() |> print()
  comp2 <- comp2[[1]] |> unique() |> print()
  
  comp1 <- as.list(comp1)
  comp2 <- as.list(comp2)
  
  dupes <- comp1[comp1 %in% comp2] |> unlist() |> print()
  
  val <- plyr::mapvalues(dupes,
                  from = c(dict$key),
                  to = c(dict$value)) |>
    as.numeric() |> 
    print()
  # 
  final_count <- append(final_count, val)
  # 

}


#### Answer 1 #### 
final_count |> sum()

#### Solution 2 ####
y <- 1
z <- ""
tmp <- c()
final <- c()
for (i in df$sack) {
  # print(i)
  z = i |> strsplit("") |> unique() 
  z = z[[1]] |> unique()
  tmp[[y]] <- z  
  
  
  
  if(y == 3) {
    y <- 1
    step1 <- tmp[[1]][tmp[[1]] %in% tmp[[2]]]
    step2 <- step1[step1 %in% tmp[[3]]]
    final <- append(final, step2)

    print(tmp)
  } else {
    y <- y + 1
  }
}
 

#### Answer 2 #### 
plyr::mapvalues(final,
                from = c(dict$key),
                to = c(dict$value)) |>
  as.numeric() |> 
  sum()
  print()
 







 
