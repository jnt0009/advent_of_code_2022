if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  "readr",
  "dplyr"
)

df <- read_csv("C:/Users/JamariusTaylor/Desktop/projects/advent_of_code/day_4/input.txt", 
                  col_names = FALSE)


colnames(df) <- c("elf_1", "elf_2")

#### Solution 1 ####

for (i in 1:nrow(df)) {
  
  y <- df$elf_1[i] |> strsplit("-")
  df$elf_1_min[i] <- y[[1]][1] |> as.numeric()
  df$elf_1_max[i] <- y[[1]][2] |> as.numeric()
  
  
  z <- df$elf_2[i] |> strsplit("-")
  df$elf_2_min[i] <- z[[1]][1] |> as.numeric()
  df$elf_2_max[i] <- z[[1]][2] |> as.numeric()
  
  
}

df <- df |> 
  rowwise() |> 
  mutate(
    tell_1 = if_else(between(elf_2_max, elf_1_min, elf_1_max), TRUE, FALSE),
    tell_2 = if_else(between(elf_2_min, elf_1_min, elf_1_max), TRUE, FALSE),
    
    tell_3 = if_else(between(elf_1_min, elf_2_min, elf_2_max), TRUE, FALSE),
    tell_4 = if_else(between(elf_1_max, elf_2_min, elf_2_max), TRUE, FALSE),
    
    final_tell = case_when(
      tell_1 == TRUE && tell_2 == TRUE ~ TRUE,
      tell_3 == TRUE && tell_4 == TRUE ~ TRUE,
      TRUE ~ FALSE
    )
    
  ) 

#### Answer 1 ####
df |> 
  filter(final_tell) |>
  count() |>
  sum()

#### Solution 2 ####
fin <- c()
for (i  in 1:nrow(df)) {
    y <- seq(df$elf_1_min[i], df$elf_1_max[i], 1)[seq(df$elf_1_min[i], df$elf_1_max[i], 1) %in% seq(df$elf_2_min[i], df$elf_2_max[i], 1)] |>
    length() 
    
    fin <- append(fin, y)
}

#### Answer 2 ####
fin[fin != 0] |> length()



