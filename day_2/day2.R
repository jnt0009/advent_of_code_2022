if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  "readr",
  "dplyr",
  "stringr"
)


df <- read.csv("../day_2/input")

df <- str_split_fixed(df$info, " ", 2) |> as.data.frame()

colnames(df) <- c("opp", "me")
# Rock is A and X and worth 1
# Paper is B and Y and worth 2
# Scissors is C and Z and worth 3 points


df$opp <- plyr::mapvalues(df$opp, c("A", "B", "C"), c("Rock", "Paper", "Scissors"))
df$forced_outcome <- plyr::mapvalues(df$me, c("X", "Y", "Z"), c("Loss", "Draw", "Win")) 
df$me <- plyr::mapvalues(df$me, c("X", "Y", "Z"), c("Rock", "Paper", "Scissors"))

ndf <- df |> 
  mutate(
    outcome = case_when(
      opp == me ~ "Draw",
      opp == "Rock" & me == "Paper" ~ "Win",
      opp == "Rock" & me == "Scissors" ~ "Loss",
      opp == "Paper" & me == "Scissors" ~ "Win",
      opp == "Paper" & me == "Rock" ~ "Loss",
      opp == "Scissors" & me == "Rock" ~ "Win",
      opp == "Scissors" & me == "Paper" ~ "Loss",
    ),
    opp_points = case_when(
      opp == "Rock" ~ 1,
      opp == "Paper" ~ 2,
      opp == "Scissors" ~ 3
    ),
    my_points = case_when(
      me == "Rock" ~ 1,
      me == "Paper" ~ 2,
      me == "Scissors" ~ 3
    ),
    outcome_points = case_when(
      outcome == "Win" ~ 6,
      outcome == "Loss" ~ 0,
      outcome == "Draw" ~ 3
    ),
    final_score = outcome_points + my_points
  )

## First Answer
ndf$final_score |> sum() 

mdf <- df |>
  mutate(
    me  = case_when(
      forced_outcome == "Draw" ~ opp,
      forced_outcome == "Loss" & opp == "Paper" ~ "Rock",
      forced_outcome == "Win" & opp == "Paper" ~ "Scissors",
      forced_outcome == "Loss" & opp == "Scissors" ~ "Paper",
      forced_outcome == "Win" & opp == "Scissors" ~ "Rock",
      forced_outcome == "Loss" & opp == "Rock" ~ "Scissors",
      forced_outcome == "Win" & opp == "Rock" ~ "Paper",
    ),
    opp_points = case_when(
      opp == "Rock" ~ 1,
      opp == "Paper" ~ 2,
      opp == "Scissors" ~ 3
    ),
    my_points = case_when(
      me == "Rock" ~ 1,
      me == "Paper" ~ 2,
      me == "Scissors" ~ 3
    ),
    outcome_points = case_when(
      forced_outcome == "Win" ~ 6,
      forced_outcome == "Loss" ~ 0,
      forced_outcome == "Draw" ~ 3
    ),
    final_score = outcome_points + my_points
  )

## Second Answer
mdf$final_score |> sum()



