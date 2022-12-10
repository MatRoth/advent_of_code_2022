library(tidyverse)

#tables
win <- c("CX","BZ","AY")
tie <- c("CZ","BY","AX")


file <- "day_2/task_input.txt"
data<-read_lines(file = file)
read_delim(file = file,delim = " ",col_names = c("opponent","me"))|>
  mutate(data = data |> str_remove(" "),
         data_split = data,
         outcome = case_when(data %in% win ~ "w",
                             data %in% tie ~ "t",
                             T~"l"),
         points_shape = case_when(
           me == "X" ~ 1,
           me == "Y" ~ 2,
           me == "Z" ~ 3
         ),
         points_outcome = case_when(
           outcome == "w" ~ 6,
           outcome == "t" ~ 3,
           T ~ 0
         ),
         points_sum = points_shape+points_outcome)|>
  pull(points_sum) |>
  sum()

# part 2
translation_enemy <- setNames(
  c("R","P","S"),c("A","B","C")
)

strategy <- setNames(c("w","l","t"),c("Z","X","Y"))


#tables
case_lose <- setNames(
  c("R","P","S"),
  c("P","S","R"))
case_win <- setNames(
  c("R","P","S"),
  c("S","R","P"))
case_tie <- setNames(
  c("R","P","S"),
  c("R","P","S")
)

file <- "day_2/task_input.txt"
read_delim(file = file,delim = " ",col_names = c("opponent","me")) |>
  mutate(opponent = translation_enemy[opponent],
         me = strategy[me],
         choosen_strat = case_when(
           me == "w" ~ case_win[opponent],
           me == "l" ~ case_lose[opponent],
           me == "t" ~ opponent
         ),
         points_strat = case_when(
           choosen_strat == "R" ~ 1,
           choosen_strat == "P" ~ 2,
           choosen_strat == "S" ~ 3
         ),
         points_outcome = case_when(
           me == "w" ~ 6,
           me == "t" ~ 3,
           T ~ 0
         ),
         sum_points = points_outcome+points_strat)|>
  pull(sum_points)|>
  sum()
