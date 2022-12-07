library(tidyverse)

priorities <- setNames(
  1:length(c(letters,LETTERS)),
  c(letters,LETTERS)
)

#task 1
read_lines("day_3/task_input.txt") |>
  map(\(bag){
    str_lgt <- str_length(bag)
    splt <- str_split(bag,"")|>unlist()
    list(splt[1:(str_lgt/2)],splt[((str_lgt/2)+1):str_lgt])
 })|>
  map(\(bag){
    intersect(bag[[1]],bag[[2]])
  }) |>
  map(\(letr) priorities[letr])|>
  unlist()|>
  sum()

#task 2
data<-read_lines("day_3/task_input.txt") 
data_mask <- rep(1:(length(data)/3),each=3)
tibble(data_mask,data) |> 
  group_nest(data_mask)|>
  pull(data)|>
  map(pull)|>
  map(str_split,"")|>
  map(reduce,intersect)|>
  map(\(letr) priorities[letr])|>
  unlist()|>
  sum()

