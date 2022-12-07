library(tidyverse)
#task 1
read_lines("day4/task_input.txt")|>
  str_split(",")|>
  map_depth(\(x) str_split(x,"-")|> unlist()|>as.numeric(),.depth = 2)|>
  map(\(cur_ranges){
    r1 <- cur_ranges[[1]][1]:cur_ranges[[1]][2]
    r2 <- cur_ranges[[2]][1]:cur_ranges[[2]][2]
    list(all(r1 %in% r2),all(r2 %in% r1))|>any()
  })|>unlist()|>sum()

#task 2
read_lines("day4/task_input.txt")|>
  str_split(",")|>
  map_depth(\(x) str_split(x,"-")|> unlist()|>as.numeric(),.depth = 2)|>
  map(\(cur_ranges){
    r1 <- cur_ranges[[1]][1]:cur_ranges[[1]][2]
    r2 <- cur_ranges[[2]][1]:cur_ranges[[2]][2]
    any(r1 %in% r2)
  })|>unlist()|>sum()

            