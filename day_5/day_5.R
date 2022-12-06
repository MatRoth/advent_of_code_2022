library(tidyverse)
data<-read_lines("day_5/task_input.txt",skip_empty_rows = T)
commands <- Filter(\(line)str_detect(line,"move"),data)
stack_raw <- Filter(\(line)!str_detect(line,"move"),data)|>rev()

n_stacks <- stack_raw[1]|>
    strsplit("")|>
    unlist()|>
    parse_number()|>
    na.omit()|>
    max()
stack_strc <- map(stack_raw[2:length(stack_raw)],
    \(line){
      cur_line <- line|>strsplit("")|>unlist()
      cur_line[cumsum(c(2,rep(4,n_stacks-1)))]
      })|>
  transpose()|>
  map(unlist)|>
  map(\(cur_stack) Filter(\(elem) elem != " ", cur_stack))|>
  map(\(cur_stack) c("base",cur_stack))

commands_parsed<-commands|>map(\(line){
  line |>strsplit(" ")|>
    unlist()|>
    parse_number(na = letters)|>
    na.omit()})

transform_stack <- function(stack,command){
  move <- command[1]
  from <- command[2]
  to <- command[3]

  to_move <- stack[[from]][((length(stack[[from]])-(move-1))):length(stack[[from]])] #2nd task 
  
  stack[[from]] <- stack[[from]][1:(length(stack[[from]])-move)]
  
  stack[[to]] <- c(stack[[to]],to_move) 
  
  stack
}

reduce(.x = commands_parsed,.f = transform_stack,.init = stack_strc)|>
  map(\(cur_stack) cur_stack[length(cur_stack)])|>paste0(collapse = "")
                           