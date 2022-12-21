library(tidyverse)

data <- read_lines("day_11/task_input.txt")|>
  Filter(f = \(x) x != "")

nmbr_of_mnk <- length(data)/6
split(data,rep(1:nmbr_of_mnk,each = 6))|>
  map(\(x) x[2:length(x)])|>
  map_depth(str_split,": ",.depth=2)|>
  map_depth(tail,1,.depth=3)|>
  map(flatten)|>
  map_depth(\(x){
    if(str_starts(x,"new")){
      res <- str_split(x,"= ")|>
        unlist()|>
        tail(1)|>
        str_split(" ")|>
        unlist()|>
        tail(2)
      return(res)
    } else{
      str_split(x,", ")|>
        map(str_extract,"\\d+")|>
        unlist()|>
        as.integer()
    }
  },.depth=2)|>
  map(\(x){
    list(
      start_items = x[[1]],
      oper_fct = eval(sym(x[[2]][1])),
      oper_nmbr = x[[2]][2],
      test_nmbr = x[[3]],
      true_monkey = x[[4]],
      false_monkey = x[[5]],
      inspection_counter = 0)
  }) -> monkey_list

play_game <- function(monkey_list,nmbr_r){
for(round in 1:nmbr_r){
  for(mnk in 1:length(monkey_list)){
    while(length(monkey_list[[mnk]]$start_items > 0)){
      monkey_list[[mnk]]$inspection_counter <- monkey_list[[mnk]]$inspection_counter +1
      cur_item <- monkey_list[[mnk]]$start_items[1]
      cur_worry <- if(monkey_list[[mnk]]$oper_nmbr != "old"){
        #browser()
        floor(monkey_list[[mnk]]$oper_fct(cur_item,as.numeric(monkey_list[[mnk]]$oper_nmbr))/3)
      } else {
          floor(monkey_list[[mnk]]$oper_fct(cur_item,cur_item)/3)}
      #browser()
      target_monkey_slct <- (cur_worry %% monkey_list[[mnk]]$test_nmbr) == 0
      #browser()
      target_monkey <- if(target_monkey_slct) monkey_list[[mnk]]$true_monkey else monkey_list[[mnk]]$false_monkey
      monkey_list[[mnk]]$start_items <- monkey_list[[mnk]]$start_items[-1]
      monkey_list[[target_monkey+1]]$start_items <- c(monkey_list[[target_monkey+1]]$start_items,cur_worry)
    }
  }
}
  return(monkey_list)
}

res<- play_game(monkey_list,20)

map(res,"inspection_counter")|>unlist()|>sort()|>tail(2)|>prod()


###part 2

get_last_dig <- function(number,digits=4){
  browser()
  format(number,scientific=T)|>
    as.character()|>
    str_split("e")|>
    map(head,1)|>
    str_split("\\.")|>
    unlist()|>
    tail(1)|>
    str_split("")|>
    unlist()|>
    tail(digits)|>
    paste(collapse="")|>as.numeric()
}
play_game_2 <- function(monkey_list,nmbr_r){
  lcm <- map_int(monkey_list,\(mnk) mnk$test_nmbr)|>prod()
  for(round in 1:nmbr_r){
    for(mnk in 1:length(monkey_list)){
      while(length(monkey_list[[mnk]]$start_items > 0)){
        monkey_list[[mnk]]$inspection_counter <- monkey_list[[mnk]]$inspection_counter +1
        cur_item <- monkey_list[[mnk]]$start_items[1]
        #browser()
        cur_worry <- if(monkey_list[[mnk]]$oper_nmbr != "old"){
          #browser()
          monkey_list[[mnk]]$oper_fct(cur_item,as.numeric(monkey_list[[mnk]]$oper_nmbr))
        } else {
          monkey_list[[mnk]]$oper_fct(cur_item,cur_item)}
        #browser()
        cur_worry <- cur_worry %% lcm
        #browser()
        target_monkey_slct <- ((cur_worry %% monkey_list[[mnk]]$test_nmbr) == 0)
        #browser() 
        target_monkey <- if(target_monkey_slct) monkey_list[[mnk]]$true_monkey else monkey_list[[mnk]]$false_monkey
        monkey_list[[mnk]]$start_items <- monkey_list[[mnk]]$start_items[-1]
        monkey_list[[target_monkey+1]]$start_items <- c(monkey_list[[target_monkey+1]]$start_items,cur_worry)
      }
    }
  }
  return(monkey_list)
}

res<- play_game_2(monkey_list,10000)

map(res,"inspection_counter")|>unlist()|>sort()|>tail(2)|>prod()

