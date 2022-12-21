library(tidyverse)
commands<-read_lines("day_9/test_input.txt")|>
  str_split(" ")|>
  map(\(x) rep(x[1],x[2]))|>
  flatten_chr()
  
head_movement<-accumulate(.x = commands,
           .init = c(x = 1,y = 1),
           .f = \(l,r){
               if(r == "R") l["x"] <- l["x"]+1
               if(r == "L") l["x"] <- l["x"]-1
               if(r == "U") l["y"] <- l["y"]+1
               if(r == "D") l["y"] <- l["y"]-1
             l
           })

hd_m <- head_movement[2:length(head_movement)]

check_adj <- function(h,t){
  x_check<- ((h["x"]-1)<=t["x"])&
            (t["x"]<=(h["x"]+1))
  y_check<- ((h["y"]-1)<=t["y"])&
            (t["y"]<=(h["y"]+1))
  any(x_check,y_check)
}

tail_mov <- accumulate2(.init = c(x = 1,y = 1),
                        .x = hd_m,
                        .y = head_movement[1:(length(head_movement)-1)],
                        .f = \(t,h,hmo){
                          if(check_adj(t,h)) return(t)
                          hmo
                        })
tail_mov|>unique()|>length()

#part 2
commands<-read_lines("day_9/task_input.txt")|>
  str_split(" ")|>
  map(\(x) rep(x[1],x[2]))|>
  flatten_chr()
head_movement<-accumulate(.x = commands,
                          .init = c(x = 1,y = 1),
                          .f = \(l,r){
                            if(r == "R") l["x"] <- l["x"]+1
                            if(r == "L") l["x"] <- l["x"]-1
                            if(r == "U") l["y"] <- l["y"]+1
                            if(r == "D") l["y"] <- l["y"]-1
                            l
                          })
calc_movement<-function(start_mov,desired_depth,depth_counter = 0){
  depth_counter = depth_counter + 1
  tail_mov <- accumulate(.init = c(x = 1,y = 1),
                          .x = start_mov[2:length(start_mov)],
                          .f = \(t,h){
                            #browser()
                            if(all(abs(h-t) <= 1)) return(t)
                            
                            dif <- h-t
                            t+(map_if(dif,
                                      \(x)abs(x)>1,
                                      \(x)if(x > 0)x-1 else x+1)|>unlist())
                          })
  if(depth_counter == desired_depth) return(tail_mov)
  calc_movement(tail_mov,desired_depth,depth_counter)
}

calc_movement(head_movement,9)|>unique()|>length()
