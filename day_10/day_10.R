library(tidyverse)
inpt<-read_lines("day_10/task_input.txt")|>
  str_split(" ")

inpt[length(inpt)+1] <- "noop"


state <- list(cycle = 0, register = 1, comp_res = 0)
res<-reduce(.x = inpt,
                    .init = state,
                    .f = \(l,r){
                      #browser()
                      if(r[1] == "noop") {
                        l$cycle <- c(l$cycle,l$cycle[length(l$cycle)] + 1)
                        if(l$comp_res != 0){
                          l$register <- c(l$register,
                                          l$register[length(l$register)]+l$comp_res)
                          l$comp_res <- 0
                        }else{
                          l$register <- c(l$register,l$register[length(l$register)])
                          }
                      }
                      if(r[1] == "addx"){
                        l$cycle <- c(l$cycle,
                                     l$cycle[length(l$cycle)] + c(1,2))
                        if(l$comp_res != 0){
                          l$register <- c(l$register,
                                          l$register[length(l$register)]+l$comp_res,
                                          l$register[length(l$register)]+l$comp_res)
                        }else{
                          l$register <- c(l$register,
                                          l$register[length(l$register)],
                                          l$register[length(l$register)])
                        }
                        l$comp_res <- as.numeric(r[2])
                      }
                      l
                      })

register <- res$register[2:length(res$register)]
register <- register[1:(length(register)-1)]
selection <- c(20,60,100,140,180,220)
(register[selection]*selection)|>sum()

data_rows<-split(register,rep(0:5,each=40))
pixel_rows<-split(rep(0:39,6),rep(0:5,each=40))

map2(pixel_rows,data_rows,\(p_row,d_row){
  map2(p_row,d_row,\(p,d){
    #browser()
    if(d %in% ((p-1):(p+1))) "#" else "."})|>unlist()})|>
  map_chr(paste0,collapse ="")|>
  paste(collapse = "\n")|>cat()
