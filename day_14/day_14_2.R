library(tidyverse)
grid<-read_lines("day_14/task_input.txt")|>
  str_split(" -> ")|>
  map(str_split,",")|>
  map_depth(2,as.numeric)|>
  map_depth(2,as.list)|>
  map_depth(2,setNames,c("col","row"))|>
  map(\(line)accumulate(line,\(l,r){
    #browser()
    list(col = tail(l$col,1):r$col,
         row = tail(l$row,1):r$row)
  }))|>
  map_depth(2,\(line){
    max_len <- map_int(line,length)|>max()
    map_if(line,
           \(coord) length(coord) == 1,
           \(coord) rep(coord,max_len))
  })|>
  flatten()|>
  reduce(.init = matrix(0,nrow= 1000,ncol = 1000)|>as.tibble(),
         \(grid,coord){
           walk2(coord$row,coord$col,\(r,c) grid[r,c] <<- 1)
           grid
         })



lowest_point <- map_int(grid,\(col)which(col == 1)|>last())|>max(na.rm = T)

grid<-grid|>imodify(\(col_data,name){
  col_data[lowest_point+2] <- 1
  col_data
})
#grid|>transpose()|>map(paste,collapse="")|>paste(collapse = "\n")|>write_lines("day_14/out.txt")

rock_nmbr <- 1
cur_position <- c(row = 0, col = 500)
last_position <- c(row = 999, col = 999)
while(last_position[["row"]] != 0){
  if(rock_nmbr %% 100 == 0) print(rock_nmbr)
  if(grid[cur_position["row"]+1,cur_position["col"]] == 0){
    cur_position["row"] <- cur_position["row"] + 1
  } else{
    if(grid[cur_position["row"]+1,cur_position["col"]-1] == 0) {
      cur_position["row"] <- cur_position["row"]+1
      cur_position["col"] <- cur_position["col"]-1 
    } else {
      if(grid[cur_position["row"]+1,cur_position["col"]+1] == 0) {
        cur_position["row"] <- cur_position["row"]+1
        cur_position["col"] <- cur_position["col"]+1 
      } else {
        if(cur_position["row"] == 0) break
        grid[cur_position["row"],cur_position["col"]] <- 1
        rock_nmbr <- rock_nmbr + 1
        last_position <- cur_position
        cur_position <- c(row = 0, col = 500)
      }
    }
  }
}

