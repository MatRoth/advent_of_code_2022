library(tidyverse)
elevation <- setNames(1:length(letters),letters)|>append(setNames(c(1,26),c("S","E")))

grid_let<-read_lines("day_12/test_input.txt")|>
  map(str_split,"")|>
  flatten()|>
  map_df(\(x) setNames(x,1:length(x)))

start <- list(col = 1,row = which(grid_let[,1] == "S"))
end <- list(
  col = map(grid_let,\(x) which(x == "E"))|>Filter(f = \(x) length(x) > 0)|>names()|>as.integer(),
  row = map(grid_let,\(x) which(x == "E"))|>Filter(f = \(x) length(x) > 0)|>as.integer())

grid<-read_lines("day_12/test_input.txt")|>
  map(str_split,"")|>
  flatten()|>
  map(\(x)elevation[x])|>
  map_df(\(x) setNames(x,1:length(x)))

grid_row <- nrow(grid)
grid_col <- ncol(grid)

visited_hash <- hashtab(size = grid_col*grid_row)
hashkeys <- function(h) {
  val <- vector("list", numhash(h))
  idx <- 0
  maphash(h, function(k, v) { idx <<- idx + 1
  val[idx] <<- list(k) })
  val
}

walk_grid <- function(row,col,depth=0,visited_hash,row_end,col_end){
  #if(done == T) return("done")
  #print(paste("row",row,"col",col))
  depth <- depth+1
  sethash(h = visited_hash,key = paste(row,col),value = depth)
  if(row == row_end & col == col_end) {
    #browser()
    #done <<- T
    return("done")}
  cur_heigth <- grid[row,col]|>pull()
  
  new_col <- c(-1,+1,0,0)+col
  new_row <- c(0,0,-1,+1)+row
  #browser()
  new_coord <- tibble(new_row,new_col)|>
    filter(new_col %in% 1:grid_col & new_row %in% 1:grid_row)|>
    mutate(height = map2_dbl(.x = new_row,.y = new_col,.f = \(row,col) {grid[row,col]|>as.double()}))|>
    filter(height <= cur_heigth+1)|>
    mutate(not_visited = map2_lgl(new_row,new_col,\(new_row,new_col) is.null(gethash(visited_hash,paste(new_row,new_col),nomatch = NULL))),
           depth_shorter = pmap_lgl(list(new_row,new_col,not_visited),\(row,col,nvis){
             #browser()
             if(nvis == T) return(T)
             if(gethash(visited_hash,paste(row,col))>(depth+1)) T else F
           }))|>
    filter(depth_shorter == T)
  #browser()
  map2(new_coord$new_row,new_coord$new_col,\(n_row,n_col) walk_grid(n_row,n_col,depth,visited_hash,row_end,col_end))
}
done <- F
walk_grid(start$row,start$col,depth=0,visited_hash = visited_hash,row_end = end$row,col_end = end$col)
hashkeys(visited_hash)|>map(\(key)list(key,gethash(visited_hash,key)))
visited_hash[[paste(end$row,end$col)]]-1

