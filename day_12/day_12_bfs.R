library(tidyverse)
elevation <- setNames(1:length(letters),letters)|>append(setNames(c(1,26),c("S","E")))

grid_let<-read_lines("day_12/task_input.txt")|>
  map(str_split,"")|>
  flatten()|>
  map_df(\(x) setNames(x,1:length(x)))

start <- list(col = 1,row = which(grid_let[,1] == "S"))
end <- list(
  col = map(grid_let,\(x) which(x == "E"))|>Filter(f = \(x) length(x) > 0)|>names()|>as.integer(),
  row = map(grid_let,\(x) which(x == "E"))|>Filter(f = \(x) length(x) > 0)|>as.integer())

grid<-read_lines("day_12/task_input.txt")|>
  map(str_split,"")|>
  flatten()|>
  map(\(x)elevation[x])|>
  map_df(\(x) setNames(x,1:length(x)))

grid_row <- nrow(grid)
grid_col <- ncol(grid)
#
#edge_list<-map(1:grid_col,\(cur_col) map(1:grid_row,\(cur_row){
#  #print(paste("row",cur_row,"col",cur_col))
#  new_coord <- tibble(
#    new_col = c(-1,+1,0,0)+cur_col,
#    new_row = c(0,0,-1,+1)+cur_row)|>
#    filter(new_col %in% 1:grid_col,new_row %in% 1:grid_row)|>
#    mutate(height = map2_dbl(new_row,new_col,\(row,col) grid[row,col]|>as.double()),
#           height_ok = map_lgl(height,\(neighbour_height) neighbour_height <= (grid[cur_row,cur_col]+1)))|>
#    filter(height_ok == T)
#  list(row = cur_row,col = cur_col,height = grid[cur_row,cur_col] |> as.double(),neighbours = new_coord)
#}))
#
#start_node <- list(pluck(edge_list,start$col,start$row))
#
#go_one_step <- function(cur_nodes,steps,visited_matr,end_row,end_col){
# # browser()
#  steps <- steps+1
#  #browser()
#  #print(steps)
#  walk(cur_nodes,\(x) visited_matr[x$row,x$col]<<-visited_matr[x$row,x$col] <- steps)
#  if(any(map(cur_nodes,\(x) if(x$row == end_row & x$col == end_col) T else F)|>flatten_lgl())) return(steps)
#  #browser()
#  cur_neigh<-map(cur_nodes,\(x) x$neighbours)|>
#    reduce(bind_rows)|>
#    distinct()|>
#    mutate(steps_p1_smaller = map2_lgl(new_row,new_col,\(row,col) {
#      (steps+1) < ((visited_matr[row,col])|>pull())}))|>
#    filter(steps_p1_smaller == T)
#  #browser()
#  cur_neigh_strct <- map2(cur_neigh$new_row,cur_neigh$new_col,\(row,col) pluck(edge_list,col,row))|>
#    Filter(f = \(x) nrow(x$neighbours) != 0)|>
#    Filter(f = \(x) !is.null(x))
#  go_one_step(cur_neigh_strct,steps,visited_matr,end_row,end_col)
#}
#
#go_one_step(start_node,steps = 0, visited_matr = grid |> modify(\(x) 1e100),end_row = end$row,end_col = end$col)
#

#part 2
low_points <- imap(grid_let,
                        \(col,i) map(which(col == "a"),\(row)
                                      list(col = as.numeric(i), row = row)))|>
  flatten()

edge_list<-map(1:grid_col,\(cur_col) map(1:grid_row,\(cur_row){
  #print(paste("row",cur_row,"col",cur_col))
  new_coord <- tibble(
    new_col = c(-1,+1,0,0)+cur_col,
    new_row = c(0,0,-1,+1)+cur_row)|>
    filter(new_col %in% 1:grid_col,new_row %in% 1:grid_row)|>
    mutate(height = map2_dbl(new_row,new_col,\(row,col) grid[row,col]|>as.double()),
           height_ok = map_lgl(height,\(neighbour_height) neighbour_height >= (grid[cur_row,cur_col]-1)))|>
    filter(height_ok == T)
  list(row = cur_row,col = cur_col,height = grid[cur_row,cur_col] |> as.double(),neighbours = new_coord)
}))


start_node <- list(pluck(edge_list,end$col,end$row))

go_one_step <- function(cur_nodes,steps,visited_matr,visited_low_points){
  #browser()
  steps <- steps+1
  #browser()
  print(steps)
  walk(cur_nodes,\(x) visited_matr[x$row,x$col]<<-visited_matr[x$row,x$col] <- steps)
  walk(cur_nodes,\(x) if(visited_low_points[x$row,x$col] == T) visited_low_points[x$row,x$col]<<-visited_low_points[x$row,x$col] <- F)
  if(sum(visited_low_points) == 0) return(visited_matr)
  #browser()
  if(length(cur_nodes) == 0) return(visited_matr)
  cur_neigh<-map(cur_nodes,\(x) x$neighbours)|>
    reduce(bind_rows)|>
    distinct()|>
    mutate(steps_p1_smaller = map2_lgl(new_row,new_col,\(row,col) {
      (steps+1) < ((visited_matr[row,col])|>pull())}))|>
    filter(steps_p1_smaller == T)
  #browser()
  cur_neigh_strct <- map2(cur_neigh$new_row,cur_neigh$new_col,\(row,col) pluck(edge_list,col,row))|>
    Filter(f = \(x) nrow(x$neighbours) != 0)|>
    Filter(f = \(x) !is.null(x))
  go_one_step(cur_neigh_strct,steps,visited_matr,visited_low_points)
}

step_matr<-go_one_step(start_node,steps = -1, visited_matr = grid |> modify(\(x) 1e100),visited_low_points= grid |> modify(\(x) x==1))

data_mask<-grid |>
  modify(\(x) x==1)|>
  pivot_longer(everything(),names_to = "col")|>
  mutate(row = rep(1:nrow(grid),each = ncol(grid)))|>
  filter(value ==T )|>
  select(-value)

map2_dbl(data_mask$row,data_mask$col,\(row,col) step_matr[row,col]|>pull())|>min()


#2nd impl
go_one_step <- function(cur_nodes,steps,visited_matr,lw=1){
  #browser()
  steps <- steps+1
  #browser()
  #print(steps)
  walk(cur_nodes,\(x) visited_matr[x$row,x$col]<<-visited_matr[x$row,x$col] <- steps)
  if(any(map(cur_nodes,\(x) if(x$height == lw) T else F)|>flatten_lgl())) return(steps)
  #browser()
  if(length(cur_nodes) == 0) return(visited_matr)
  cur_neigh<-map(cur_nodes,\(x) x$neighbours)|>
    reduce(bind_rows)|>
    distinct()|>
    mutate(steps_p1_smaller = map2_lgl(new_row,new_col,\(row,col) {
      (steps+1) < ((visited_matr[row,col])|>pull())}))|>
    filter(steps_p1_smaller == T)
  #browser()
  cur_neigh_strct <- map2(cur_neigh$new_row,cur_neigh$new_col,\(row,col) pluck(edge_list,col,row))|>
    Filter(f = \(x) nrow(x$neighbours) != 0)|>
    Filter(f = \(x) !is.null(x))
  go_one_step(cur_neigh_strct,steps,visited_matr)
}

go_one_step(start_node,steps = -1, visited_matr = grid |> modify(\(x) 1e100))
                       