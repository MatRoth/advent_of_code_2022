library(tidyverse)
library(R6)
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

node<-R6Class("node",
        public = list(
          initialize = function(col,row,z,parents){
            self$name <- paste(col,row,collapse = "_")
            self$col <- col
            self$row <- row
            self$z <- z
            self$parents <- parents
          },
          name = c(),
          col = c(),
          row = c(),
          z = c(),
          parents = c(),
          neighbours = list(),
          add_neighbour = function(new_neighbour){
            self$neighbours <- append(self$neighbours,new_neighbour)
          },
          find_neighbours = function(visited_positions){
            new_col <- c(-1,+1,0,0)+self$col
            new_row <- c(0,0,-1,+1)+self$row

            new_neighbours<-map2(new_col,new_row,\(new_col,new_row){
              #browser()
                if(new_col<1 | new_col>grid_col) new_col <- self$col
                if(new_row<1 | new_row>grid_row) new_row <- self$row
                z <- grid[new_row,new_col]|>pull()
                node$new(new_col,new_row,z,c(self$parents,self$name))
                })|>
              Filter(f = \(neigh) !((neigh$col == self$col) & (neigh$row == self$row))|
                       !(neigh$name == self$name))|>
              Filter(f = \(neigh) (neigh$z)<=(self$z+1))
            #browser()
            walk(new_neighbours,self$add_neighbour)}
          )
        )

start_node <- node$new(start$col,start$row,1,"none")
visited_positions <- list()# setNames(list(start_node$parents),start_node$name)
counter <- 0
walk_grid <-function(cur_node,end_col,end_row){
  counter <<-counter+1
  if((counter %% 1000) == 0 )print(paste("Counter:",counter,"col",cur_node$col,"row",cur_node$row))
  if((cur_node$col == end_col) & (cur_node$row == end_row)) {
    visited_positions[[cur_node$name]] <<- c(cur_node$parents,cur_node$name)
    cat(paste("target_found\nnumber of steps:",length(c(cur_node$parents,cur_node$name))-2,"\n"))
    length(c(cur_node$parents,cur_node$name))-2
  }
  #browser()
  if(cur_node$name %in% names(visited_positions)){
    #browser()
      if((length(c(cur_node$parents))+1) >= length(visited_positions[[cur_node$name]])) return()
    }
  visited_positions[[cur_node$name]] <<- c(cur_node$parents,cur_node$name)
  cur_node$find_neighbours(visited_positions)|>map(walk_grid,end_col,end_row)
}

result<-walk_grid(start_node,end$col,end$row)
