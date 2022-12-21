library(tidyverse)
# tree node
create_node <- function(name){
  list(name = name,children = list(),data = list())
}

create_coordinate <- function(x,y,z){
  list(x = x, y = y,z = z)
}

#tree method
modify_tree <- function(tree,attribute,value){
  #browser()
  attribute <-attribute[2:length(attribute)]
  if(attribute[length(attribute)] == "children"){
    #browser()
    #if((pluck(tree,!!!attribute)) return(tree)
    res<-modify_in(tree,attribute,\(x) append(x,list(create_node(value))))
    res<-modify_in(res,attribute,\(x) {
      #browser()
      x<-setNames(x,c(names(x)[1:(length(x)-1)],value))
    })
  }
  if(attribute[length(attribute)] == "data"){
    res<-modify_in(tree,attribute,\(x) append(x,list(value)))
  }
  res
}

#depth tracker for parsing
pdpt <-function(depth_tracker){
  dpt<- map2(depth_tracker,"children",c)|>unlist()
  dpt[-length(dpt)]
}

elevation <- setNames(1:length(letters),letters)|>append(setNames(c(1,26),c("S","E")))

grid_let<-read_lines("day_12/test_input.txt")|>
  map(str_split,"")|>
  flatten()|>
  map_df(\(x) setNames(x,1:length(x)))

start <- list(x = 1,y = which(grid_let[,1] == "S"))
end <- list(
  x = map(grid_let,\(x) which(x == "E"))|>Filter(f = \(x) length(x) > 0)|>names()|>as.integer(),
  y = map(grid_let,\(x) which(x == "E"))|>Filter(f = \(x) length(x) > 0)|>as.integer())

grid<-read_lines("day_12/test_input.txt")|>
  map(str_split,"")|>
  flatten()|>
  map(\(x)elevation[x])|>
  map_df(\(x) setNames(x,1:length(x)))

grid_row <- nrow(grid)
grid_col <- ncol(grid)

detect_env <- function(current_node){
  x <- expand.grid(c(-1,1),c(1,-1))|>setNames(c("x","y"))|>pull(x)|>map_dbl(sum,current_node$data$x)
  y <- expand.grid(c(-1,1),c(1,-1))|>setNames(c("x","y"))|>pull(y)|>map_dbl(sum,current_node$data$y)
  #browser()
  env_coord<-map2(x,y,\(x,y){
    if(x<1 | x>grid_col) x <- current_node$data$x
    if(y<1 | y>grid_row) y <- current_node$data$y
    z <- grid[y,x]|>pull()
    create_coordinate(x,y,z)})|>
    Filter(f = \(x) x$y != x$x)|>
    Filter(f = \(x) current_node$data$z <= x$z)
  env_coord
}
tree <- start_node
visited_positions <- list()#setNames(list(start_node$name),start_node$name)
walk_grid <- function(current_node,depth_counter = c(),end_node){
  browser()
  if(current_node$data$x == end_node$x & current_node$data$y == end_node$y) return("found")
  depth_counter <- c(depth_counter,current_node$name)
  already_visited <- visited_positions[[current_node$name]]
  if(!is.null(already_visited)){
    if(length(depth_counter)>length(visited_positions[[current_node$name]])) return("already short path available")
  }
  new_position <- setNames(depth_counter,current_node$name)
  visited_positions <- append(visited_positions,new_position)
  env<-detect_env(current_node)|>
      map(\(coord){
        new_node<-create_node(name = paste(coord$x,coord$y,collapse = "_"))
        new_node$data <- coord
        new_node})
  new_names <- map_chr(env,"name")
  env <- setNames(env,new_names)
  tree <- assign_in(tree,c(depth_counter[-1],"children"),env)
  children_names <- 
  map()
  }


#visited_hash <- hashtab(size = grid_col*grid_row)
start_node<-create_node(name = paste(start,collapse = "_"))
start_node$data <- create_coordinate(start$x,start$y,1)
detect_env(start_node)|>
  map(\(coord){
    new_node<-create_node(name = paste(coord$x,coord$y,collapse = "_"))
    new_node$data <- coord
    new_node})
res<-walk_grid(current_node = start_node,end_node = end)
res
