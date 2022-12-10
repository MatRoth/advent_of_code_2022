library(tidyverse)
# tree node
create_folder <- function(name){
  list(name = name,children = list(),data = list())
}

#tree method
modify_tree <- function(tree,attribute,value){
  #browser()
  attribute <-attribute[2:length(attribute)]
  if(attribute[length(attribute)] == "children"){
    #browser()
    #if((pluck(tree,!!!attribute)) return(tree)
    res<-modify_in(tree,attribute,\(x) append(x,list(create_folder(value))))
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

#build tree
depth_tracker = c()
data <-  read_lines("day_7/task_input.txt") |>str_remove("^\\$ ")|>Filter(f = \(x)!str_starts(x,"ls"))
tree<-reduce(.x = data[1:length(data)],.f = \(tree,r){
  if(str_starts(r,"cd \\/")) {depth_tracker <<- "/"; return(tree)}
  if(str_starts(r,"cd \\.\\.")) {depth_tracker <<- depth_tracker[-length(depth_tracker)]; return(tree) }
  if(str_starts(r,"cd ")) {depth_tracker <<- c(depth_tracker,str_remove(r,"cd ")); return(tree)}
  if(str_starts(r,"dir")) return(modify_tree(tree,c(pdpt(depth_tracker),"children"),str_remove(r,"dir ")))
  return(modify_tree(tree,c(pdpt(depth_tracker),"data"),r)
)
},.init = create_folder(name = "root"))

get_dir_size <- function(tree,result = c()){
  if(length(tree$children)==0) return(c(result,tree$data|>unlist()))
  result <- c(result,tree$data|>unlist())
  map(tree$children,get_dir_size,result)|>flatten()|>unlist()
}

get_all_dir_sizes <- function(tree){
  result_all = list()
  if(length(tree$children)==0) {
    result_all[[tree$name]] <- get_dir_size(tree)
    return(result_all)}
  result_all[[tree$name]] <- get_dir_size(tree)
  inter_res<- map(tree$children,get_all_dir_sizes)|>flatten()
  c(result_all,inter_res)
}
get_dir_size(tree) 
get_all_dir_sizes(tree)|>
  map(parse_number)|>
  map(sum)|>
  Filter(f =\(x)x<1e5)|>
  unlist()|>
  sum()

get_all_dir_sizes(tree)|>
  map(parse_number)|>
  map(sum)|>
  Filter(f =\(x)x>=8381165)|>
  unlist()|>
  sort()
  


