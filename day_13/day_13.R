library(tidyverse)
data<-read_lines("day_13/task_input.txt")|>
  Filter(f=\(x) x != "")

data <- data |>
  split(rep(1:(length(data)/2),each=2))|>
  map_depth(2,\(x) x|>
              str_replace_all("\\[","list(")|>
              str_replace_all("\\]",")")|>
              str_replace_all("\\(\\)","\\(\\)"))|>
  map_depth(2,\(x)parse(text = x))|>
  map_depth(2,eval)|>
  flatten()|>
  split(rep(1:(length(data)/2),each=2))

#cmpr_a_val <- function(l,r){
#  #browser()
#  if(xor(is.numeric(l),is.numeric(r))){
#    lifted<-map_if(list(l,r),.p = \(x) is.numeric(x),.f = list)
#    res <- process_list(lifted[[1]],lifted[[2]])
#  }
#  if(all(map_lgl(list(l,r),is.list))){
#    if(length(l)==0 & length(r)==0)return(NA)
#    if(length(l)==0)return(T)
#    if(length(r)==0)return(F)
#    res <- process_list(l,r)}
#  if(is.numeric(l) & is.numeric(r)){
#     if(l<r) return(T)
#     if(l>r) return(F)
#     if(l == r) return(NA)}
#  res
#  }
#
#process_list <- function(l,r){
#    lowest_lngth <- pmin(length(l),length(r))
#    res<-map2(l[1:lowest_lngth],r[1:lowest_lngth],cmpr_a_val)
#    if(all(is.na(res))){
#      if(length(l) == length(r)) return(NA)
#      if(length(l) < length(r)) return(T) else return(F)
#    }else{
#      res
#    }
#  }
#
#map_lgl(data,\(x) process_list(x[1],x[2])|>unlist()|>discard(is.na)|>head(1)|>all())|>
#  imap(\(x,i)if(x==T) as.numeric(i)else 0)|>unlist()|>sum()


#part 2

cmpr_a_val <- function(l,r){
  #browser()
  if(xor(is.numeric(l),is.numeric(r))){
    lifted<-map_if(list(l,r),.p = \(x) is.numeric(x),.f = list)
    res <- process_list(lifted[[1]],lifted[[2]])
  }
  if(all(map_lgl(list(l,r),is.list))){
    if(length(l)==0 & length(r)==0)return(NA)
    if(length(l)==0)return(T)
    if(length(r)==0)return(F)
    res <- process_list(l,r)}
  if(is.numeric(l) & is.numeric(r)){
    if(l<r) return(T)
    if(l>r) return(F)
    if(l == r) return(NA)}
  if(is.null(l)|is.null(r)){
    if(is.null(l) & is.null(r)) return(NA)
    if(is.null(l)) return(T)
    if(is.null(r)) return(F)
  }
  res
}

process_list <- function(l,r){
  lowest_lngth <- pmin(length(l),length(r))
  res<-map2(l[1:lowest_lngth],r[1:lowest_lngth],cmpr_a_val)
  if(all(is.na(res))){
    if(length(l) == length(r)) return(NA)
    if(length(l) < length(r)) return(T) else return(F)
  }else{
    res
  }
}

cmpr_pair <- function(l,r){
  process_list(l,r)|>unlist()|>discard(is.na)|>head(1)|>all()
}

data_new <- data|>flatten()|>append(list(list(2)))|>append(list(list(6)))
quicksort<-function(data){
  data <- data|>discard(is.character)
  print(paste("Length of data",length(data)))
  if(length(data) <= 1) return(data)
  cur_pivot_index <- sample(1:length(data),1)
  cur_pivot <- data[[cur_pivot_index]]
  cur_split<-map(data,\(x){
    
    if(identical(x,cur_pivot)) return(list("out",x))
    if(cmpr_pair(x,cur_pivot)) return(list("out",x)) else return(list(x,"out"))
  })|>transpose()
  
  map(cur_split,quicksort)|>flatten()
}
res_2<-quicksort(data_new)|>rev()
which(res_2 == list(2))

one<-detect_index(res_2,\(x) identical(x,list(2)))
two<-detect_index(res_2,\(x) identical(x,list(6)))
one*two
