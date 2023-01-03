library(tidyverse)
inpt <- "day_15/task_input.txt"
max_x <- 4000000#20
max_y <- 4000000#20

set_lines<-read_lines(inpt)|>
  map(str_split,":")|>
  map_depth(2,str_split,",")|>
  map_depth(3,str_extract,"-*\\d+")|>
  map_depth(3,as.numeric)|>
  map_depth(3,setNames,c("x","y"))|>
  map_depth(2,setNames,c("sensor","beacon"))|>
  flatten()|>
  map(\(pair){
    #browser()
    man<- abs(pair$sensor["x"]-pair$beacon["x"])+abs(pair$sensor["y"]-pair$beacon["y"])
    dia <- tibble(x_l = c(pair$sensor["x"]-(0:man),rev(pair$sensor["x"]-(0:man))),
                  x_r = c(pair$sensor["x"]+(0:man),rev(pair$sensor["x"]+(0:man))),
                  y = c(pair$sensor["y"]-(man:0),pair$sensor["y"]+(0:man)))
    dia|>distinct()
  },.progress = T)|>
  bind_rows()|>
  filter(y >= 0 & y <= 4000000)|>
  arrange(y)

set_lines$x_l[set_lines$x_l < 0] <- 0
set_lines$x_l[set_lines$x_l > 4000000] <- 4000000
set_lines$x_r[set_lines$x_r < 0] <- 0
set_lines$x_r[set_lines$x_r > 4000000] <- 4000000
set_lines <- set_lines + 1

set_lines<-set_lines|>
  group_by(y)|>
  arrange(x_l,.by_group = T)|>
  nest_by()

fn <- \(df){
  res<-vector(mode = "logical",length = 3)
  cur_max <- 0
  #browser()
  for(i in 1:length(df)){
    if(i %% 100000 == 0) print(i/length(df))
    if(df[[i]]$x_l[[1]] != 1) return(list(row = i,data = df[[i]]))
    for(j in 2:nrow(df[[i]])){
      cur_max <- pmax.int(cur_max,df[[i]]$x_r[j-1])
      res[j-1] <- between(df[[i]]$x_l[j],0,cur_max)
    }
    cur_max <- pmax.int(cur_max,df[[i]]$x_r[j])
    if(cur_max != 4000001) return(list(row = i,data = df[[i]]))
    if(!all(res)) return(list(row = i,data = df[[i]])) else cur_max <- 0}
}

res<-fn(set_lines$data)
setdiff(1:4000001,pmap(res$data,\(x_l,x_r) x_l:x_r)|>reduce(union))
sprintf("%.2f",((x-1)*4000000)+(y-1))

#splt <- split(set_lines,set_lines$y)

#rng <- 0:4000000
#is_beacon <- function(l,r){
#  setdiff(rng,map2(l,r,`:`)|>
#    reduce(union))
#}
#is_beacon_cmp_mem <- is_beacon |> compiler::cmpfun()|>memoise::memoise()

#for(i in seq_along(splt)){
#  print(i)
#  cur_res <- is_beacon_cmp_mem(splt[[i]]$x_l,splt[[i]]$x_r)
#  if(length(cur_res)>0) stop()
#}

#cpr<-Rcpp::cppFunction('
#                  IntegerVector comp_res_cpp(DataFrame set_lines){
#                    // dataframe columns
#                    IntegerVector x_l = set_lines[0];
#                    IntegerVector x_r = set_lines[1];
#                    IntegerVector y = set_lines[2];
#                    
#                    // accumulator
#                    IntegerVector accum_val (4000000,1);
#                    IntegerVector accum = accum_val;
#                    
#                    
#                    // first iteration
#                    IntegerVector range = seq(x_l[0],x_r[0]);
#                    for(int i = 0; i < range.length();++i){
#                      accum[range[i]] = 0;
#                    }
#                    
#                    // all other iterations
#                    for(int i = 1; i < y.length(); ++i){
#                      if((i % 100) == 0) Rprintf("%i\\n",i);
#                      if(y[i-1] != y[i]){
#                        if(sum(accum) > 0 ) {return(accum);};
#                        IntegerVector accum (4000000,1);
#                    }
#                    //set positions to 0 which are ticked off
#                      IntegerVector range = seq(x_l[i],x_r[i]);
#                      for(int i = 0; i < range.length();++i){
#                        accum[range[i]] = 0;
#                      }
#                      
#                    }
#                  }')
#
#cpr(set_lines)
#
#comp_res <- function(set_lines){
#  accum_val <- rep(1,4000001)
#  accum <- accum_val
#  accum[(set_lines$x_l[1]:set_lines$x_r[1])] <- 0 
#  for(i in 2:nrow(set_lines)){
#    if((i %% 100) == 0) print(i)
#    if(set_lines$y[i-1] != set_lines$y[i]){
#      if(sum(accum) > 0) return(accum)
#      accum <- rep(1,4000001)
#    }
#    if(sum(accum) == 0) next
#    accum[(set_lines$x_l[i]:set_lines$x_r[i])] <- 0 
#  }
#}
#
#comp_res_c <- compiler::cmpfun(comp_res)
#res<-comp_res_c(set_lines)
