library(tidyverse)
inpt <- "day_15/task_input.txt"
rw <- 2000000 # 2000000
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
    })|>
  bind_rows()|>
  filter(y == rw)

posi<-map2(set_lines$x_l,set_lines$x_r,`:`)|>
  unlist()|>
  unique()

beacon_points <- read_lines(inpt)|>
  map(str_split,":")|>
  map_depth(2,str_split,",")|>
  map_depth(3,str_extract,"-*\\d+")|>
  map_depth(3,as.numeric)|>
  map_depth(3,setNames,c("x","y"))|>
  map_depth(2,setNames,c("sensor","beacon"))|>
  flatten()|>
  map("beacon")|>
  bind_rows()|>
  filter(y == rw)|>
  distinct()|>
  pull(x)

sensor_points <- read_lines(inpt)|>
  map(str_split,":")|>
  map_depth(2,str_split,",")|>
  map_depth(3,str_extract,"-*\\d+")|>
  map_depth(3,as.numeric)|>
  map_depth(3,setNames,c("x","y"))|>
  map_depth(2,setNames,c("sensor","beacon"))|>
  flatten()|>
  map("sensor")|>
  bind_rows()|>
  filter(y == rw)|>
  distinct()|>
  pull(x)

length(posi)

setdiff(posi,beacon_points)|>
  setdiff(sensor_points)|>length()

setdiff(posi,beacon_points)|>length()


#part 2