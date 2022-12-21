library(tidyverse)
nmbr_of_cls<-read_lines("day_8/task_input.txt")|>
  str_split("")|>tail(1)|>unlist()|>length()


data<-read_lines("day_8/task_input.txt")|>
  str_split("")|>
  map(as.integer)|>
  transpose()|>
  setNames(1:nmbr_of_cls)|>
  as_tibble()|>
  mutate(across(everything(),as.numeric))

#part 1
#data |>
#  imap(\(col_val,col){
#    col <- col|>as.numeric()
#    imap(col_val,\(row_value,row){
#      if(row == 1 | col == 1 | row == nrow(data) | col == ncol(data)) return(T)
#      cur_row <- data[row,]|>as.numeric()
#      cur_col <- data[,col]|>pull()|>as.numeric()
#      row_left<-cur_row[1:(col-1)] < row_value
#      row_right<-cur_row[(col+1):ncol(data)] < row_value
#      col_up <- cur_col[1:(row-1)] < row_value
#      col_down <- cur_col[(row+1):nrow(data)] < row_value
#      
#      res <- list(row_left,row_right,col_up,col_down)
#      any(map_lgl(res,all))
#  })
#    })|>unlist()|>sum()

view_dist <- function(vec,x){
  w_res <- which(vec >= x)
  result <- w_res[1]
  if(length(w_res) == 0) length(vec) else result
}

result<-data |>
  imap(\(col_val,col){
    col <- col|>as.numeric()
    imap(col_val,\(row_value,row){
      if(row == 1 | col == 1 | row == nrow(data) | col == ncol(data)) return(0)
      cur_row <- data[row,]|>as.numeric()
      cur_col <- data[,col]|>pull()|>as.numeric()
      
      row_left<- view_dist(cur_row[1:(col-1)]|>rev(),row_value)          
      row_right<-view_dist(cur_row[(col+1):ncol(data)],row_value) 
      col_up <-  view_dist(cur_col[1:(row-1)]|>rev(),row_value) 
      col_down <-view_dist(cur_col[(row+1):nrow(data)],row_value) 
      
      res <- c(row_left,row_right,col_up,col_down)
      #if(col == 3 & row == 2)browser()
      
      prod(res)
    })
  })

result |> unlist()|>max()
