library(tidyverse)

data <- read_lines("day_1/task_input.txt")|>as.numeric()

counter <- 1
result <- numeric()
for(i in seq_along(data)){
  if(!is.na(data[i])) result[i] <- counter else counter <- counter+1
}
#part 1
tibble(grp = result, data = data)|>
  group_by(grp)|>
  summarize(sm= sum(data))|>
  pull(sm)|>
  max(na.rm=T)

#part 2
tibble(grp = result, data = data)|>
  group_by(grp)|>
  summarize(sm= sum(data))|>
  pull(sm)|>
  sort()|>
  tail(3)|>
  sum()
