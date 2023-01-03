paste0("day_",1:26)|>map_if(\(x)!dir.exists(x),
                            dir.create)
paste0("day_",1:26)|>
  map(\(x) list(x,paste0(x,".R"),"test_input.txt","task_input.txt"))|>
  walk(\(x){
    file <- x[[1]]
    rest <- x[2:length(x)]
    paths<-paste0(file,"/",rest)
    map_if(paths,
           \(x)!file.exists(x),
           \(x)file.create(x,overwrite = F))
  })

  