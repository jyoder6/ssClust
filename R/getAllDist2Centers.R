getAllDist2Centers<-function(data,matCenters){

  
  sapply(1:nrow(data),function(rw){
    dist2Centers(data[rw,],matCenters)
  })
}