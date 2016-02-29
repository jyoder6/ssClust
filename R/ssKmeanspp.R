
ssKmeanspp <-function(data, k, knownLabels, trueLabels, iter.max=100){
  cent = ssClust::kppInit(data, numGroups = k, knownLabels = knownLabels, trueLabels=trueLabels)
  centers = do.call(rbind, cent)
  matAsList<-function(mat){
    lapply(seq_len(nrow(mat)),function(rw){mat[rw,]})
  }
  
  clOld = rep(0,nrow(data))
  for(iter in 1:iter.max){
    cl = ssClust::getClassGivenCenters(data=data,centers=matAsList(centers),
                                       knownLabels=knownLabels,trueLabels=trueLabels)+1
    centers = t(vapply(seq_len(k), function(g){
      if(any(cl==g)){
        return(colMeans(data[cl==g,,drop=F]))
      } 
      return(rep(NA,ncol(data)))
    }, numeric(ncol(data))))
    if(all(cl==clOld))break
    clOld=cl
  }
  cl = cl - 1
  return(list(centers=centers, cluster=cl))
}