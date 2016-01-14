svd.extract <-function(A, dim = NULL, scaling = TRUE){
  
  require(irlba)
  L<-A
  if(is.null(dim)) {
    L.svd <- irlba(L)
    dim <- scree.thresh(L.svd$d)
  } else {
    L.svd <- irlba(L,dim,dim)
  }
  
  L.svd.values <- L.svd$d[1:dim]
  L.svd.vectors <- L.svd$v[,1:dim]
  
  if(scaling == TRUE){
    if(dim == 1){
      L.coords <- sqrt(L.svd.values) * L.svd.vectors
    }else{
      L.coords <- L.svd.vectors %*% diag(sqrt(L.svd.values))
    }
  }
  else{
    L.coords <- L.svd.vectors
  }
  
  return(L.coords)
}
