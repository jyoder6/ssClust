getZFromClassAssignments <-
function(clss, n, G, knownLabels,
         knownCannotLink=NULL,
         prohibitedGroups=NULL){
  success=T
  z=matrix(0, n,G)
  
  for(i in 1:n){
    z[i,clss[i]+1]<-1
  } # end for
  

  
  z=modifyZ(z=z, 
            G=G,
            clss=clss,
            knownLabels=knownLabels,
            knownCannotLink=knownCannotLink,
            prohibitedGroups=prohibitedGroups)
  
  #done with initial class assignments
  
  if (any(apply( z, 2, sum)<=1)) {
    #missing groups
    small <- sqrt(.Machine$double.neg.eps)
    z[z < small] <- small
    z <-  t(apply( z, 1, function(x) x/sum(x)))
    success=F
  }#end if
  
  return(list(z=z, success=success))
}
