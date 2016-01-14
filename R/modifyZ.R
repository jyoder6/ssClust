

modifyZ <- function(z,
                    G,
                    clss,
                    knownLabels,
                    knownCannotLink,
                    prohibitedGroups){ #cannotLinkWithIdx \subset knownLabels
  if(!is.null(knownLabels)){
    for(i in knownLabels){
      z[i,clss[i]+1]<-1
      if(G>1){
        z[i,-(clss[i]+1)]<-0
      }#end if
    }#end for    
  }#end if
  
  
  if(!is.null(knownCannotLink)){
    for(i in knownCannotLink){
      for(j in prohibitedGroups[[as.character(i)]]){
        z[i,(j+1)]<-0
      }#end for j
      z[i,]<-z[i,]/sum(z[i,])
    }#end for    
  }#end if
  
  return(z)
}