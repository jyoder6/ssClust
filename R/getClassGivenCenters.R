



getClassGivenCenters<-function(data,centers,knownLabels=NULL,trueLabels=NULL, 
                               knownCannotLink=NULL,
                               cannotLinkWithIdx=NULL){
  if(!is.null(knownLabels)){
    clss <- rep(0,nrow(data))
    newDat <- data[-knownLabels,, drop=FALSE]
    clss[-knownLabels]<-sapply(1:nrow(newDat),function(rw){
      which.min(unlist(lapply(centers, function(cent){sum((newDat[rw,]-cent)^2)})))-1
    })
} else {
   clss<-sapply(1:nrow(data),function(rw){
    which.min(unlist(lapply(centers, function(cent){sum((data[rw,]-cent)^2)})))-1
    })
}
  
#fix supervised
  if(!is.null(trueLabels)&!is.null(knownLabels)){
    clss[knownLabels]=trueLabels[knownLabels]
  }


  if(!is.null(knownCannotLink)){
    for(i in knownCannotLink){
      cannotLinkWithGroups <- unique(clss[cannotLinkWithIdx[[as.character(i)]]])
      modCenters <- centers
      for(g in (cannotLinkWithGroups+1))modCenters[[g]]=modCenters[[g]]*Inf

      clss[i] <-  which.min(unlist(lapply(modCenters, function(cent){sum((data[i,]-cent)^2)})))-1
    }
  }
  return(clss)
}# end getClassGivenCenters


