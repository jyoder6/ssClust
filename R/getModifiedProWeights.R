getModifiedProWeights<-function(n,
                                c,
                                G,
                                clss,
                                knownLabels,
                                m,
                                knownCannotLink,
                                prohibitedGroups){
  proWeights<-matrix(0,n,G)
  if(!is.null(knownLabels)){
    for(i in knownLabels){
      proWeights[i,clss[i]+1]<-1
      if(G > 1){
        proWeights[i,-(clss[i]+1)]<-0
      } #end if
    }#end for 
    proWeights[-knownLabels,]<-rep(1,n-c) %o% m$parameters$pro
  }else{
    proWeights[,]<-rep(1,n) %o% m$parameters$pro 
  }#end if
  if(!is.null(knownCannotLink)){
    for(i in knownCannotLink){
      proWeights[i,prohibitedGroups[[as.character(i)]]+1] = 0
      proWeights[i,] = proWeights[i,]/sum(proWeights[i,])
    }
  }
  return(proWeights)
}