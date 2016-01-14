getInitialAssignments <-
function(c, 
         knownLabels,trueLabels,
         oneD, data, 
         G, hcObj, initClassAssignments){

  numClassesKnown <- ifelse(c>0,length(unique(trueLabels[knownLabels])),0)
  if(is.null(initClassAssignments)){
    if(numClassesKnown <1){    
      if(oneD){
        clss <- qclass( data, as.numeric(G))
        if(!is.null(knownLabels)){
          clss[knownLabels] <- mode(clss[knownLabels])
        }#end if
      } else {
        clss <- hclass(hcObj, G) -1  
      }#end if
      
      clss <- clss - min(clss)
    } else {
      clss <- cutree(hcObj,k=G)
      clss <- clss - min(clss)
    }#end if
  } else {
    clss <- initClassAssignments
    clss <- clss - min(clss)
  }
  return(clss )
}
