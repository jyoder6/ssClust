getProhibitedGroups<-function(clss,
                              knownLabels,
                              knownCannotLink,
                              cannotLinkWithIdx){
  prohibitedGroups = new.env()
  if(!is.null(knownCannotLink)){
    for(i in knownCannotLink){
      prohibitedGroups[[as.character(i)]] = NULL
      for(j in cannotLinkWithIdx[[as.character(i)]]){
        if(!j %in% knownLabels ) stop("cannotLinkWithIdx must be amungst knownLabels")
        prohibitedGroups[[as.character(i)]] = c(prohibitedGroups[[as.character(i)]], clss[j])
      }#end for j
      prohibitedGroups[[as.character(i)]] = unique(prohibitedGroups[[as.character(i)]])
    }#end for   
  }
  
  return(prohibitedGroups)
}