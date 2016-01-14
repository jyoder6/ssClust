getInitialClustering <-
function(data,
         knownLabels=NULL,
         trueLabels=NULL,
         knownCannotLink=NULL,
         cannotLinkWithIdx=NULL){
  trueLabels[-knownLabels]<-NA
  c <- length(knownLabels)
  numClassesKnown <- ifelse(c>0,length(unique(trueLabels[knownLabels])),0)
  knownTrueLabels <- sort(unique(trueLabels[knownLabels]))
  d<-as.matrix(dist(data))
  knownGroups<- lapply(knownTrueLabels, function(x){which(trueLabels==x)})
  allKnownLabels<-unique(knownLabels)
  
  #Jimmy the distance matrix to account for semi-sup information.  Put objects in the same cluster at distance 0 and object in different clusters at distance 10^25.
  for(group in knownGroups){
    notThisGroup<-setdiff(allKnownLabels,group)
    for(guy in group){
      d[group,guy]<-0
      d[guy,group]<-0
      d[guy,notThisGroup]<-10^25
      d[notThisGroup,guy]<-10^25
    }#end for
  }#end for
  
 # bannedGroups=new.env()
  if(!is.null(knownCannotLink)){
    for(i in knownCannotLink){
      for(j in cannotLinkWithIdx[[as.character(i)]]){
          group <- unlist(knownGroups[(trueLabels[j]+1)])
          d[group,i]<-10^25
          d[i,group]<-10^25
      }#end for j
    }#end for i    
}#end if

  library('cluster')
  agg <- as.hclust(agnes(x=d,diss=TRUE,method="complete"))
  return(agg)
  #return(list(agg=agg, bannedGroups = bannedGroups))
}
