kppInit <- function(data,
                    numGroups,
                    knownLabels=NULL,
                    trueLabels=NULL
                    ){
  n = dim(data)[1]
  if(is.null(knownLabels)){
    knownGroups = NULL
  } else {
  knownGroups <- sort(unique(trueLabels[knownLabels]))
  }
  numKnownGroups <- length(knownGroups)
  centers <- NULL
  if (numKnownGroups > 0) {
    for (grpNum in 1:numKnownGroups) {
      grp = knownGroups[grpNum]
      knownGroupmembers <- knownLabels[knownLabels %in% which(trueLabels == grp)]
      centers[[grp+1]] <- apply(data[knownGroupmembers, , 
                                   drop = F], 2, mean)
    }
    cannotBeConsidered <- knownLabels
  } else {
    cannotBeConsidered = sample(1:n,1)
    centers[[1]] = data[cannotBeConsidered,]
    numKnownGroups=1
    knownGroups=0
  }
  if(!is.list(centers))centers=list(centers)

  
  numGroupsLeft <- numGroups - numKnownGroups
  
  getWhichAllowed <- function(n, cannotBeConsidered){
    if(is.null(cannotBeConsidered) || is.na(cannotBeConsidered)) return ((1:n))
    return ((1:n)[-cannotBeConsidered])
  }
  covered = knownGroups+1
  
  while(numGroupsLeft > 0){
    numKnownGroups <- numKnownGroups + 1
    pr=getAllDist2Centers(data[getWhichAllowed(n,cannotBeConsidered),,drop=F],do.call(rbind,centers))
    newCenterIndex <- sample(getWhichAllowed(n,cannotBeConsidered),1, prob = pr)
    cannotBeConsidered <- c(cannotBeConsidered,newCenterIndex)
    
    uncovered = setdiff(1:numGroups, covered)
    whichIdx = uncovered[1]
    covered = c(covered, whichIdx)
    centers[[whichIdx]]<- data[newCenterIndex,]
    numGroupsLeft <- numGroups- numKnownGroups
  }# end while
#   
#   
#   #fix correspondence between center label and trueLabel (if know)
#   knownGroups
#   if(length(knownGroups)>0){
#     for(grpNum in 1:length(knownGroups)){
#        grp = knownGroups[grpNum]
#        if((grp+1)!=grpNum){ #if they dont match then need to swap
#          temp = centers[[grpNum]] #where the supervised center was
#          centers[[grpNum]] = centers[[grp+1]]
#          centers[[grp+1]]=temp
#        }
#     }
#   }
#   
  
  centers
}# end kppInit
