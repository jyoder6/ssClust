getModifiedPro <- function( m, 
                            z, 
                            n, 
                            c, 
                            knownLabels, 
                            knownCannotLink, 
                            prohibitedGroups){

  if(!is.null(knownLabels)){
  divisor = rep(n-c, m$G)
} else {
  divisor = rep(n,m$G)
}

generalGroups <- 1:m$G
if(!is.null(knownCannotLink)){
  for(i in knownCannotLink){
    generalGroups <- setdiff(generalGroups, setdiff(1:m$G,prohibitedGroups[[as.character(i)]]+1))
    divisor[prohibitedGroups[[as.character(i)]]+1] <- divisor[prohibitedGroups[[as.character(i)]]+1] - 1
  }
}
constrainedGroups <- setdiff(1:m$G, generalGroups)
pro = rep(NA,m$G)

allowed = setdiff(1:n, c(knownLabels, knownCannotLink))
for(g in generalGroups)
pro[g]<- sum(z[allowed,g])/divisor[g]


if(!is.null(knownCannotLink)){
  for(i in knownCannotLink){
    pg <- (prohibitedGroups[[as.character(i)]]+1)
    num <- pro[pg]
    div <- 1 - num
    divisor[-pg]<- divisor[-pg] + num/div
    }
}

if(!is.null(constrainedGroups)){
  for(g in constrainedGroups)
    pro[g]<- sum(z[,g])/divisor[g]  
}

  return(pro)
}