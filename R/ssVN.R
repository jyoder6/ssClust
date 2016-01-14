ssVN <-
function(adj,
                 embeddingDim,
                 knownRed,
                 knownNotRed,
                 initializationStrategy="kpp",
                 Grange=c(2:5)){
  theData <- svd.extract(A=adj, dim=embeddingDim, scaling=TRUE)
  n = nrow(theData)
  
  #set up SS info
  trueLabels <- rep(NA, n)
  trueLabels[knownRed] = 1
  knownCannotLink = knownNotRed
  cannotLinkWithIdx=new.env()
  for(i in knownCannotLink){
    cannotLinkWithIdx[[as.character(i)]]<-knownRed[1]
  }
  
  
  ss<-ssClust(X = theData,
              knownLabels=knownRed,
              trueLabels=trueLabels,
              knownCannotLink=knownNotRed,
              cannotLinkWithIdx=cannotLinkWithIdx,
              Grange=Grange,
              modelNames=c('VVV',"EEE","VII","EII"),
              runParallel=F,
              fracOfCores2Use=1,
              initClassAssignments=NULL,
              initializationStrategy = initializationStrategy)
  
  
  #now determine which class is "red"
  redClassNumber <- unique(ss$cl[knownRed])
  
  
  #then process the z's and get mahalanobis distance for rankings
  redRanking <- ss$z[,redClassNumber]
  
  library('stats')
  mahalanobisDistances <- mahalanobis(theData, 
                                      center = ss$parameters$mean[,redClassNumber],
                                      cov = ss$parameters$variance$sigma[,,redClassNumber])
  
  return(list(redRanking=redRanking, redDist = mahalanobisDistances,ssClustObj = ss))
}
