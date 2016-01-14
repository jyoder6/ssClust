ssClustOneModel <-
function(data,
         knownLabels=NULL,
         trueLabels=NULL,
         knownCannotLink=NULL,
         cannotLinkWithIdx=NULL,
         G=2,
         modelName=c('VVV'),
         initClassAssignments,
         initializationStrategy="kpp"){

  #For a selected model, do a semi-supervised clustering  
  dataDim = dim(data)
  oneD = FALSE
  if(is.null(dataDim)|| dataDim[2]==1) oneD = TRUE
  if(oneD){
    n = length(data)
    d = 1
  } else {
    n = nrow(data)
    d = ncol(data)
  }
  c = length(knownLabels)

  ##### get initial class assignments
    need_initial_clust = T
    attempts = 0
    while(need_initial_clust){
      if(is.null(initClassAssignments)){
      centers <- kppInit(data,G,knownLabels,trueLabels)
      clss <- getClassGivenCenters(data=data,
                                   centers=centers,
                                   knownLabels=knownLabels,
                                   trueLabels=trueLabels,
                                   knownCannotLink=knownCannotLink,
                                   cannotLinkWithIdx=cannotLinkWithIdx)
      } else {
        clss <- initClassAssignments
      }
 
     prohibitedGroups <- getProhibitedGroups(clss=clss,
                                            knownLabels=knownLabels,
                                            knownCannotLink=knownCannotLink,
                                            cannotLinkWithIdx=cannotLinkWithIdx)#for cannot links to labeled guys
    
    zGuess = getZFromClassAssignments(clss=clss,
                                 n=n,
                                 G=G, 
                                 knownLabels=knownLabels,
                                 knownCannotLink=knownCannotLink,
                                 prohibitedGroups=prohibitedGroups)

   need_initial_clust = F
   attempts = attempts +1
   if(!zGuess$success && attempts < 10) need_initial_clust=T
}
 if(!zGuess$success) warn("There are missing groups or groups of size 1.")
  z = zGuess$z

  fDiff<-Inf
  tol = 10^(-5)
  iter = 0
  iterMax = 200
  iterMin = 2
  fVal <- -Inf

  while((fDiff > tol || iter < iterMin ) && iter < iterMax){
    
    m<-mstep(modelName,data, z)
    m$parameters$pro = getModifiedPro(m, 
                                      z, 
                                      n, 
                                      c, 
                                      knownLabels, 
                                      knownCannotLink, 
                                      prohibitedGroups)
    e <-estep( modelName, data, m$parameters)
    z = modifyZ(e$z, 
                G,
         clss,
         knownLabels,
         knownCannotLink,
         prohibitedGroups)
    
    likelihoods <-cdens(modelName=modelName, data=data,logarithm=FALSE,e$parameters)
    proWeights<-getModifiedProWeights(n,
                                      c,
                                                G,
                                                clss,
                                                knownLabels,
                                                m,
                                                knownCannotLink,
                                                prohibitedGroups)
    
    loglik<- sum(log(rowSums(likelihoods*proWeights)))
    
    e$loglik<-loglik
    fDiff<- abs(loglik - fVal)/(1+abs(fVal))#relTol
    fVal <- loglik
    iter <- iter + 1
    
  }#end while
  
  parameters = e$parameters
  
  numParams <- nVarParams(modelName, d, G) + G*d + (G - 1)
  
  nFull <- n - c - length(knownCannotLink) 
  nPartial <- c + length(knownCannotLink) 
  if(nPartial > 0 && nFull > 0){
    BIC <- 2*loglik - numParams*log(nFull)
  }else{
    BIC <- 2*loglik - numParams*log(n)
  }#end if
  BICunadjusted = 2*loglik - numParams*log(n)
  return(list(loglik = loglik, parameters = parameters, 
               BIC=BIC,
              z=z, BICunadjusted=BICunadjusted, numParams=numParams,
initPart=clss))
}
