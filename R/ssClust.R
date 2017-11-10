ssClust <-
function(X,
         knownLabels=NULL,
         trueLabels=NULL,
         knownCannotLink=NULL,
         cannotLinkWithIdx=NULL,
         Grange=2,
         modelNames=c('VVV'),
         runParallel=TRUE,
         fracOfCores2Use=1,
         initClassAssignments=NULL,
         initializationStrategy="kpp",
         penalizeSupervised=T){
  
  loadParallelFramework(runParallel, fracOfCores2Use)

  if(!is.null(trueLabels)){
  trueLabels<-trueLabels-min(trueLabels[!is.na(trueLabels)])
  numberOfClassesKnown = length(unique(trueLabels[knownLabels]))
  } else{
    numberOfClassesKnown=0
  }
  
  Grange = Grange[Grange >= numberOfClassesKnown]
  if(length(Grange) == 0)stop("No valid number of clusters to consider.  Expand Grange.")

  
  #Go through all the models and apply semi-supervised clustering
  indivModels<- expand.grid(x=Grange,y=modelNames)
  num2run <- nrow(indivModels)

    hcObj <-NULL

baseCallList = list(data=X, 
                    knownLabels=knownLabels,
                    trueLabels=trueLabels,
                    knownCannotLink= knownCannotLink, 
                    cannotLinkWithIdx=cannotLinkWithIdx,
                    #G=as.integer(indivModels[i,1]), #need to add this
                    #modelName=as.character(indivModels[i,2]), #and this
                    initClassAssignments = initClassAssignments,
                    initializationStrategy = initializationStrategy)
safe_ssClustOneModelCall = function(G, modelName){
  tryCatch(do.call(ssClustOneModel, args=c(baseCallList, list(G=G,modelName=modelName))),
           error = function(e){list(loglik = NA, parameters = NA, BIC=NA, BICunadjusted=NA, z=NA, numParams=NA, initPart=NA)}
  )
}

get_model_run = function(num2run, i, indivModels){
  if(num2run > 1){
    res <- safe_ssClustOneModelCall(G=as.integer(indivModels[i,1]),
                                    modelName=as.character(indivModels[i,2]))
  } else {
    res <- safe_ssClustOneModelCall(G=as.integer(indivModels[1]),
                                    modelName=as.character(indivModels[[2]]))
  }
  
  if(!any(is.na(res$z))){
    res$cl <- apply(res$z,1,which.max)
  } else {
    res$cl <- NA
  }
  res
}

  #MAIN LOOP
  if(runParallel){
    cclustRuns <- foreach(i=1:num2run) %dopar% {
      res = get_model_run(num2run=num2run, i=i, indivModels=indivModels)
      res
    }#end foreach
  } else {
    cclustRuns=NULL
  }#end if
  
  modelsBIC = rep(0,num2run)
  modelsLoglik = rep(0,num2run)
  modelsUnadjustedBIC = rep(0, num2run)
  modelsNumParams = rep(0, num2run)
  modelsInitPart = matrix(NA, nrow=num2run, ncol=nrow(X))
  modelsClasses = matrix(NA, nrow=num2run, ncol=nrow(X))

  for(i in 1:num2run){
    if(!runParallel){
      cclustRuns[[i]] = get_model_run(num2run=num2run, i=i, indivModels=indivModels)
    }#end if
    
    modelsBIC[i]=cclustRuns[[i]]$BIC
    modelsLoglik[i]=cclustRuns[[i]]$loglik
    modelsUnadjustedBIC[i]=cclustRuns[[i]]$BICunadjusted
    modelsNumParams[i]=cclustRuns[[i]]$numParams
    modelsInitPart[i,]=cclustRuns[[i]]$initPart   
    modelsClasses[i,]=cclustRuns[[i]]$cl
  }#end for
  #END MAIN LOOP
  
  
  #PICK WINNER
  if(penalizeSupervised){
    whichIsBest<-which.max(modelsUnadjustedBIC)
  } else { 
    whichIsBest<-which.max(modelsBIC)
  }
  modelsBIC<-cbind(indivModels,modelsBIC,modelsLoglik)
  classes <- apply(cclustRuns[[whichIsBest]]$z,1,which.max)
  BIC = matrix(modelsBIC$modelsBIC,nrow=length(Grange))
  BICunadjusted = matrix(modelsUnadjustedBIC,nrow=length(Grange))
  numParams = matrix(modelsNumParams, nrow=length(Grange))
  loglik = matrix(modelsLoglik, nrow=length(Grange))
  colnames(BIC)<-modelNames
  row.names(BIC)<-Grange
  colnames(BICunadjusted)<-modelNames
  row.names(BICunadjusted)<-Grange
  return(list(selectedModel=indivModels[whichIsBest,],
              BIC=BIC,
              BICunadjusted=BICunadjusted,
              allModels=modelsBIC, 
              z = cclustRuns[[whichIsBest]]$z, 
              parameters=cclustRuns[[whichIsBest]]$parameters,classes=classes,
         numParams=numParams,
         loglik=loglik,
         initPart=modelsInitPart,
         modelsClasses=modelsClasses))
}
