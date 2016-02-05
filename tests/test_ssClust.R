library(ssClust)
library(unittest, quietly=F)
library('MASS')
set.seed(234134)
n=200
C=3
trueLabels = sort(sample(1:C,n,replace=T)-1)#cut(1:n,C,labels=F)-1 #equal pro

#generate the data
tableLabel = table(trueLabels)
cumSumTableLabel = cumsum(tableLabel)
cumSumTableLabel = c(0,cumSumTableLabel)
dataDim = 5
X = matrix(0, n,dataDim)
#save the true parameters
trueMeans = NULL
trueCovMat = NULL
trueMeans[[1]]=rep(0,dataDim)
trueMeans[[2]]=rep(10,dataDim)
trueMeans[[3]]=rep(-10,dataDim)
trueCovMat[[1]]=diag(1,dataDim)
trueCovMat[[2]]=diag(1,dataDim)
trueCovMat[[3]]=diag(1,dataDim)
for(i in 1:C){
  X[(cumSumTableLabel[i]+1):(cumSumTableLabel[i+1]),] = mvrnorm(n=tableLabel[i],mu=trueMeans[[i]],Sigma=trueCovMat[[i]])
}  
###

### check parallel
knownLabels = 1:10
ss<-ssClust(X,knownLabels=knownLabels,trueLabels=trueLabels,Grange=2:5,modelNames=c('VVV','EEE','VVI','VII'))
ok(all(ss$cl[knownLabels]==(trueLabels[knownLabels]+1)), 'all known labels are correct (G=2:5,nSup=10, parallel)')
ok(adjustedRandIndex(ss$cl, trueLabels)==1, 'all labels are correct on easy problem (G=2:5,nSup=10, parallel)')


ss<-ssClust(X,knownLabels=NULL,trueLabels=trueLabels,Grange=2:5,modelNames=c('VVV','EEE','VVI','VII'))
ok(adjustedRandIndex(ss$cl, trueLabels)==1, 'all labels are correct on easy problem (G=2:5,nSup=0, parallel)')

## check not parallel
ss<-ssClust(X,knownLabels=knownLabels,trueLabels=trueLabels,Grange=2:5,modelNames=c('VVV','EEE','VVI','VII'), runParallel=F)
ok(all(ss$cl[knownLabels]==(trueLabels[knownLabels]+1)), 'all known labels are correct (G=2:5,nSup=10, not parallel)')
ok(adjustedRandIndex(ss$cl, trueLabels)==1, 'all labels are correct on easy problem (G=2:5,nSup=10, not parallel)')


ss<-ssClust(X,knownLabels=NULL,trueLabels=trueLabels,Grange=2:5,modelNames=c('VVV','EEE','VVI','VII'), runParallel=F)
ok(adjustedRandIndex(ss$cl, trueLabels)==1, 'all labels are correct on easy problem (G=2:5,nSup=0,  not parallel)')

## check with random initial partition
ss<-ssClust(X,knownLabels=NULL,trueLabels=trueLabels,Grange=2:5,modelNames=c('VVV','EEE','VVI','VII'), runParallel=F, initClassAssignments=sample(trueLabels))
ok(adjustedRandIndex(ss$cl, trueLabels)==1, 'all labels are correct on easy problem (G=2:5,nSup=0,  not parallel, initial part. given)')

## check cannot link
knownLabels = c(1,2)
knownCannotLink = c(3,4,5,6,7,8)
cannotLinkWithIdx = new.env()
for(i in knownCannotLink) cannotLinkWithIdx[[as.character(i)]]=knownLabels
ss<-ssClust(X,knownLabels=knownLabels,
            knownCannotLink=knownCannotLink,
            cannotLinkWithIdx=cannotLinkWithIdx,
            trueLabels=trueLabels,Grange=2:5,modelNames=c('VVV','EEE','VVI','VII'), runParallel=F)
ok(all(ss$cl[knownLabels]==(trueLabels[knownLabels]+1)), 'all known labels are correct (G=2:5,nSup=2, nCannotLink=6  not parallel)')
ok(all(!ss$cl[knownCannotLink] %in% trueLabels[knownLabels]), 'all labels are correct on easy problem (G=2:5,nSup=2, nCannotLink=6  not parallel)')