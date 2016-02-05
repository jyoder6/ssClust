\name{ssClust}
\alias{ssClust}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ssClust
}
\description{
Performs GMM using some supervision
}
\usage{
ssClust(X, knownLabels = NULL, trueLabels = NULL,
                 knownCannotLink = NULL, cannotLinkWithIdx = NULL,
                 Grange = 2, modelNames = c("VVV"), runParallel = TRUE,
                 fracOfCores2Use = 1, initClassAssignments = NULL,
                 initializationStrategy = "kpp",
         penalizeSupervised=T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
data
}
  \item{knownLabels}{
vector of indices of rows of x whose labels are known
}
  \item{trueLabels}{
length nrow(x) vector of true labels (only trueLabels[knownLabels] matter)
}
 \item{knownCannotLink}{
  vector of indices of data with cannot link with constraints
}
\item{cannotLinkWithIdx}{
	cannotLinkWithIdx[[``i"]] = data i cannot link with these indices (in vector)

	ex:
	Suppose cannotLinkWithIdx[[``2"]]=c(1,3).
	Then, 2 cannot link with 1 and 3.

	Constructed with, for example:
	
	knownCannotLink = c(3,4,5,6,7,8)
	
	cannotLinkWithIdx = new.env()
	
	for(i in knownCannotLink) cannotLinkWithIdx[[as.character(i)]]=c(1,2)


}

  \item{Grange}{
number of clsuters considered
}
  \item{modelNames}{
models considered
}
  \item{runParallel}{
boolean for whether to run in parallel
}
  \item{fracOfCores2Use}{
Fraction of all cores to use (if in parallel)
}
  \item{initClassAssignments}{
Hard initial class assignment which overrides the default hiearchical clustering initialization scheme.
}
\item{initializationStrategy}{Strategy to initialize cluster labels for EM algorithm.  Currently only supports kpp for semi-supervised k-means++}

\item{penalizeSupervised}{Boolean for whether or not to include supervised data in the penalty term in the BIC.  Defaults to TRUE.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{    
         \item{selectedModel}{Model selected (num components and covariance constraints)}
         \item{BIC}{Bayesian Information Criteria values for all models, adjusted for alternative penalty (dont use this number unless you know what you're doing))}
         \item{BICunadjusted}{Traditional Bayesian Information Criteria for all models}
         \item{allModels}{BIC and likelihood for all models}
         \item{z}{posterior probabilities of class memberships}
         \item{parameters}{winning model's parameters, like Mclust's parameters.  
         	
         	Item:pro--mixing parameters
         	Item: mean--means
         	Item: variance--variance of the form mclustVariance}
         	\item{classes}{vector of class assignments for winning model}
         \item{numParams}{number of parameters estimated for all models}
         \item{loglik}{loglik of winning model}
         \item{initPart}{initial partition}
         \item{modelsClasses}{matrix of class assignments for all models considered (generally ignore this)}
         
         
}
\references{
http://www.stat.washington.edu/mclust/
}
\author{
Jordan Yoder
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{



library('MASS')
library('mclust')

########### GENERATE DATA#######
n = 500 #total number of data points
C = 3 #total number of clusters
c = 5 #total number of labeled data point
meanOfControlCluster=-1
varOfControlCluster = 1/5
trueLabels = sort(sample(1:C,n,replace=TRUE)-1) #equal prior proportions

#generate the data
tableLabel = table(trueLabels)
cumSumTableLabel = cumsum(tableLabel)
cumSumTableLabel = c(0,cumSumTableLabel)
dataDim = 5
X = matrix(0, n,dataDim)
#save the true parameters
trueMeans = NULL
trueCovMat = NULL
spreadFactor=5 
trueMeans[[1]]=rep(meanOfControlCluster,dataDim)
trueMeans[[2]]=rep(1,dataDim)
trueMeans[[3]]=rep(-1,dataDim)
trueCovMat[[1]]=diag(varOfControlCluster,dataDim)
trueCovMat[[2]]=diag(1,dataDim)
trueCovMat[[3]]=diag(1,dataDim)
for(i in 1:C){
  X[(cumSumTableLabel[i]+1):(cumSumTableLabel[i+1]),] = mvrnorm(n=tableLabel[i],mu=trueMeans[[i]],Sigma=trueCovMat[[i]])
}

##### CLUSTER USING ssClust and Mclust ######
mclust.out <- Mclust(X,G = 2:5, modelNames = c('VVV','EEE','VVI','VII') )
ssClust.out<-ssClust(X,knownLabels=1:c,trueLabels=trueLabels,Grange=2:5,modelNames=c('VVV','EEE','VVI','VII'), runParallel=FALSE)
retVal <- c(adjustedRandIndex(mclust.out$cl[-(1:c)],trueLabels[-(1:c)]),
            adjustedRandIndex(ssClust.out$cl[-(1:c)],trueLabels[-(1:c)]))
retVal <- ifelse(is.nan(retVal),-1,retVal)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ GMM }
\keyword{ semi-supervised }% __ONLY ONE__ keyword per line
