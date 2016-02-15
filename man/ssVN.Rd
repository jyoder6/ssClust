\name{ssVN}
\alias{ssVN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Semi-superved Vertex Nomination
}
\description{
Uses information about RED and NOT RED memberships to nominate RED vertices.
}
\usage{
ssVN(adj, embeddingDim, knownRed, knownNotRed, 
  initializationStrategy = "kpp", Grange = c(2:5))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{adj}{
Adjacency matrix
}
  \item{embeddingDim}{
Scalar dimension to embed the adjacency matrix into
}
  \item{knownRed}{
indices of the vertices that are known to be red
}
  \item{knownNotRed}{
indices of the vertices that are known NOT to be red}
  \item{initializationStrategy}{
currently only accepts "kpp" for semi-supervised k-means++
}
  \item{Grange}{
number of classes to consider (2 minimum for red and a single not red class)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{redRanking}{vector of posterior probabilities of being red.  Supervised points are easy money (0 or 1, respectively)}
\item{redDist}{vector of mahalanobis distance to red center}
\item{ss}{object as returned by the call to ssClust (see documentation in this package)}}
\references{
%% ~put references to the literature/web site here ~
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
library(ssClust)
library(igraph)
n=500
MCR=100
numNumKnownNotRed <- 5
numKnownNotRedPerJ <- 20
j=6
numKnownNotRed <- (j-1)*numKnownNotRedPerJ

###### FORM ADJ MATRIX #####
#matching Li's simulation parameters
rho <- c(0.4,0.3,.3)
B1 <- matrix(c(0.5,0.3,.4,
               0.3,0.8,.6,
               .4,0.6, .3),  nrow = 3,ncol=3)
B2 <- matrix(.5 ,nrow = 3,ncol=3)
if(n==10)
  nu=1
if(n==500)
  nu = .3
if(n==10000)
  nu = .1
B = nu*B1 + (1-nu)*B2
blockSizes <- n*rho

#simulate graph
A.igraph<- sbm.game(n, pref.matrix = B, block.sizes = blockSizes, 
  directed = FALSE, loops = FALSE)
###### END FORM ADJ MATRIX ########

#shovel into obj we need
A <- NULL
A$adj <- get.adjacency(A.igraph)

#tau is actual labels
A$tau <- rep(1,blockSizes[1])
A$tau <- c(A$tau, rep(2, blockSizes[2]))
A$tau <- c(A$tau, rep(3, blockSizes[3]))
redGroup = min(A$tau)


#semi-supervise
if(n ==10)
  knownRed <- sample(which(A$tau==redGroup),4)
if(n ==500)
  knownRed <- sample(which(A$tau==redGroup),20)
if(n ==10000)
  knownRed <- sample(which(A$tau==redGroup),40)

#more semi-supervision
knownNotRed <- sample(which(A$tau!=1),
                      numKnownNotRed)

#cluster and nominate
ss = ssVN(adj=A$adj,
          embeddingDim=3,
          knownRed = knownRed,
          knownNotRed = knownNotRed,
          initializationStrategy = "kpp",
          Grange=2:5)
library(ROCR)
pred = prediction(ss$redRanking, A$tau==redGroup)
perf = performance(pred, 'tpr','fpr')
plot(perf)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Vertex Nomination }
\keyword{ Semi-supervised }% __ONLY ONE__ keyword per line
