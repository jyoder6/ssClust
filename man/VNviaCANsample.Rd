\name{VNviaCANsample}
\alias{VNviaCANsample}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Semi-supervised Vertex Nomination using Canonical Sampling
}
\description{
Semi-supervised Vertex Nomination using Canonical Sampling
}
\usage{
VNviaCANsample(n, m, Lam, A, observe, truth, numburn, numsample)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
   Vector of number of unsupervised datapoints per block.
}
  \item{m}{
Vector of number of supervsised datapoints per block.
}
  \item{Lam}{
Interblock link probability matrix
}
  \item{A}{
Adjacency matrix (undirected, hollow)
}
  \item{observe}{
length sum(n+m)  vector of known block identities.  If -1, unknown.  If >=1, that is the block identity.
}
  \item{truth}{
length sum(n+m)  vector of true block identities.
}
  \item{numburn}{
Integer number of swaps to perform before sampling occurs.
}
  \item{numsample}{
Integer number of swaps to perform while sampling occurs.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
probs = probs,avgprec = avgprec,reveal = reveal
\item{probs}{Length sum(n) vector of estimated probability of block 1 membership, sorted in nomination order}
\item{avgprec}{Average precision (at n(1)).  Higher is better.  1 is the best.}
\item{reveal}{boolean vector of length sum(n) of whether or not the i^th vector in the nomination order is from block 1.}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
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
numVert=500 #number of ambiguous vertices

#matching Li's simulation parameters
rho <- c(0.4,0.3,.3)
B1 <- matrix(c(0.5,0.3,.4,
               0.3,0.8,.6,
               .4,0.6, .3),  nrow = 3,ncol=3)
B2 <- matrix(.5 ,nrow = 3,ncol=3)
nu = .3
Lam0 = nu*B1 + (1-nu)*B2

n0 <- numVert*rho
m0<-c(20, 0, 0)
numburn0 = 10^4
numsample0 = 10^4

sbm.out = makeSBM(n0,m0,Lam0)
  A0 = as.matrix(sbm.out$A)
  observe0 = sbm.out$observe
  truth0 = sbm.out$truth


canSamp.out = VNviaCANsample(n=n0, m=m0, Lam=Lam0, A=A0, 
                               observe=observe0, 
                               truth=truth0, 
                               numburn=numburn0, 
                               numsample=numsample0)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
