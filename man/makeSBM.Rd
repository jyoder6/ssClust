\name{makeSBM}
\alias{makeSBM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
makeSBM
}
\description{
Generates a realization Stochastic Block Model with information for supervised labels.
}
\usage{
makeSBM(n, m, Lam)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
vector of unsupervised block sizes
}
  \item{m}{
vector of supervised block sizes
}
  \item{Lam}{
interblock link probability matrix
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sbm }

