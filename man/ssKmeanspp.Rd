\name{ssKmeanspp}
\alias{ssKmeanspp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ssKmeanspp
}
\description{
Semi-supervised k-means++ (ss-k-means++ initialiation and Lloyd's algorithm).
}
\usage{
ssKmeanspp(data, k, knownLabels, trueLabels, iter.max=100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data
}
  \item{k}{
number of groups
}
  \item{knownLabels}{
vector of indices of rows of x whose labels are known
}
  \item{trueLabels}{
length nrow(x) vector of true labels (only trueLabels[knownLabels] matter)
}
  \item{iter.max}{
maximum number of LLoyd's iterations
}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{centers}{Final centers}
\item{cluster}{Final cluster assignments}
}
\references{
http://arxiv.org/abs/1602.00360
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
data = matrix(runif(20),nrow=10,ncol=2)
knownLabels=c(1,2)
trueLabels=rep(NA,10)
trueLabels[1:2] = c(1,0)

res = ssKmeanspp(data, k=2,
		knownLabels=knownLabels, trueLabels=trueLabels) #note trueLabels are 0-indexed


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{k-means++ }
\keyword{semi-supervised}% __ONLY ONE__ keyword per line
