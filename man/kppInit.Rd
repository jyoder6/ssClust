\name{kppInit}
\alias{kppInit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
kppInit
}
\description{
Performs k++ with semi-supervision
}
\usage{
kppInit(data,
                    numGroups,
                    knownLabels=NULL,
                    trueLabels=NULL
                    )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data
}
  \item{knownLabels}{
vector of indices of rows of x whose labels are known
}
  \item{trueLabels}{
length nrow(x) vector of true labels (only trueLabels[knownLabels] matter)
}
  \item{numGroups}{
number of clsuters considered
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
data = matrix(runif(20),nrow=10,ncol=2)
library(ssClust)
centers = kppInit(data, numGroups=3, knownLabels=c(1,2), trueLabels=c(1,0)) #note trueLabels are 0-indexed


all(centers[[1]]==data[2,]) #center 1 must be data2 because the true label of 2 is 0
all(centers[[2]]==data[1,]) #center 2 must be data1 because the true label of 1 is 1


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{k-means++ }
\keyword{semi-supervised}% __ONLY ONE__ keyword per line
