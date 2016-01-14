\name{svd.extract}
\alias{svd.extract}

\title{
svd.extract
}
\description{
Get scaled eigenvectors of adjacency matrix}
\usage{
svd.extract(A, dim = NULL, scaling = TRUE)
}
\arguments{
  \item{A}{ square adjacency matrix
 }
  \item{dim}{
dimension of embedding
}
  \item{scaling}{
boolean:  should eigenvectors be scaled?  (yes)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Scaled eigenvectors.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Youngser Park
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
library(irbla)
}
\examples{
svd.extract(matrix(runif(60)>.5,nrow=10,ncol=6),dim=2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ adjacency spectral embedding }
\keyword{ irbla}% __ONLY ONE__ keyword per line
