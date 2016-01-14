\name{getClassGivenCenters}
\alias{getClassGivenCenters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
getClassGivenCenters
}
\description{
After obtaining centers, adhears to supervision constraints while getting class labels.
}
\usage{
getClassGivenCenters(data,centers,knownLabels=NULL,trueLabels=NULL, 
                               knownCannotLink=NULL,
                               cannotLinkWithIdx=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
data as a matrix.  Unknown if it works for 1d.
}
\item{centers}{
	list of centers of the classes
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
}
\details{
 This function was entirely internal (specialized to the case with RED and DEFINITELY NOT RED constraints).  It was originally called by the ssVN() function, which sets up the (admittably strange) datastructure for cannotLinkWithIdx.

   I have chosen to make it external for a bit more flexibility in my own experiments.  Use at own risk.
}
\value{
clss: length n vector of class labels
}
\references{

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
centers = kppInit(data, numGroups=3, knownLabels=c(1,2), trueLabels=c(0,1)) #note trueLabels are 0-indexed
knownCannotLink = c(3,4,5,6,7,8)
cannotLinkWithIdx = new.env()
for(i in knownCannotLink) cannotLinkWithIdx[[as.character(i)]]=c(1,2)
cl<-getClassGivenCenters(data,centers,knownLabels=c(1,2),trueLabels=c(0,1), 
                               knownCannotLink=c(3,4,5,6,7,8),
                               cannotLinkWithIdx=cannotLinkWithIdx)

cl[1]==0
cl[2]==1
all(cl[knownCannotLink] == 2) #must be cl2 because cannot link with data1 or data2, which are cl0 and cl1, respectively


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
