\name{randomiser}
\alias{randomiser}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Randomly reassign training and test samples }
\description{
This function is used to check for bias between a training and test
data. It return a new index, which randomly re-assigns samples in the training data to the test
dataset and vice versa.
}
\usage{
randomiser(ntrain = 77, ntest = 19)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ntrain}{Numeric. A integer indicating the number of cases in
    the training dataset }
  \item{ntest}{Numeric. A integer indicating the number of cases in
    the test dataset }
}
\details{
Produces new indices that can be used for training/test datasets
}
\value{
It returns a list, containing 2 vectors
  \item{train }{A vector of length ntrain, which can be used to index a
    new training dataset}
  \item{test }{A vector of length ntest, which can be used to index a
    new test dataset}

}

\author{Aedin Culhane }

\examples{
randomiser(10,5)
train<-matrix(rnorm(400), ncol=20, nrow=20, dimnames=list(1:20,
paste("train",letters[1:20], sep=".")))
test<-matrix(rnorm(200), ncol=10, nrow=20, dimnames=list(1:20,
paste("test",LETTERS[1:10], sep=".")))
all<-cbind(train,test)

colnames(train)
colnames(test)
newInd<-randomiser(ntrain=20, ntest=10)

newtrain<-all[,newInd$train]
newtest<-all[,newInd$test]

colnames(newtrain)
colnames(newtest)

}
\keyword{ manip }% at least one, from doc/KEYWORDS

