\name{bga.jackknife}
\alias{bga.jackknife}
\title{Jackknife between group analysis}
\description{
Performs one-leave-out jackknife analysis of a between group analysis as
described by Culhane et al., 20002
}
\usage{
bga.jackknife(data, classvec, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{Input dataset. A \code{\link{matrix}}, \code{\link{data.frame}}  
     If the input is gene expression data in a \code{\link{matrix}} or \code{\link{data.frame}}. The 
     columns contain the cases (array samples) which will be jackknifed.}
  \item{classvec}{ A factor or vector which describes the classes in the
  training dataset}
  \item{\dots}{ further arguments passed to or from other methods}
}
\details{
Performs a one-leave-out cross validation of between group analysis \code{\link[made4:bga]{bga}}. 
Input is a training dataset.  This can take 5-10 minutes to compute on standard data gene expression matrix.

In jackknife one leave out analysis, one case (column) is removed. The remaining dataset is subjected to 
\code{\link[made4:bga]{bga}}.  Then the class of the case that was removed is predicted using \code{\link[made4:suppl]{suppl}}.
This analysis is repeated until all samples have been removed and predicted.}
\value{A list containing 
  \item{results}{The projected co-ordinates of each sample}
  \item{summary}{A summary of number and percentage of correctly assigned samples}}
\references{Culhane et al., 2002 Between-group analysis of microarray data. Bioinformatics. 18(12):1600-8.}
\author{ Aedin Culhane }

\seealso{See Also  \code{\link[made4:bga]{bga}},
  \code{\link[made4:bga.suppl]{bga.suppl}},
  \code{\link[made4:suppl]{suppl}},\code{\link[made4:bga]{bga}},
  \code{\link[ade4:between]{between}},
  \code{\link[made4:plot.bga]{plot.bga}}}
\examples{
data(khan)
# NOTE using a very reduced dataset (first 5 genes) to speed up results
# hence expect poor prediction accuracy
dim(khan$train)
print("using only small subset of data")
if (require(ade4, quiet = TRUE)) {
bga.jackknife(khan$train[1:5,], khan$train.classes) }
}
\keyword{manip}% at least one, from doc/KEYWORDS
\keyword{multivariate}

