\name{comparelists}
\alias{comparelists}
\alias{print.comparelists}
\title{Return the intersect, difference and union between 2 vectors}
\description{
This is a very simple function which compares two vectors, x and y. It
returns the intersection and unique lists. It is useful for comparing two genelists.
}
\usage{
comparelists(dx,dy, ...)
print.comparelists(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dx,dy}{A vector}
  \item{x}{An object from \code{comparelists}}
  \item{\dots}{further arguments passed to or from other methods }
}
\details{
  reports on the intersect, difference and union between two lists.
}
\value{
 An object of class \code{comparelists}:

  \item{intersect}{Vector containing the intersect between x and y}
  \item{Set.Diff}{Vector containing the elements unique to X obtained using \code{\link[setdiff]{setdiff}} }
  \item{XinY}{Numeric, indicating the number of elements of x in y}
  \item{YinX}{Numeric, indicating the number of elements of y in x}
  \item{Length.X}{Numeric, the number of elements in x}
  \item{Length.Y}{Numeric, the number of elements in y}
  \item{\dots}{Further arguments passed to or from other methods }

}

\author{Aedin Culhane }
\seealso{See Also  \code{\link{intersect}}, \code{\link[setdiff]{setdiff}}  }
\examples{

a<-sample(LETTERS,20)
b<-sample(LETTERS,10)
z<-comparelists(a,b)
z$Set.Diff
z$intersect

}
\keyword{manip}% at least one, from doc/KEYWORDS

