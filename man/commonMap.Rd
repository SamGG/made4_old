\name{commonMap}
\alias{commonMap}

\title{Highlight common points between two 1D plots }
\description{
CommonMap draws two 1D plots, and links the common points between the two. 
}
\usage{
commonMap(x, y, hor=TRUE, cex=1.5, scaled=TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The coordinates of the first axis }
  \item{y}{ The coordinates of the second axis}
  \item{hor}{Logical, whether a horizontal line should be drawn on plot. Default is TRUE.}
  \item{cex}{Numeric. The amount by which plotting text
          and symbols should be scaled relative to the default }
  \item{scaled}{Logical, whether the data in x and y are scaled. Scaling is useful for 
     visualising small or large data values. Set to FALSE if actually or true values should 
     be visualised. The default is TRUE.}
  \item{\dots}{further arguments passed to or from other method }
}
\details{
Useful for mapping the genes in common from coinertia analysis 
This graphs a 1D graph, x and y are the coordinates from two different analyses
but the rows of each vectors correspond (ie common genes)

}
\value{
}
\references{ }
\author{ Ailis Fagan and Aedin Culhane }
\note{
This is useful for examining common points in axes from coinertia analysis, or comparing results 
from two different analysis. }

\seealso{See also \code{\link[made4:between.graph]{between.graph}}, 
\code{\link[made4:graph1D]{graph1D}}

}
\examples{
a<-rnorm(20)
b<-rnorm(20)
par(mfrow=c(2,2))
commonMap(a,b)
commonMap(a,b,hor=FALSE, col="red", pch=19)
commonMap(a,b,col="blue", cex=2, pch=19)

# If the vectors contain different variables, the rows should define the variables that correspond
a[15:20]<-NA
b[10:15]<-NA
cbind(a,b)
commonMap(a,b, col="dark green", pch=18)

}
\keyword{multivariate }% at least one, from doc/KEYWORDS
\keyword{hplot}% __ONLY ONE__ keyword per line
