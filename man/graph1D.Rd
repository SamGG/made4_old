\name{graph1D}
\alias{graph1D}
\title{Plot 1D graph of axis from multivariate analysis}
\description{
 Draw 1D plot of an axis from multivariate analysis. Useful for visualising an individual axis from 
 analyses such as PCA \code{\link[ade4:dudi.pca]{dudi.pca}} or COA \code{\link[ade4:dudi.coa]{dudi.coa}}. 
 It accepts a factor so that groups of points can be coloured.
 It can also be used for graphing genes, and will only label n genes at the ends of the axis.
 }


\usage{graph1D(dfx,  classvec=NULL,ax = 1, hor=FALSE, s.nam=row.names(dfx), n=NULL,
       scaled=TRUE, col="red", width=NULL, ...)}
\arguments{
\item{dfx}{\code{\link{vector}}, \code{\link{matrix}}, or \code{\link{data.frame}}, which contains a column 
 with axis coordinates}
\item{ax}{Numeric, indicating column of \code{\link{matrix}}, or \code{\link{data.frame}} to be plotted. 
  The default is 1.}
\item{classvec}{Factor, indicating sub-groupings or classes in dfx or dfx[,ax]}
\item{hor}{Logical, indicating whether the graph should be drawn horizontal or vertically. 
 The default is vertically.}
\item{s.nam}{Vector. labels of dfx, The default is row.names(dfx)}
\item{n}{Numeric. Whether all rows should be plotted, n=10 would label only the 10 variables at the
end of the axis.  By default all variables (row of dfx) are labelled}
\item{scaled}{A logical indicating whether the plot should be scaled to fit. The default is TRUE}
\item{col}{A character or vector indicating the colour(s) for points or groups of points. If points
are to be coloured according to a factor, length(col) should equal length(levels(classvec))}
\item{width}{A vector of length 2, which is the width (of a vertical plot) or height (of a 
horizontal plot). This can be increased if variable labels are unreadable. The default is
c(-2,1)}
\item{\dots}{further arguments passed to or from other methods }
}
\details{
}
\value{

}
\references{  }
\author{ Aedin Culhane }
\note{}
\seealso{ \code{between.graph}}
\examples{
a<-rnorm(25)
graph1D(a, s.nam=letters[1:25])
graph1D(a, s.nam=letters[1:25], col="blue", pch=19,  n=3)
data(khan)
if (require(ade4, quiet = TRUE)) {
khan.coa<-dudi.coa(khan$train, scan=FALSE, nf=2)
}
graph1D(khan.coa$co, ax=1)
}
\keyword{hplot }% at least one, from doc/KEYWORDS
\keyword{manip }% __ONLY ONE__ keyword per line
