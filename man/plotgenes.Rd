\name{plotgenes}
\alias{plotgenes}
\title{Graph xy plot of variable (gene) projections from PCA or COA. Only label variables at ends of axes}
\description{
Graph xy plot of variables but only label variables at ends of X and Y axes. Useful for graphing genes
coordinates (\$co) resulting from PCA or COA of microarray data.
}
\usage{
plotgenes(coord, nlab = 10, axis1 = 1, axis2 = 2, 
genelabels = row.names(coord), boxes = TRUE, 
colpoints = "black", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{coord}{a  \code{\link{data.frame}} or \code{\link{matrix}} or object from \code{ord}
    \code{bga} or \code{cia} analysis with at least two
    columns, containing x, y coordinates to be plotted}
  \item{nlab}{Numeric. An integer indicating the number of variables at ends of axes to
  be labelled}
  \item{axis1}{An integer, the column number for the x-axis. Default is 1, 
  so axis 1 is dudivar[,1]}
  \item{axis2}{An integer, the column number for the y-axis. Default is 2, 
   so axis 2 is dudivar[,2]}
  \item{genelabels}{A vector of gene (variable) labels. Default is row.names(coord)}
  \item{boxes}{A logical, indicating whether a box should be plotted surrounding
   each variable label. The default is TRUE.}
  \item{colpoints}{The colour of the points on the plot. The
    default is "black"}
  \item{\dots}{ further arguments passed to or from other method }
}
\details{
\code{plotgenes} calls the function \code{genes} which return an index of the "top"
variables at the ends of the x and y axes. 

If you wish to return a table or list of the top genes at
the end of an axis, use the function \code{\link[made4:topgenes]{topgenes}}.


}
\value{
An xy plot
}
\note{ 

\code{plotgenes} plots variables using \code{\link[made4:s.var]{s.var}}, which is a modified version 
of \code{\link[ade4:s.label]{s.label}}.} 


}
\author{ Aedin Culhane }
\seealso{See Also as \code{\link[made4:s.var]{s.var}} and \code{\link[ade4:s.label]{s.label}}}
\examples{
data(khan)
if (require(ade4, quiet = TRUE)) {
khan.bga<-bga(khan$train, khan$train.classes) 
}
attach(khan.bga)
par(mfrow=c(2,2))
s.var(bet$ls, col=as.numeric(khan$train.classes), clabel=0.8)
plotgenes(bet$co, colpoints="red")
plotgenes(bet$co, colpoints="red", genelabels=khan$annotation$Symbol)
plotgenes(bet$co, colpoints="gray", genelabels=khan$annotation$Symbol,
boxes=FALSE)
s.groups(bet$ls, khan$train.classes, add.plot=TRUE,
col=c(1:length(levels(khan$train.classes))) )

}
\keyword{multivariate }% at least one, from doc/KEYWORDS
\keyword{hplot}% __ONLY ONE__ keyword per line
