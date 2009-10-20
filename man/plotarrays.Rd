\name{plotarrays}
\alias{plotarrays}
\title{Graph xy plot of variable (array) projections from ordination, between group analysis or coinertia analysis.}
\description{
Graph xy plot of variables using s.var, s.groups or s.match.col. Useful for visualising array
coordinates (\$li) resulting from ord, bga or cia of microarray data.
}
\usage{
plotarrays(coord, axis1 = 1, axis2 = 2, arraylabels = NULL, classvec=NULL,  graph = c("groups", "simple", "labels", "groups2", "coinertia","coinertia2"), labelsize=1, star=1, ellipse=1, arraycol=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{coord}{a \code{\link{data.frame}} or \code{\link{matrix}} or object from \code{ord}
    \code{bga} or \code{cia} analysis with at least two
    columns, containing x, y coordinates to be plotted}
  \item{axis1}{An integer, the column number for the x-axis. Default is 1,
  so axes 1 is dudivar[,1]}
  \item{axis2}{An integer, the column number for the y-axis. Default is 2, 
   so axes 2 is dudivar[,2]}
  \item{arraylabels}{A vector of variables labels. Default is row.names(coord)}
  \item{classvec}{ A \code{factor} or \code{vector} which describes the classes in
  \code{coord}.  Default is NULL. If included variables will be coloured by class.}
 \item{graph}{A character of type  "groups", "simple", "labels", "groups2", "coinertia" or "coinertia2" which
  specifies the type of plot type or "graph" to be drawn. By default the  graph
  will be selected depending on the class of cooord, and whether a classvector is specified}
 \item{labelsize}{Size of sample labels, by default=1}
  \item{star}{If drawing groups, whether to join samples to centroid creating a "star"}
  \item{ellipse}{If drawing groups, whether to draw an ellipse or ring around the samples}
  \item{arraycol}{Character with length equal to the number of levels in the factor classvec. Colors for each of the levels in the factor classvec}
  \item{\dots}{ further arguments passed to or from other method }
}
\details{
\code{plotarrays} calls the function \code{s.var}, \code{s.groups} or \code{s.match.col}.

If you wish to return a table or list of the top array at
the end of an axis, use the function \code{\link[made4:topgenes]{topgenes}}.


}
\value{
An xy plot
}
\note{ 

\code{plotarrays} plots variables using \code{\link[made4:s.var]{s.var}}, \code{\link[made4:s.groups]{s.groups}},
\code{\link[made4:s.match.col]{s.match.col}} which are modifieds version
of \code{\link[ade4:s.label]{s.label}}, \code{\link[ade4:s.class]{s.class}}. , and
\code{\link[ade4:s.match]{s.match}}.


}
\author{ Aedin Culhane }
\seealso{See Also as \code{\link[made4:s.var]{s.var}} and \code{\link[ade4:s.label]{s.label}}}
\examples{
data(khan)
if (require(ade4, quiet = TRUE)) {
khan.bga<-bga(khan$train, khan$train.classes) 
}
attach(khan.bga)
par(mfrow=c(2,1))
plotarrays(khan.bga)
plotarrays(khan.bga, graph="simple")
plotarrays(khan.bga, graph="labels")
plotarrays(khan.bga, graph="groups")
plotarrays(khan.bga, graph="groups2")
}
\keyword{multivariate }% at least one, from doc/KEYWORDS
\keyword{hplot}% __ONLY ONE__ keyword per line
