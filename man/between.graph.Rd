\name{between.graph}
\alias{between.graph}

\title{Plot 1D graph of results from between group analysis }
\description{
Plots a 1D graph, of results of between group analysis similar to that in Culhane et al., 2002. 
}
\usage{
between.graph(x, ax = 1, cols = NULL, hor = TRUE, scaled=TRUE, centnames=NULL, varnames=NULL, ...)
}

\arguments{
  \item{x}{Object of the class \code{bga} resulting from a \code{\link[made4:bga]{bga}} analysis. 
   }
  \item{ax}{Numeric. The column number of principal component (\$ls and \$li) to be
    used. Default is 1. This is the first component of the analysis.}
  \item{cols}{Vector of colours. By default colours are obtained
    using \code{\link[made4:getcol]{getcol}} }
  \item{hor}{Logical, indicating whether the graph should be plotted
   horizontally or vertically. The default is a horizontal plot.}
  \item{scaled}{Logical, indicating whether the coordinates in the graph 
   should be scaled to fit optimally in plot. Default is TRUE}
  \item{centnames}{A vector of variables labels. Default is NULL, if NULL the row names of the centroid \$li coordinates will be used.}
  \item{varnames}{A vector of variables labels. Default is NULL, if NULL the row names of the variable \$ls coordinates will be used.}
  \item{\dots}{further arguments passed to or from other methods }
}
\details{
This will produce a figure similar to Figure 1 in the paper by Culhane et al., 2002.

\code{between.graph} requires both samples and centroid co-ordinates (\$ls, \$li) which are passed 
to it via an object of class \code{bga}.   If cases are to be coloured by class, it also requires 
a \$fac \code{factor}  which is also passed to it via an object of class \code{bga}.


To plot a 1D graph from other multivariate analysis such as PCA (\code{\link[ade4:dudi.pca]{dudi.pca}}), 
COA (\code{\link[ade4:dudi.coa]{dudi.coa}}), or \code{\link[ade4:coinertia]{coinertia}} analysis. Please 
use \code{\link[made4:graph1D]{graph1D}}.

}
\value{

}
\references{
Culhane AC, et al., 2002 Between-group analysis of microarray data. Bioinformatics. 18(12):1600-8. 
}
\author{Aedin Culhane}
\note{}
\seealso{\code{\link[made4:graph1D]{graph1D}}
 }
 \examples{
data(khan)
if (require(ade4, quiet = TRUE)) {
khan.bga<-bga(khan$train, khan$train.classes)
}

between.graph(khan.bga)
between.graph(khan.bga, ax=2, lwd=3, hor=FALSE, cex=0.5, col=c("green", 
"blue", "red", "yellow"))

}
\keyword{multivariate }
\keyword{hplot}
