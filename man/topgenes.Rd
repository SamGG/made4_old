
\name{topgenes}
\alias{topgenes}
\title{Topgenes, returns a list of variables at the ends (positive, negative or both) of an axis}
\description{
\code{topgenes} will return a list of the top N variables from the positive, negative or both ends of an
axis. That is, it returns a list of variables that have the maximum and/or minimum values in 
a vector.
}
\usage{

topgenes(x, n = 10, axis = 1, labels = row.names(x), ends = "both", ...)
}
\arguments{
  \item{x}{A \code{\link{vector}}, \code{\link{matrix}} or
    \code{\link{data.frame}}. Typically a data frame \$co or \$li 
    from \code{\link[ade4:dudi]{dudi}} or  \$ls, \$li, \$co from 
    \code{\link[made4:bga]{bga}}.}
  \item{n}{An integer indicating the number of variables to be
    returned. Default is 5. }
  \item{axis}{An integer indicating the column of x. Default is 1
    (first axis, of \$co or \$li file) }
  \item{labels}{A vector of row names, for x[,axis]. Default values is
    row.names(x)}
  \item{ends}{A string, "both", "neg", "pos", indicating whether
    variable label should be return from both, the negative or the
    positive end of an axis.  The default is both.}
  \item{\dots}{further arguments passed to or from other methods }
}
\details{
\code{topgenes} calls \code{\link[ade4:genes1d]{genes1d}}. \code{genes1d} is similar to
\code{\link[made4:genes]{genes}}, but returns an index of genes at the ends of
one axis not two axes.  Given a \$co or \$li file it will return that variables at the ends of the axis.
}
\value{
Returns a vector or list of vectors.
}
\references{ }
\author{AedinCulhane}
\note{ }
\seealso{See Also as \code{\link[made4:genes]{genes}}}
\examples{

# Simple example
a<-rnorm(50)
order(a)
topgenes(a, labels=c(1:length(a)), ends="neg")

# Applied example
data(khan)
if (require(ade4, quiet = TRUE)) {
khan.coa<-dudi.coa(khan$train[1:100,1:10], scannf=FALSE, nf=2)
}
ind<-topgenes(khan.coa$li, ends="pos")
ind.ID<-topgenes(khan.coa$li, ends="pos", labels=khan$gene.labels.imagesID)
ind.symbol<-topgenes(khan.coa$li, ends="pos", labels=khan$annotation$Symbol)
Top10.pos<- cbind("Gene Symbol"=ind.symbol, 
      "Clone ID"=ind.ID, "Coordinates"=khan.coa$li[ind,], row.names=c(1:length(ind)))
Top10.pos
}
\keyword{manip}
