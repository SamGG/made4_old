
\name{genes1d}
\alias{genes1d}
\title{genes1d: list top N variables on one axis. Lists the maximum and minimum 
values in a column of a data.frame}
\description{
Lists the top N variables from the positive and negative ends of an
axis. Returns a list of N variables that have the max and min. Given a
\$co or \$li file it will return that variables at the ends of the axis. 
}
\usage{
genes1d(array, n = 5, axis = 1, listgenes = FALSE)
}
\arguments{
  \item{array}{A \code{\link{matrix}} or
    \code{\link{data.frame}}. Typically a \$co or \$li from 
    \code{\link[made4:bga]{bga}}, \code{\link[made4:cia]{cia}} or 
    \code{\link[ade4:dudi]{dudi}}.}
  \item{n}{An integer indicating the number of variables to be
    returned. Default is 5. }
  \item{axis}{An integer indicating the column of x. Default is 1
    (first axis, of \$co or \$li file) }
  \item{listgenes}{A logical, If TRUE a list containing the variables 
        at each ends of the axes are returned}
  \item{\dots}{further arguments passed to or from other methods }
}
\details{
Genes1d is similar to \code{link[made4:genes]{genes}}, but returns 
an index of genes at the ends of one axes.
}
\value{
Returns a vector or list of vectors.
}
\references{ }
\author{Aedin Culhane}

\seealso{See Also as \code{\link[made4:genes]{genes}}}
\examples{
data(khan)
if (require(ade4, quiet = TRUE)) {
khan.coa<-ord(khan$train)
}
ind<-topgenes(khan.coa, ends="pos")
ind.ID<-topgenes(khan.coa, ends="pos", labels=khan$gene.labels.imagesID)
ind.symbol<-topgenes(khan.coa, ends="pos", labels=khan$annotation$Symbol)
Top10.pos<- cbind("Gene Symbol"=ind.symbol, 
"Clone ID"=ind.ID, "Coordinates"=khan.coa$ord$co[ind,], row.names=c(1:length(ind)))
Top10.pos
}
\keyword{internal}
