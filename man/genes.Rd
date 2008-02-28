\name{genes}
\alias{genes}
\title{Returns an index of the top N variables at the
  ends of the x and y axes}
\description{
 Returns an index of the top n maximum and minimum 
 values in 2 columns of a matrix or data.frame.
}
\usage{
genes(dudivar,n=5, axis1=1, axis2=2)
}
\arguments{
  \item{dudivar}{a \code{data frame} or \code{matrix} with at least two
    columns, containing the x,y coordinates.}
  \item{axis1}{the column number for the x-axis.}
  \item{axis2}{the column number for the y-axis.}
  \item{n} {n is the number of genes to be extracted.}
}
\details{
Genes returns an index of the "top" variables at the ends of the x and y axes.  
Genes is not normally called by the user but is called by plotgenes()

If you wish to return a table or list of the top genes at
the end of an axis, use the function \code{\link[made4:topgenes]{topgenes}}.

Thanks to Miss Jess Mar for helping on this function.
}
\value{}
\author{Aedin Culhane}
\seealso{See Also as \code{\link[made4:plotgenes]{plotgenes}} 
}
\examples{
}
\keyword{internal}
