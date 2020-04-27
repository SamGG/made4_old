\name{array2ade4}
\alias{array2ade4}
\title{Converts microarray input data into a data frame suitable for analysis in ADE4.}
\description{
  Converts input data into a data.frame suitable for analysis
  in ADE4. This function is called by \code{bga} and other made4
  function
}
\usage{
array2ade4(dataset, pos = FALSE, trans = FALSE)
}
\arguments{
  \item{dataset}{A \code{\link{matrix}}, \code{\link{data.frame}}, 
     \code{\link[Biobase:ExpressionSet-class]{ExpressionSet}} or
     \code{\link[marray:marrayRaw-class]{marrayRaw-class}}.  
     If the input is gene expression data in a \code{\link{matrix}} or \code{\link{data.frame}}. The 
     rows and columns are expected to contain the variables (genes) and cases (array samples) 
     respectively.
     }
  \item{pos}{Logical indicating whether to add an integer to \code{dataset}, to generate positive \code{data.frame}.
    Required  for \code{dudi.coa} or \code{dudi.nsc}.}
  \item{trans}{Logical indicating whether \code{dataset} should be transposed. Default is \code{FALSE}.}
}
\details{
\code{\link[made4:bga]{bga}} and other functions in made4 call this function and it is generally \strong{not} necessary to call \code{array2ade4} this directly.

array2ade4 calls \code{\link[made4:getdata]{getdata}}, and will accept a \code{\link{matrix}}, \code{\link{data.frame}}, 
\code{\link[Biobase:ExpressionSet-class]{ExpressionSet}} or \code{\link[marray:marrayRaw-class]{marrayRaw-class}} format.
It  will also transpose data or add a integer to generate a positive data matrix. 

If the input data contains missing values (NA), these must first be removed or imputed (see the R libraries impute() or pamr()).  
}
\value{
 Returns a data.frame suitable for analysis by ade4 or made4 functions.
}
\references{ }
\author{Aedin Culhane}
\seealso{\code{\link[made4:getdata]{getdata}},\code{\link[convert:coerce]{as}} in the Bioconductor package convert.}
\examples{
data(geneData)
class(geneData)
dim(geneData)
dim(array2ade4(geneData))
class(array2ade4(geneData))
}
\keyword{manip}% at least one, from doc/KEYWORDS
