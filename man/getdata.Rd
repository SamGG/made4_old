\name{getdata}
\alias{getdata}
\title{Check dataset}
\description{
  This function is called by array2ade4 which converts input data into a 
  data.frame suitable for analysis in ADE4. This function is not normally 
  called by the user 
}
\usage{
getdata(arraydata)
}
\arguments{
  \item{arraydata}{ a \code{\link{matrix}}, \code{\link{data.frame}}, 
     \code{\link[Biobase:exprSet-class]{exprSet}} or \code{\link[marrayClasses:marrayRaw-class]{marrayRaw}}.  
     If the input is gene expression data in a \code{\link{matrix}} or \code{\link{data.frame}}. The 
     rows and columns are expected to contain the variables (genes) and cases (array samples) 
     respectively.}
}
\details{
\code{\link[made4:bga]{bga}} and other functions in made4 call this function and it is
generally not necessary to call this directly.

\code{\link[made4:array2ade4]{array2ade4}} calls \code{\link[made4:getdata]{getdata}}, and 
will accept a \code{\link{matrix}},\code{\link{data.frame}}, \code{\link[Biobase:exprSet-class]{exprSet}} 
or \code{\link[marrayClasses:marrayRaw-class]{marrayRaw}} format. It will also transpose data or add a
integer to generate a positive data matrix. If the input data contains NA, these must first be removed 
or imputed (see the R libraries impute() or pamr()).  \code{getdata} is very similar 
to \code{\link[vsn:getIntensityMatrix]{getIntensityMatrix}} in \code{\link[vsn:vsn]{vsn}}.
}
\value{
 Returns a data.frame suitable for analysis by ade4 or made4 functions.
}
\author{Aedin Culhane }
\note{ This is not normally called by a user. Internal function. }
\seealso{ \code{\link[made4:array2ade4]{array2ade4}}, \code{\link[vsn:getIntensityMatrix]{getIntensityMatrix}}}
\examples{}

\keyword{internal}% at least one, from doc/KEYWORDS

