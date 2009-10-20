\name{forrwcoa}
\alias{forrwcoa}

\title{Row weighted Correspondence Analysis}
\description{
\code{dudi.rwcoa} Row weighted COA, calls \code{forrwcoa} to perform row
weighted correspondence analysis.
}
\usage{
forrwcoa(df, rowweights = rep(1/nrow(df),nrow(df)))
}

\arguments{
  \item{df}{a \code{data.frame} containing positive or null values. It should not
    contain missing (NA) values. }
  \item{rowweights}{ a vector of row weights (by default, uniform row weights) }
  \item{\dots}{further arguments passed to or from other methods ) }
}
\details{
Performs row weighted COA. Calls \code{forrwcoa} to calculates weights.
}
\value{
Returns a list of class 'coa', 'rwcoa', and 'dudi' (see \code{\link[ade4:dudi]{dudi}}) 
}
\references{ Culhane AC, et al., 2003 Cross platform comparison and visualisation of gene expression data using co-inertia analysis.  BMC Bioinformatics. 4:59  }
\author{ Aedin Culhane,  A.B. Dufour }
\note{ In the paper by Culhane et al., 2002, coinertia analysis
  was performed with two COAs, a standard \code{\link[ade4:dudi.coa]{COA}} and a row weighted COA \code{dudi.rwcoa}, on
  the two gene expression datasets. However it is now recommended to perform
  two non-symmetric COA, instead of two COA. This avoids having to force
the row weights from one analysis on the second.  To perform
non-symmetric correspondence coinertia analysis, use \code{\link[made4:bet.coinertia]{bet.coinertia}}.}


 \seealso{ See Also as
   \code{\link[ade4:dudi]{dudi}},\code{\link[ade4:dudi.coa]{dudi.coa}},\code{\link[ade4:dudi.pca]{dudi.pca}}
 \code{\link[made4:bet.coinertia]{bet.coinertia}} }
\examples{

}
\keyword{internal}

