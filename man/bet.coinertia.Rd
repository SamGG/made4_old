\name{bet.coinertia}
\alias{bet.coinertia}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Between class coinertia analysis }
\description{
Between class coinertia analysis. \code{\link[made4:cia]{cia}} of 2 datasets where 
covariance between groups or classes of cases, rather than individual cases are maximised. 
}
\usage{
bet.coinertia(df1, df2, fac1, fac2, cia.nf = 2, type = "nsc", ...)
}

\arguments{
  \item{df1}{First dataset.A \code{\link{matrix}}, \code{\link{data.frame}}, 
     \code{\link[Biobase:exprSet-class]{exprSet}} or \code{\link[marrayClasses:marrayRaw-class]{marrayRaw}}.  
     If the input is gene expression data in a \code{\link{matrix}} or \code{\link{data.frame}}. The 
     rows and columns are expected to contain the variables (genes) and cases (array samples) 
     respectively.
}
  \item{df2}{Second dataset. A \code{\link{matrix}}, \code{\link{data.frame}}, 
     \code{\link[Biobase:exprSet-class]{exprSet}} or \code{\link[marrayClasses:marrayRaw-class]{marrayRaw}}.  
     If the input is gene expression data in a \code{\link{matrix}} or \code{\link{data.frame}}. The 
     rows and columns are expected to contain the variables (genes) and cases (array samples) 
     respectively.}
  \item{fac1}{A \code{factor} or \code{vector} which describes the classes in df1 }
  \item{fac2}{A \code{factor} or \code{vector} which describes the classes in df2}
  \item{cia.nf}{Integer indicating the number of coinertia analysis
    axes to be saved. Default value is 2.}
  \item{type}{A character string, accepted options are type="nsc" or type="pca" }
  \item{\dots}{ further arguments passed to or from other methods }
}

\value{
A list of class \code{bet.cia} of length 5

  \item{coin}{An object of class 'coinertia', sub-class \code{\link[ade4:dudi]{dudi}}. See
    \code{\link[ade4:coinertia]{coinertia}} }
  \item{coa1, pca1}{An object of class 'nsc' or 'pca', with sub-class
    'dudi'. See \code{\link[ade4:dudi]{dudi}},
      \code{\link[ade4:dudi.pca]{dudi.pca}} or \code{\link[ade4:dudi.nsc]{dudi.nsc}}}
  \item(coa2, pca2){An object of class 'nsc' or 'pca', with sub-class
    'dudi'. See \code{\link[ade4:dudi]{dudi}},
    \code{\link[ade4:dudi.pca]{dudi.pca}} or
    \code{\link[ade4:dudi.nsc]{dudi.nsc}}}
  \item{bet1}{An object of class 'bga', with sub-class
    'dudi'. See \code{\link[ade4:dudi]{dudi}},
      \code{\link[made4:bga]{bga}} or \code{\link[ade4:between]{between}}}
  \item{bet2}{An object of class 'bga', with sub-class
    'dudi'. See \code{\link[ade4:dudi]{dudi}},
      \code{\link[made4:bga]{bga}} or \code{\link[ade4:between]{between}}. }
}
\references{ 

Culhane AC, et al., 2003 Cross platform comparison and
  visualisation of gene expression data using co-inertia analysis. BMC Bioinformatics. 4:59   
}
\author{Aedin Culhane }
\note{ 
This is very computational intensive. The authors of ade4 are currently re-writing the code for coinertia
analysis, so that it should substantially improve the computational
requirements (May 2004).
}

\seealso{ See Also as \code{\link[ade4:coinertia]{coinertia}}, \code{\link[made4:cia]{cia}}. }
\examples{
### NEED TO DO 
if (require(ade4, quiet = TRUE)) {}
}
\keyword{multivariate}
\keyword{manip}

