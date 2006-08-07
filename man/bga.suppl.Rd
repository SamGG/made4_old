\name{bga.suppl}
\alias{bga.suppl}
\title{Between group analysis with supplementary data projection}
\description{
\code{bga.suppl} performs a \code{bga} between group analysis with projection
of supplementary points using \code{suppl}
} 

\usage{
bga.suppl(dataset, supdata, classvec, supvec = NULL, suponly = FALSE, type="coa", ...)
}

\arguments{
  \item{dataset}{Training dataset. A \code{\link{matrix}}, \code{\link{data.frame}}, 
     \code{\link[Biobase:exprSet-class]{exprSet}} or \code{\link[marrayClasses:marrayRaw-class]{marrayRaw}}.  
     If the input is gene expression data in a \code{\link{matrix}} or \code{\link{data.frame}}. The 
     rows and columns are expected to contain the variables (genes) and cases (array samples) 
     respectively.
    }
  \item{supdata}{Test or blind dataset. A \code{\link{matrix}}, \code{\link{data.frame}}, 
     \code{\link[Biobase:exprSet-class]{exprSet}} or \code{\link[marrayClasses:marrayRaw-class]{marrayRaw}}.  
     If the input is gene expression data in a \code{\link{matrix}} or \code{\link{data.frame}}. The 
     rows and columns are expected to contain the variables (genes) and cases (array samples) 
     respectively.  The test dataset \code{supdata} and the training dataset \code{dataset} must contain 
     the same number of variables (genes).
     }
  \item{classvec}{A \code{factor} or \code{vector} which describes the classes in the
  training data \code{dataset}}
  \item{supvec}{A \code{factor} or \code{vector} which describes the classes in the
  test dataset \code{supdata}}
  \item{suponly}{Logical indicating whether the returned output should contain 
    the test class assignment results only. The default value is FALSE, that 
    is the training coordinates, test coordinates and class assignments 
    will all be returned.}
 \item{type}{Character, "coa", "pca" or "nsc" indicating which data
    transformation is required. The default value is type="coa"}

  \item{\dots}{further arguments passed to or from other methods }}

\details{\code{bga.suppl} calls \code{bga} to perform between group analysis (bga) on the training dataset, 
then it calls \code{suppl} to project the test dataset onto the bga axes. 
It returns the coordinates and class assignment of the cases (microarray samples) in the test dataset as 
described by Culhane et al., 2002. 

The test dataset must contain the same number of variables (genes) as the training dataset. 

The input format of both the training dataset and test dataset are verified using \code{array2ade4}. 
Use \code{\link[made4:plot.bga]{plot.bga}} to plot results from bga.}

\value{
  If \code{suponly} is FALSE (the default option)  \code{bga.suppl} returns a list of length 4 containing 
  the results of the \code{bga} of the training dataset and the results of the projection of the test dataset onto the bga axes-
  \item{ord}{Results of initial ordination. A list of class "dudi" (see \code{\link[ade4:dudi]{dudi}}). }
  \item{bet}{Results of between group analysis. A list of class "dudi"
    (see \code{\link[ade4:dudi]{dudi}}),"between" (see \code{\link[ade4:between]{between}}),and 
    "dudi.bga"(see \code{\link[made4:bga]{bga}})}
  \item{fac}{The input classvec, the factor or vector which described the classes in the input dataset}
  \item{suppl}{An object returned by \code{\link[made4:suppl]{suppl}}}

 If \code{suponly} is TRUE only the results from \code{suppl} will be returned.
  
}
\references{Culhane AC, et al., 2002 Between-group analysis of microarray data. Bioinformatics. 18(12):1600-8.  }
\author{ Aedin Culhane }

\seealso{See Also  \code{\link[made4:bga]{bga}},
  \code{\link[made4:suppl]{suppl}}, \code{\link[ade4:between]{between}},
  \code{\link[made4:plot.bga]{plot.bga}}, \code{\link[made4:bga.jackknife]{bga.jackknife}} }
\examples{
data(khan)
#khan.bga<-bga(khan$train, khan$train.classes)
if (require(ade4, quiet = TRUE)) {
khan.bga<-bga.suppl(khan$train, supdata=khan$test, 
classvec=khan$train.classes, supvec=khan$test.classes)

khan.bga
plot.bga(khan.bga, genelabels=khan$annotation$Symbol)
khan.bga$suppl
}
}
\keyword{manip}% at least one, from doc/KEYWORDS
\keyword{multivariate}



