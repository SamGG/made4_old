\name{suppl}
\alias{suppl}
\alias{plot.suppl}
\title{Projection of supplementary data onto axes from a between group analysis}
\description{
Projection and class prediction of supplementary points onto 
axes from a between group analysis, \code{\link[made4:bga]{bga}}.
}
\usage{
suppl(dudi.bga, supdata, supvec = NULL, assign=TRUE, \dots)
\method{plot}{suppl}(x, dudi.bga, axis1=1, axis2=2, supvec=sup$true.class, supvec.pred= sup$predicted, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dudi.bga}{ An object returned by \code{\link[made4:bga]{bga}}.}
  \item{supdata}{Test or blind dataset. Accepted formats are a \code{\link{matrix}}, 
   \code{\link{data.frame}}, \code{\link[Biobase:ExpressionSet-class]{ExpressionSet}} or 
   \code{\link[marray:marrayRaw-class]{marrayRaw-class}}.}
  \item{supvec}{A factor or vector which describes the classes in the
  training dataset.}
   \item{supvec.pred}{A factor or vector which describes the classes which were predicted
   by \code{\link[made4:suppl]{suppl}}.}
  \item{assign}{A logical indicating whether class assignment should be calculated
   using the method described by Culhane et al., 2002. The default value is \code{TRUE}.}
  \item{x}{An object returned by \code{\link[made4:suppl]{suppl}}.}
  \item{axis1}{Integer, the column number for the x-axis. The default is 1.}
  \item{axis2}{Integer, the column number for the y-axis. The default is 2.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
After performing a between group analysis on a training dataset using \code{\link[made4:bga]{bga}}, 
a test dataset can be then projected onto \code{bga} axes using \code{\link[made4:suppl]{suppl}}.  

\code{suppl} returns the projected coordinates and assignment of each test case (array).
 
The test dataset must contain the same number of variables (genes) as the training dataset. 
The input format of both the training dataset and test dataset are verified using
\code{array2ade4}. Use \code{\link[made4:plot.bga]{plot.bga}} to plot results from bga.  
}
\value{
  A list containing:
  \item{suppl}{An object returned by \code{\link[made4:suppl]{suppl}}} 
}
\references{Culhane AC, et al., 2002 Between-group analysis of microarray data. Bioinformatics. 18(12):1600-8.  }
\author{Aedin Culhane}
\seealso{See Also  \code{\link[made4:bga]{bga}},
  \code{\link[ade4:between]{between}}, \code{\link[made4:plot.bga]{plot.bga}}, 
  \code{\link[made4:bga.jackknife]{bga.jackknife}} }
\examples{
data(khan)
#khan.bga<-bga(khan$train, khan$train.classes)
if (require(ade4, quiet = TRUE)) {
khan.bga<-bga.suppl(khan$train, supdata=khan$test, classvec=khan$train.classes, 
          supvec=khan$test.classes)

khan.bga
plot.bga(khan.bga, genelabels=khan$annotation$Symbol)
khan.bga$suppl

plot.suppl(khan.bga$suppl, khan.bga) 
plot.suppl(khan.bga$suppl, khan.bga, supvec=NULL, supvec.pred=NULL)
plot.suppl(khan.bga$suppl, khan.bga, axis1=2, axis2=3,supvec=NULL, supvec.pred=NULL)
}
}
\keyword{manip}% at least one, from doc/KEYWORDS
\keyword{multivariate}
