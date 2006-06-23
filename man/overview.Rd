\name{overview}
\alias{overview}
\title{Draw boxplot, histogram and hierarchical tree of gene expression data }
\description{
Very simple wrapper function that draws a boxplot, histogram and hierarchical tree of expression 
data }

\usage{overview(dataset, labels=NULL, title="", classvec=NULL, ...)}

\arguments{
   \item{dataset}{ a \code{\link{matrix}}, \code{\link{data.frame}}, 
    \code{\link[Biobase:exprSet-class]{exprSet}} or \code{\link[marrayClasses:marrayRaw-class]{marrayRaw}}.  
     If the input is gene expression data in a \code{\link{matrix}} or \code{\link{data.frame}}. The 
     rows and columns are expected to contain the variables (genes) and cases (array samples) 
     respectively.}
   \item{labels}{Vector, labels to be placed on samples in plots. Default is rownames(dataset)}
   \item{title}{Character, label to be placed on plots. Default is NULL}
  \item{classvec}{ A \code{factor} or \code{vector} which describes the classes in columns of the
  \code{dataset}.  Default is NULL. If included columns will be coloured by class.}
   \item{\dots}{further arguments passed to or from other methods }
  }
\details{The hierarchical plot is produced using average linkage cluster analysis with Eisen's 
correlation metric.}
\value{}
\references{  }
\author{ Aedin Culhane }
\note{}
\seealso{ See also as \code{\link[boxplot]{boxplot}}, \code{\link[hclust]{hclust}}, 
\code{\link[hist]{hist}} }
\examples{
  data(khan)

  logkhan<-log2(khan$train)
  overview(logkhan, title="Subset of Khan Training data")
  overview(logkhan, classvec=khan$train.classes, labels=khan$train.classes)
}
\keyword{hplot }% at least one, from doc/KEYWORDS
\keyword{manip }% __ONLY ONE__ keyword per line
