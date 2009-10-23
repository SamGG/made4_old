\name{pretty.dend}
\alias{pretty.dend}
\title{Draw hierarchical tree of gene expression data with a colorbar for numerous class vectors}
\description{
Function which performs a hierarchical cluster analysis of data, drawing a dendrogram, with colorbars for different sample covariate beneath the dendrogram}

\usage{pretty.dend(dataset, labels = NULL, title = "", classvec = NULL,covars=1,returnTree=FALSE, getPalette=getcol,\dots)
}
\arguments{
  \item{dataset}{ a \code{\link{matrix}}, \code{\link{data.frame}}, 
    \code{\link[Biobase:ExpressionSet-class]{ExpressionSet}} or
    \code{\link[marray:marrayRaw-class]{marrayRaw-class}}.  
     If the input is gene expression data in a \code{\link{matrix}} or \code{\link{data.frame}}. The 
     rows and columns are expected to contain the variables (genes) and cases (array samples) 
     respectively.}
  \item{labels}{Vector, labels to be placed on samples in plots. Default is rownames(dataset).}
  \item{title}{Character, label to be placed on plots. Default is \code{NULL}.}
  \item{classvec}{ A \code{factor} or \code{vector} or \code{matrix} or \code{data.frame} which describes the classes in columns of the \code{dataset}.  Default is \code{NULL}.}
  \item{covars}{ Numeric. The columns of the data.frame classve to be used as class vectors.  These will be displayed as color bars under the dendrogram. The default is 1 (column 1).}
  \item{returnTree}{ Logical. Return the hieracrhical cluster analysis results. Default is \code{FALSE}.}
  \item{getPalette}{Function, which generates a palette of colors.  The default uses \code{getcol} function in made4. Other examples are provided below}
  \item{\dots}{further arguments passed to or from other methods.}
  }
\details{The hierarchical plot is produced using average linkage cluster analysis with 1- Pearson's correlation metric.
The default set of colors used to generate the color bars of the plots can be changed (see example below).  By default, if there is only two levels in the factor, the colors will be black and grey. 
}
\value{}
\references{  }
\author{Aedin Culhane}
\note{}
\seealso{ See also as \code{\link[made4:overview]{overview}}, \code{\link[stats:hclust]{hclust}}}
\examples{
  data(khan)
  logkhan<-log2(khan$train)
  pretty.dend(logkhan, classvec=khan$train.classes, labels=khan$train.classes)
 
 # Get a character vector which defines which khan samples are cell lines or tissue sample
  khanAnnot= cbind(as.character(khan$train.classes),khan$cellType)
  print(khanAnnot[1:3,])

# Add 2 color bar, one for cancer subtype, another for cell type under dendrogram
  pretty.dend(logkhan, classvec=khanAnnot, covars = c(1,2), labels=khan$train.classes)

# To change the palette of colors
# Use topo.colors(), see colors() for more help on inbuilt palettes
  pretty.dend(logkhan, classvec=khanAnnot, covars = c(1,2), labels=khan$train.classes, getPalette=topo.colors)

# To use RColorBrewer Palettes
  library(RColorBrewer)

  # Use RColorBrewer Dark2 which contains 8 colors 
  pretty.dend(logkhan, classvec=khanAnnot, covars = c(1,2), labels=khan$train.classes, getPalette=function(x) brewer.pal(8,"Dark2")[1:x])

  # Use RColorBrewer Set1 which contains 9 colors 
  pretty.dend(logkhan, classvec=khanAnnot, covars = c(1,2), labels=khan$train.classes, getPalette=function(x) brewer.pal(9,"Set1")[1:x])

}
\keyword{hplot}% at least one, from doc/KEYWORDS
\keyword{manip}% __ONLY ONE__ keyword per line
