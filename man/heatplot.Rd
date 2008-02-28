\name{heatplot}
\alias{heatplot}
\title{Draws heatmap with dendrograms.}
\description{
  \code{heatplot} calls \code{heatmap.2} using a red-green colour scheme by default.  It also draws dendrograms of the cases and variables
  using correlation similarity metric and average linkage clustering as described by Eisen. \code{heatplot} is useful for a
  quick overview or exploratory analysis of data }

\usage{heatplot(dataset, dend = c("both", "row", "column", "none"),  cols.default = TRUE, lowcol = "green", highcol = "red", scale="row",  classvec=NULL, classvec2=NULL,  ...)
}
\arguments{
\item{dataset}{
     a \code{\link{matrix}}, \code{\link{data.frame}}, 
     \code{\link[Biobase:ExpressionSet-class]{ExpressionSet}} or 
     \code{\link[marrayClasses:marrayRaw-class]{marrayRaw}}.  
     If the input is gene expression data in a \code{\link{matrix}} or \code{\link{data.frame}}. The 
     rows and columns are expected to contain the variables (genes) and cases (array samples) 
     respectively.}
\item{dend}{A character indicating whether dendrograms should be drawn for both rows and columms "both", just rows "row" or column "column" or no dendrogram "none". Default is both.}
\item{cols.default}{Logical. Default is \code{TRUE}. Use blue-brown color scheme.}
\item{lowcol, highcol}{Character indicating colours to be used for down and upregulated genes when drawing heatmap if the default colors are not used, that is cols.default = FALSE.}
\item{scale}{Default is row. Scale and center either  "none","row", or "column").}
\item{classvec}{ A \code{factor} or \code{vector} which describes the classes in columns or rows of the
  \code{dataset}.  Default is \code{NULL}. If included, a color bar including the class of each column (array sample) or row (gene) will be drawn.
  It will automatically add to either the columns or row, depending if the length(as.character(classvec)) ==nrow(dataset) or ncol(dataset).}
\item{classvec2}{ A \code{factor} or \code{vector} which describes the classes in columns or rows of the
  \code{dataset}.  Default is \code{NULL}. If included, a color bar including the class of each column (array sample) or row (gene) will be drawn.
  It will automatically add to either the columns or row, depending if the length(as.character(classvec)) ==nrow(dataset) or ncol(dataset).}
\item{\dots}{further arguments passed to or from other methods.}
}
\details{
The hierarchical plot is produced using average linkage cluster analysis with a
correlation metric distance. \code{heatplot} calls \code{\link[gplots:heatmap.2]{heatmap.2}} in the R package \code{gplots}.
}
\value{Plots a heatmap with dendrogram of hierarchical cluster analysis}
\references{ 
Eisen MB, Spellman PT, Brown PO and Botstein D. (1998). Cluster Analysis and Display of 
Genome-Wide Expression Patterns. Proc Natl Acad Sci USA 95, 14863-8.
}
\author{Aedin Culhane}
\note{
Because Eisen et al., 1998 use green-red colours for the heatmap \code{heatplot} 
uses these by default however a blue-red or yellow-blue are easily obtained by 
changing lowcol and highcol}
\seealso{ See also as \code{\link[stats:hclust]{hclust}}, 
\code{\link[stats:heatmap]{heatmap}} and \code{\link[stats:dendrogram]{dendrogram}}}
\examples{
data(khan)

heatplot(khan$train[1:30,], cols.default=FALSE, lowcol="white", highcol="red")
heatplot(khan$train[1:26,], labCol = c(64:1), labRow=LETTERS[1:26])

heatplot(khan$train[1:26,], classvec=khan$train.classes)

if (require(ade4, quiet = TRUE)) {
res<-ord(khan$train, ord.nf=5)  # save 5 components from correspondence analysis
khan.coa = res$ord
}

# Provides a view of the components of the Correspondence analysis (gene projection) in the lines (row)
heatplot(khan.coa$li)    # first 5 components
heatplot(khan.coa$li, margins=c(4,20))  # Change the margin size. The default is c(5,5)

# Sample projection (columns)
# See that the difference between tissues and cell line samples are defined in the first axis.

heatplot(khan.coa$co,classvec2=khan$train.classes, cols.default=FALSE, lowcol="blue", dend="row", scale="none")
}
\keyword{hplot}
\keyword{manip}
