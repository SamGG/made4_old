\name{heatplot}
\alias{heatplot}
\title{Draws heatmap with dendrograms.}
\description{
  \code{heatplot} calls \code{heatmap.2} using a red-green colour scheme by default.  It also draws dendrograms of the cases and variables
  using correlation similarity metric and average linkage clustering as described by Eisen. \code{heatplot} is useful for a
  quick overview or exploratory analysis of data }

\usage{heatplot(dataset, dend = c("both", "row", "column", "none"),  cols.default = TRUE, lowcol = "green", highcol = "red", scale="none",  classvec=NULL, classvecCol=NULL, classvec2=NULL, distfun=NULL, returnSampleTree=FALSE,method="ave", dualScale=TRUE, zlim=c(-3,3),scaleKey=TRUE, ...)
}
\arguments{
\item{dataset}{
     a \code{\link{matrix}}, \code{\link{data.frame}}, 
     \code{\link[Biobase:ExpressionSet-class]{ExpressionSet}} or 
     \code{\link[marray:marrayRaw-class]{marrayRaw-class}}.  
     If the input is gene expression data in a \code{\link{matrix}} or \code{\link{data.frame}}. The 
     rows and columns are expected to contain the variables (genes) and cases (array samples) 
     respectively.}
\item{dend}{A character indicating whether dendrograms should be drawn for both rows and columms "both", just rows "row" or column "column" or no dendrogram "none". Default is both.}
\item{cols.default}{Logical. Default is \code{TRUE}. Use blue-brown color scheme.}
\item{lowcol, highcol}{Character indicating colours to be used for down and upregulated genes when drawing heatmap if the default colors are not used, that is cols.default = FALSE.}
\item{scale}{Default is row. Scale and center either  "none","row", or "column").}
\item{classvec,classvec2}{ A \code{factor} or \code{vector} which describes the classes in columns or rows of the
  \code{dataset}.  Default is \code{NULL}. If included, a color bar including the class of each column (array sample) or row (gene) will be drawn. It will automatically add to either the columns or row, depending if the length(as.character(classvec)) ==nrow(dataset) or ncol(dataset).}
\item{classvecCol}{A vector of length the number of levels in the factor classvec. These are the colors to be used for the row or column colorbar. Colors should be in the same order, as the levels(factor(classvec)) }
\item{distfun}{A character, indicating function used to compute the distance between both rows and columns.  Defaults to 1- Pearson Correlation coefficient}
\item{method}{The agglomeration method to be used. This should be one of '"ward"', '"single"','"complete"', '"average"', '"mcquitty"', '"median"' or '"centroid"'. See \code{hclust} for more details. Default is "ave"}
 \item{dualScale}{A \code{logical} indicating whether to \code{scale} the data by row and columns. Default is TRUE}
 \item{zlim}{A \code{vector} of length 2, with lower and upper limits using for scaling data. Default is c(-3,3)}
 \item{scaleKey}{A \code{logical} indicating whether to draw a heatmap color-key bar. Default is TRUE}
 \item{returnSampleTree}{A \code{logical} indicating whether to return the sample (column) tree.  If TRUE it will return an object of class \code{dendrogram}.  Default is FALSE}.
 \item{\dots}{further arguments passed to or from other methods.}
}
\details{
The hierarchical plot is produced using average linkage cluster analysis with a
correlation metric distance. \code{heatplot} calls \code{\link[gplots:heatmap.2]{heatmap.2}} in the R package \code{gplots}.

NOTE: We have changed heatplot scaling in made4 (v 1.19.1) in Bioconductor v2.5.  Heatplot by default dual scales the data to limits of -3,3.  To reproduce older version of heatplot, use  the parameters dualScale=FALSE, scale="row".
}
\value{Plots a heatmap with dendrogram of hierarchical cluster analysis.  If returnSampleTree is TRUE, it returns an object \code{dendrogram} which can be manipulated using   }
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

## Change color scheme
heatplot(khan$train[1:30,])
heatplot(khan$train[1:30,], cols.default=FALSE, lowcol="white", highcol="red")

## Add labels to rows, columns
heatplot(khan$train[1:26,], labCol = c(64:1), labRow=LETTERS[1:26])

## Add a color bar
heatplot(khan$train[1:26,], classvec=khan$train.classes)
heatplot(khan$train[1:26,], classvec=khan$train.classes, classvecCol=c("magenta", "yellow", "cyan", "orange"))

## Change the scaling to the older made4 version (pre Bioconductor 2.5)
heatplot(khan$train[1:26,], classvec=khan$train.classes, dualScale=FALSE, scale="row")


## Getting the members of a cluster and manuipulating the tree
sTree<-heatplot(khan$train, classvec=khan$train.classes, returnSampleTree=TRUE)
class(sTree)
plot(sTree)

## Cut the tree at the height=1.0
lapply(cut(sTree,h=1)$lower, labels)

## Zoom in on the first cluster
plot(cut(sTree,1)$lower[[1]])
str(cut(sTree,1.0)$lower[[1]])



## Visualizing results from an ordination using heatplot
if (require(ade4, quiet = TRUE)) {
res<-ord(khan$train, ord.nf=5)  # save 5 components from correspondence analysis
khan.coa = res$ord
}

# Provides a view of the components of the Correspondence analysis (gene projection) 
heatplot(khan.coa$li,  dend="row", dualScale=FALSE)    # first 5 components, do not cluster columns, only rows.

# Provides a view of the components of the Correspondence analysis (sample projection) 
# The difference between tissues and cell line samples are defined in the first axis.
heatplot(khan.coa$co, margins=c(4,20), dend="row")  # Change the margin size. The default is c(5,5)

# Add a colorbar, change the heatmap color scheme and no scaling of data
heatplot(khan.coa$co,classvec2=khan$train.classes, cols.default=FALSE, lowcol="blue", dend="row", dualScale=FALSE)
apply(khan.coa$co,2, range)


}
\keyword{hplot}
\keyword{manip}
