\name{heatplot}
\alias{heatplot}
\title{Draws heatmap with dendrograms.}
\description{
  \code{heatplot} calls \code{heatmap} using a red-green colour scheme by default.  It also draws dendrograms of the cases and variables
  using correlation similarity metric and average linkage clustering as described by Eisen. \code{heatplot} is useful for a
  quick overview or exploratory analysis of data }

\usage{heatplot(dataset, dend = TRUE, lowcol = "green", 
                highcol = "red", Colv=NULL, Rowv=NULL, ...)}
\arguments{
\item{dataset}{
     a \code{\link{matrix}}, \code{\link{data.frame}}, 
     \code{\link[Biobase:exprSet-class]{exprSet}} or \code{\link[marrayClasses:marrayRaw-class]{marrayRaw}}.  
     If the input is gene expression data in a \code{\link{matrix}} or \code{\link{data.frame}}. The 
     rows and columns are expected to contain the variables (genes) and cases (array samples) 
     respectively. }

\item{dend}{Logical, indicating whether dendrograms should be drawn. Default is TRUE. If FALSE both Colv and Rowv are set to NA.}
\item{Colv, Rowv}{Vector or object of class \code{dendrogram} used to reorder the columns, or rows.  If no ordering is 
  required, set Colv or Rowv = NA.  The default is NULL}
\item{lowcol, highcol}{Character indicating colours to be used for down and upregulated genes when drawing heatmap. Default
  is lowcol="green", and highcol="red".}
\item{\dots}{further arguments passed to or from other methods }
}
\details{
The hierarchical plot is produced using average linkage cluster analysis with a
correlation metric distance.  \code{heatplot} calls \code{\link[stats:heatmap]{heatmap}} and 
\code{\link[stats:dendrogram]{dendrogram}}.
}
\value{

}
\references{ 
Eisen MB, Spellman PT, Brown PO and Botstein D. (1998). Cluster Analysis and Display of 
Genome-Wide Expression Patterns. Proc Natl Acad Sci USA 95, 14863-8.
}
\author{ Aedin Culhane }
\note{
Because Eisen et al., 1998 use green-red colours for the heatmap \code{heatplot} 
uses these by default however a blue-red or yellow-blue are easily obtained by 
changing lowcol and highcol }
\seealso{ See also as \code{\link[stats:hclust]{hclust}}, 
\code{\link[stats:heatmap]{heatmap}} and \code{\link[stats:dendrogram]{dendrogram}}}

\examples{
data(khan)

heatplot(khan$train[1:30,], lowcol="blue", highcol="red")
heatplot(khan$train[1:26,], lowcol="blue", highcol="red", 
         labRow = c(64:1), labCol=LETTERS[1:26])

if (require(ade4, quiet = TRUE)) {
khan.coa<-dudi.coa(khan$train, scan=FALSE, nf=6)
}

# Provides a view of the components of the Correspondence analysis 
heatplot(khan.coa$li, dend=FALSE)   

# transposed so that it is easier to view. Can see that the difference between tissues 
# and cell line samples are defined in the first axis.

heatplot(t(khan.coa$li), dend=FALSE, lowcol="blue") 

}
\keyword{hplot }
\keyword{manip }


