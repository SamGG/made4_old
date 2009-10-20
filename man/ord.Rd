\name{ord}
\alias{ord}
\alias{plot.ord}
\title{Ordination}
\description{Run principal component analysis, correspondence analysis or non-symmetric correspondence analysis 
on gene expression data} 
\usage{
ord(dataset, type="coa", classvec=NULL,ord.nf=NULL, trans=FALSE, \dots)
\method{plot}{ord}(x, axis1=1, axis2=2, arraycol=NULL, genecol="gray25", nlab=10, genelabels= NULL, arraylabels=NULL,classvec=NULL, \dots)
}
\arguments{
  \item{dataset}{Training dataset. A \code{\link{matrix}}, \code{\link{data.frame}}, 
     \code{\link[Biobase:ExpressionSet-class]{ExpressionSet}} or
     \code{\link[marray:marrayRaw-class]{marrayRaw-class}}.  
     If the input is gene expression data in a \code{\link{matrix}} or \code{\link{data.frame}}. The 
     rows and columns are expected to contain the variables (genes) and cases (array samples) 
     respectively.
    }
  \item{classvec}{A \code{factor} or \code{vector} which describes the classes in the training dataset.}
  \item{type}{Character, "coa", "pca" or "nsc" indicating which data
    transformation is required. The default value is type="coa".}
  \item{ord.nf}{Numeric. Indicating the number of eigenvector to be saved, by default, if NULL, all eigenvectors will be saved.}
  \item{trans}{Logical indicating whether 'dataset' should be transposed before ordination. Used by BGA
          Default is \code{FALSE}.}
  \item{x}{An object of class \code{ord}.  The output from \code{ord}. It contains the projection coordinates from \code{ord}, 
    the \$co or \$li coordinates to be plotted.}
   \item{arraycol, genecol}{Character, colour of points on plot. If arraycol is NULL, 
   arraycol will obtain a set of contrasting colours using \code{getcol}, for each classes 
   of cases (microarray samples) on the array (case) plot.  genecol is the colour of the 
    points for each variable (genes) on gene plot.}
  \item{nlab}{Numeric. An integer indicating the number of variables (genes) at the end of
    axes to be labelled, on the gene plot.}
  \item{axis1}{Integer, the column number for the x-axis. The default is 1.}
  \item{axis2}{Integer, the column number for the y-axis, The default is 2.}
  \item{genelabels}{A vector of variables labels, if \code{genelabels=NULL} the row.names 
   of input matrix \code{dataset} will be used.}
  \item{arraylabels}{A vector of variables labels, if \code{arraylabels=NULL} the col.names 
   of input matrix \code{dataset} will be used.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
\code{ord} calls either \code{\link[ade4:dudi.pca]{dudi.pca}}, \code{\link[ade4:dudi.coa]{dudi.coa}} or \code{\link[ade4:dudi.nsc]{dudi.nsc}}
on the input dataset.  The input format of the dataset 
is verified using \code{\link[made4:array2ade4]{array2ade4}}. 

If the user defines microarray sample groupings, these are colours on plots produced by \code{plot.ord}. 


\bold{Plotting and visualising bga results:}

 \emph{2D plots:} 
 \code{\link[made4:plotarrays]{plotarrays}} to draw an xy plot of cases (\$ls).   
\code{\link[made4:plotgenes]{plotgenes}}, is used to draw an xy plot of the variables (genes). 

 
 \emph{3D plots:}
 3D graphs can be generated using \code{\link[made4:do3d]{do3D}} and \code{\link[made4:html3D]{html3D}}. 
 \code{\link[made4:html3D]{html3D}} produces a web page in which a 3D plot can be interactively rotated, zoomed,
 and in which classes or groups of cases can be easily highlighted. 


 \emph{1D plots, show one axis only:}
 1D graphs can be plotted using  \code{\link[made4:graph1D]{graph1D}}.  \code{\link[made4:graph1D]{graph1D}} 
 can be used to plot either cases (microarrays) or variables (genes) and only requires
 a vector of coordinates (\$li, \$co)
 

\bold{Analysis of the distribution of variance among axes:}
 
The number of axes or  principal components from a \code{ord} will equal \code{nrow} the number of rows, or the 
\code{ncol}, number of columns of the dataset (whichever is less).

 The distribution of variance among axes is described in the eigenvalues (\$eig) of the \code{ord} analysis. 
These can be visualised using a scree plot, using \code{\link[ade4:scatter]{scatterutil.eigen}} as it done in \code{plot.ord}.  
 It is also useful to visualise the principal components from a using a \code{ord} or principal components analysis 
 \code{\link[ade4:dudi.pca]{dudi.pca}}, or correspondence analysis \code{\link[ade4:dudi.coa]{dudi.coa}} using a
 heatmap. In MADE4 the function \code{\link[made4:heatplot]{heatplot}} will plot a heatmap with nicer default colours.


\bold{Extracting list of top variables (genes):}

 Use \code{\link[made4:topgenes]{topgenes}}  to get list of variables or cases at the ends of axes.  It will return a list
 of the top n variables (by default n=5) at the positive, negative or both ends of an axes.  
 \code{\link[made4:sumstats]{sumstats}} can be used to return the angle (slope) and distance from the origin of a list of
 coordinates.
}
\value{
  A list with a class \code{ord} containing:

  \item{ord}{Results of initial ordination. A list of class "dudi" (see \code{\link[ade4:dudi]{dudi}})}
  \item{fac}{The input classvec, the \code{factor} or \code{vector} which described the classes in the input dataset. Can be NULL.}
}
\references{  }
\author{Aedin Culhane}

\seealso{See Also  \code{\link[ade4:dudi.pca]{dudi.pca}}, \code{\link[ade4:dudi.coa]{dudi.coa}} or \code{\link[ade4:dudi.nsc]{dudi.nsc}}, \code{\link[made4:bga]{bga}},
}
\examples{
data(khan)

if (require(ade4, quiet = TRUE)) {
  khan.coa<-ord(khan$train, classvec=khan$train.classes, type="coa")  
}

khan.coa
plot(khan.coa, genelabels=khan$annotation$Symbol)
plotarrays(khan.coa)
# Provide a view of the first 5 principal components (axes) of the correspondence analysis
heatplot(khan.coa$ord$co[,1:5], dend=FALSE,lowcol="blue")
}
\keyword{manip}
\keyword{multivariate}
