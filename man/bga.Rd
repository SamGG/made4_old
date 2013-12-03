\name{bga}
\alias{bga}
\alias{plot.bga}
\title{Between group analysis}
\description{Discrimination of samples using between group analysis as described by 
Culhane et al., 2002.} 

\usage{
bga(dataset, classvec, type = "coa", \dots)
\method{plot}{bga}(x, axis1=1, axis2=2, arraycol=NULL, genecol="gray25", nlab=10, 
         genelabels= NULL, \dots)
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
  \item{x}{An object of class \code{bga}.  The output from \code{bga} or 
    \code{\link[made4:bga.suppl]{bga.suppl}}. It contains the projection coordinates from \code{bga}, 
    the \$ls, \$co or \$li coordinates to be plotted.}
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
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
\code{bga} performs a between group analysis on the input dataset. This function
calls \code{\link[ade4:bca]{bca}}.  The input format of the dataset 
is verified using \code{\link[made4:array2ade4]{array2ade4}}. 

Between group analysis is a supervised method for sample discrimination and class prediction. 
BGA is carried out by ordinating groups (sets of grouped microarray samples), that is, 
groups of samples are projected into a reduced dimensional space. This is most easily 
done using PCA or COA, of the group means.  The choice of PCA, COA is defined by the parameter \code{type}.

The user must define microarray sample groupings in advance. These groupings are defined using 
the input \code{classvec}, which is a \code{factor} or \code{vector}. 


\bold{Cross-validation and testing of bga results:}

 bga results should be validated using one leave out jack-knife cross-validation using 
 \code{\link[made4:bga.jackknife]{bga.jackknife}} and by projecting a blind test datasets onto the bga axes 
 using \code{\link[made4:suppl]{suppl}}.  
 \code{bga} and \code{\link[made4:suppl]{suppl}} are combined in \code{\link[made4:bga.suppl]{bga.suppl}} 
 which requires input of both a training and test dataset.
 It is important to ensure that the selection of cases for a training and test set are not biased, and
 generally many cross-validations should be performed.  The function \code{\link[made4:randomiser]{randomiser}}
 can be used to randomise the selection of training and test samples.
 

\bold{Plotting and visualising bga results:}
 \emph{1D plots, show one axis only:}
 1D graphs can be plotted using \code{\link[made4:between.graph]{between.graph}} and
 \code{\link[made4:graph1D]{graph1D}}. \code{\link[made4:between.graph]{between.graph}} is used for plotting the cases,
 and required both the co-ordinates of the cases (\$ls) and their centroids (\$li). It accepts an object \code{bga}.  
 \code{\link[made4:graph1D]{graph1D}} can be used to plot either cases (microarrays) or variables (genes) and only requires
 a vector of coordinates.
 
 \emph{2D plots:}
 Use \code{plot.bga} to plot results from \code{bga}. plot.bga calls the functions 
 \code{\link[made4:plotarrays]{plotarrays}} to draw an xy plot of cases (\$ls).   
 \code{\link[made4:plotgenes]{plotgenes}}, is used to draw an xy plot of the variables (genes).
 \code{\link[made4:plotgenes]{plotgenes}}, is used to draw an xy plot of the variables (genes). 

 
 \emph{3D plots:}
 3D graphs can be generated using \code{\link[made4:do3d]{do3D}} and \code{\link[made4:html3D]{html3D}}. 
 \code{\link[made4:html3D]{html3D}} produces a web page in which a 3D plot can be interactively rotated, zoomed,
 and in which classes or groups of cases can be easily highlighted. 




\bold{Analysis of the distribution of variance among axes:}

 It is important to know which cases (microarray samples) are discriminated by the axes.  
The number of axes or  principal components from a \code{bga} will equal \code{the number of classes - 1}, 
that is length(levels(classvec))-1.

 The distribution of variance among axes is described in the eigenvalues (\$eig) of the \code{bga} analysis. 
These can be visualised using a scree plot, using \code{\link[ade4:scatter]{scatterutil.eigen}} as it done in \code{plot.bga}.  
 It is also useful to visualise the principal components from a using a \code{bga} or principal components analysis 
 \code{\link[ade4:dudi.pca]{dudi.pca}}, or correspondence analysis \code{\link[ade4:dudi.coa]{dudi.coa}} using a
 heatmap. In MADE4 the function \code{\link[made4:heatplot]{heatplot}} will plot a heatmap with nicer default colours.


\bold{Extracting list of top variables (genes):}

 Use \code{\link[made4:topgenes]{topgenes}}  to get list of variables or cases at the ends of axes.  It will return a list
 of the top n variables (by default n=5) at the positive, negative or both ends of an axes.  
 \code{\link[made4:sumstats]{sumstats}} can be used to return the angle (slope) and distance from the origin of a list of
 coordinates.


For more details see Culhane et al., 2002 and \url{http://bioinf.ucd.ie/research/BGA}. 
}
\value{
  A list with a class \code{bga} containing:

  \item{ord}{Results of initial ordination. A list of class "dudi" (see \code{\link[ade4:dudi]{dudi}} )}
  \item{bet}{Results of between group analysis. A list of class "dudi"  (see \code{\link[ade4:dudi]{dudi}}),
  "between" (see \code{\link[ade4:bca]{bca}})}
  \item{fac}{The input classvec, the \code{factor} or \code{vector} which described the classes in the input dataset}
}
\references{Culhane AC, et al., 2002 Between-group analysis of microarray data. Bioinformatics. 18(12):1600-8.}
\author{Aedin Culhane}
\seealso{See Also  \code{\link[made4:bga]{bga}},
  \code{\link[made4:suppl]{suppl}}, \code{\link[made4:bga.suppl]{suppl.bga}}, \code{\link[ade4:bca]{bca}},
  \code{\link[made4:bga.jackknife]{bga.jackknife}}}
\examples{
data(khan)

if (require(ade4, quiet = TRUE)) {
  khan.bga<-bga(khan$train, classvec=khan$train.classes)  
  }

khan.bga
plot(khan.bga, genelabels=khan$annotation$Symbol)

# Provide a view of the principal components (axes) of the bga  
heatplot(khan.bga$bet$ls, dend="none")   
}
\keyword{manip}
\keyword{multivariate}
