\name{do3d}
\alias{do3d}
\alias{rotate3d}
\title{Generate 3D graph(s) using scatterplot3d}
\description{
\code{do3d} is a wrapper for scatterplot3d. do3d will draw a single 3D xyz
plot and will plot each group of points in a different colour, given a
factor. 

\code{rotate3d} calls \code{do3d} to draw multiple 3D plots in which each plot is 
marginally rotated on the x-y axis.  
}
\usage{
do3d(dataset, x = 1, y = 2, z = 3, angle = 40, classvec = NULL, classcol
= NULL, col = NULL, cex.lab=0.3, pch=19, cex.symbols=1, ...)
rotate3d(dataset, x = 1, y = 2, z = 3, beg = 180, end = 360, step = 12, 
savefiles = FALSE, classvec = NULL, classcol = NULL, col = NULL, ...)
}

\arguments{
  \item{dataset}{XYZ coordinates to be plotted. A \code{\link{matrix}} or \code{\link{data.frame}} 
   with 3 or more columns. Usually results from multivariate analysis, such as the \$co or \$li coordinates
   from a  PCA \code{\link[ade4:dudi.pca]{dudi.pca}}, or COA 
   \code{\link[ade4:dudi.coa]{dudi.coa}} or the \$ls, \$co coordinates from
   \code{\link[made4:bga]{bga}}. }
  \item{x}{Numeric, the column number for the x-axis, the default is 1 (that is
    dataset[,1])}
  \item{y}{Numeric, the column number for the y-axis, the default is 2 (that is dataset[,2])}
  \item{z}{Numeric, the column number for the z-axis, the default is 3 (that is dataset[,3])}
  \item{angle}{Numeric, the angle between x and y axis. Note the result depends
    on scaling. See  \code{\link[scatterplot3d:scatterplot3d]{scatterplot3d}}  }
  \item{classvec}{A \code{factor} or \code{vector} which describes the classes in dataset }
  \item{classcol}{A \code{factor} or \code{vector} which list the colours for each of the classes
    in the dataset. By default NULL. When NULL, \code{getcol} is used to
    obtain an optimum set of colours of the classes in classvec.}
  \item{cex.lab}{Numeric. The magnification to be used for the axis annotation 
   relative to the current default text and symbol size. Default is 0.3}
  \item{pch}{Integer specifying a symbol or single character to be used when 
   plotting points. The default is pch=  19}
  \item{cex.symbols}{Numeric. The magnification to be used for the symbols 
   relative to the current default text size. Default is 1}
  \item{col}{A character indicating a colour. To be used if all points
    are to be one colour. If classvec, classcol and col are all
    NULL. all points will be drawn in red by default.}
  \item{beg}{Numeric. The starting angle between the x and y axis for
      rotate3d. Rotate3d will draw plots in which they are
      rotated from angle beg to angle end}
  \item{end}{Numeric. The final angle between the x and y axis for
      \code{rotate3d}. Rotate3d will draw plots in which they are
      rotated from angle \code{beg} to angle \code{end} }
  \item{step}{Numeric. Increment of the sequence between the starting
    angle beg and the final angle end. }
  \item{savefiles}{Logical, indicating whether the plot should be
    saved as a pdf file. The default is FALSE }
  \item{\dots}{further arguments passed to or from other methods }
}
\details{

}
\value{
Produces plots of the xyz coordinates. 
}
\references{ }
\author{ Aedin Culhane }
\note{ }

\seealso{See Also \code{\link[scatterplot3d:scatterplot3d]{scatterplot3d}}}
\examples{
data(khan)
if (require(ade4, quiet = TRUE)) {
khan.coa<-dudi.coa(khan$train, scannf=FALSE, nf=5)
}
par(mfrow=c(2,1))
do3d(khan.coa$co, classvec=khan$train.classes)
do3d(khan.coa$co, col="blue")
rotate3d(khan.coa$co,classvec=khan$train.classes)
khan.bga<-bga(khan$train, khan$train.classes)
plot.new()
par(bg="black")
do3d(khan.bga$bet$ls, classvec=khan$train.classes)
}

\keyword{hplot}% __ONLY ONE__ keyword per line
