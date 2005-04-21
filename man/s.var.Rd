\name{s.var}
\alias{s.var}
\alias{s.groups}
\alias{s.match.col}

\title{Draw xy plots of results of multivariate analysis }
\description{
A number of functions for graphically displaying results from between group, 
coinertia, or other multivariate analysis of gene expression data. s.var, 
s.groups and s.match.col are derived from ADE4 graphics modules, s.label, 
s.class and s.match respectively.
}
\usage{

s.var(dfxy, xax = 1, yax = 2, label = row.names(dfxy), clabel = 1, pch = 20, 
      cpoint = if (clabel == 0) 1 else 0, neig = NULL, cneig = 2, xlim = NULL, 
      ylim = NULL, grid = TRUE, addaxes = TRUE, cgrid = 1, include.origin = TRUE, 
      origin = c(0, 0), sub = "", csub = 1.25, possub = "bottomleft", pixmap = NULL, 
      contour = NULL, area = NULL, add.plot = FALSE, colpoints = rep(1, nrow(dfxy)), 
      boxes = TRUE, ...)
s.groups(dfxy, classvec, wt = rep(1, length(classvec)), xax = 1, yax = 2, cstar = 1,
      cellipse = 1.5, axesell = TRUE, label = levels(classvec), slabel = row.names(dfxy),
      clabel = 1, cpoint = 1, pch = 20, col = rep(1, length(levels(classvec))),
      xlim = NULL, ylim = NULL, grid = TRUE, addaxes = TRUE, origin = c(0, 0), 
      include.origin = TRUE, sub = "", csub = 1, possub = "bottomleft", 
      cgrid = 1, pixmap = NULL, contour = NULL, area = NULL, add.plot = FALSE, ...)
s.match.col(df1xy, df2xy, xax = 1, yax = 2, pch = 20, cpoint = 1, 
      label = row.names(df1xy), clabel = 1, edge = TRUE, xlim = NULL, ylim = NULL, 
      grid = TRUE, addaxes = TRUE, cgrid = 1, include.origin = TRUE, origin = c(0, 0), 
      sub = "", csub = 1.25, possub = "bottomleft", pixmap = NULL, contour = NULL, 
      area = NULL, add.plot = FALSE, col = rep(1, nrow(df1xy)), classvec=NULL,...)
}

\arguments{
  \item{dfxy}{coordinates, normally the \$ls, \$co or \$li coordinates 
   from PCA, COA or BGA with at least two columns, containing the x,y 
   coordinates to be plotted }
  \item{df1xy, df2xy}{coordinates from \code{cia}, normally the \$mX, \$mY 
    coordinates with at least two columns, containing the x,y coordinates to 
    be plotted }
  \item{colpoints}{Character, colour of arrays (cases),genes or points}
  \item{xax}{ the column number for the x-axis }
  \item{yax}{ the column number for the y-axis}
  \item{classvec}{\code{Factor}. A factor, classvec partitioning the rows of the data
    frame in classes}
   \item{boxes}{A logical, indicating whether a box should be drawn around
  each variable label. The default is TRUE}
  \item{add.plot}{ logical if TRUE plot draw in current graphics window}
  \item{origin, sub, wt, cstar, cellipse, axesell, 
     col, edge}{ Graphing  parameters. See \code{s.label} or \code{s.class} 
     for details}
  \item{clabel,pch, cpoint, neig, cneig, xlim, ylim, grid, 
    addaxes, cgrid, include.origin, csub, possub, pixmap, contour, 
    area}{Graphing parameters, see \code{s.match} for details}   
  \item{\dots}{ further arguments passed to or from other method }
}

\details{
These functions are modified versions of s.label, s.class and s.match graph 
modules from ADE4. Please see  \code{\link[ade4:s.label]{s.label}} or 
\code{\link[ade4:s.class]{s.class}} and  \code{\link[ade4:s.match]{s.match}} 
for father details.

These functions \code{s.var}, \code{s.groups} and \code{s.match.col} are called by 
\code{\link[made4:plot.bga]{plot.bga}} and  \code{\link[made4:plot.cia]{plot.cia}}. 

}


\author{ Aedin Culhane }


\seealso{See also  \code{\link[ade4:s.class]{s.class}}, 
\code{\link[ade4:s.label]{s.label}},
\code{\link[ade4:s.match]{s.match}}}

\examples{
data(khan)
if (require(ade4, quiet = TRUE)) {
khan.bga<-bga(khan$train, khan$train.classes)
attach(khan.bga)
par(mfrow=c(2,2))
s.var(bet$ls, col=as.numeric(khan$train.classes), clabel=0.8)
s.groups(bet$ls, khan$train.classes, add.plot=TRUE, 
col=c(1:length(levels(khan$train.classes))) )

}}
\keyword{multivariate }
\keyword{internal}


