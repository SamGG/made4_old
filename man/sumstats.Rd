
\name{sumstats}
\alias{sumstats}
\title{Summary statistics on xy co-ordinates, returns the slopes and distance from origin of each co-ordinate.}
\description{
Given a \code{\link[data.frame]{data.frame}} or
\code{\link[matrix]{matrix}} containing xy coordinates, it returns the slope and distance from
origin of each coordinate.
}
\usage{
sumstats(array, xax = 1, yax = 2)
}

\arguments{
  \item{array}{A \code{\link[data.frame]{data.frame}} or
   \code{\link[matrix]{matrix}} containing xy coordinates, normally a \$co, \$li
    from \code{\link[ade4:dudi]{dudi}} such as PCA or COA, or \$ls from 
    \code{\link[made4:bga]{bga}}}
  \item{xax}{Numeric, an integer indicating the column of the x axis
    coordinates. Default xax=1 }
  \item{yax}{ Numeric, an integer indicating the column of the x axis
    coordinates. Default xax=2 }
}
\details{
In PCA or COA, the variables (upregulated genes) that are most associated with 
a case (microarray sample), are those that are projected in the same direction 
from the origin. 

Variables or cases that have a greater contribution to the variance in the data are
projected further from the origin in PCA. Equally variables and cases with the strong 
association have a high chi-square value, and are projected with greater distance 
from the origin in COA, See a description from Culhane et al., 2002 for more details.

Although the projection of co-ordinates are best visualised on an xy plot, \code{sumstats}
returns the slope and distance from origin of each x,y coordinate in a matrix.
       

}
\value{
A matrix (ncol=3) containing 

   \preformatted{
   slope 
   angle (in degrees) 
   distance from origin }

of each x,y coordinates in a matrix.

}
\references{ }
\author{ Aedin Culhane }
\note{}



\seealso{  }
\examples{
data(khan)

if (require(ade4, quiet = TRUE)) {khan.bga<-bga(khan$train[1:1000,], khan$train.classes)}

s.var(khan.bga$bet$ls, col=as.numeric(khan$train.classes), clabel=0.8)
st.out<-sumstats(khan.bga$bet$ls)

# Get stats on classes  EWS and BL
EWS<-khan$train.classes==levels(khan$train.classes)[1]
st.out[EWS,]

BL<-khan$train.classes==levels(khan$train.classes)[2]
st.out[BL,]

# Add dashed line to plot to highlight min and max slopes of class BL
slope.BL.min<-min(st.out[BL,1])
slope.BL.max<-max(st.out[BL,1])
abline(c(0,slope.BL.min), col="red", lty=5)
abline(c(0,slope.BL.max), col="red", lty=5)

}
\keyword{manip}

