\name{jmol3D}
\alias{jmol3D}

\title{Produce web graph of 3D graph that can be viewed using Jmol in web browser}
\description{
\code{jmol3D} produces a html web page with a 3D graph of a pdb file that can be rotated and manipulated in a web browser that 
supports jmol. 
}
\usage{
jmol3D(df, classvec=NULL, title=NULL, jmoldir="../jmol/Jmol.js",
         filename="output.html", point.size=40, xyz.axes =c(1:3),    scaled=TRUE, ...)
}

\arguments{
  \item{df} {A \code{\link{matrix}} or \code{\link{data.frame}} containing the x,y,z coordinates. Typically the output from \code{\link[made4:bga]{bga}} such as the \$ls or \$co files, or other xyz coordinates (\$li or \$co) produced by PCA, COA or other \code{\link[ade4:dudi]{dudi}}
    }
  \item{classvec}{Factor or vector which describes classes in the df. Default is NULL. If specified each group 
   will be coloured in contrasting colours}
  \item{title}{Character, the title (header) of the web page saved if writehtml is TRUE. The default is NULL.}
  \item{filename}{Character. The filename of the html to be saved. The default is 
   "output.html".}
  \item{xyz.axes} {vector indicating which axes to use for x, y and z axes. By default, the  first 3 columns of df.}
  \item{jmoldir}{Character indicating the local directory containing jmol}
  \item{point.size}{Numeric, size of the points in plot. Default is 40}
  \item{\dots}{ further arguments passed to or from other methods }
}
\details{
 Produces a html file, of a 3D graph which can be rotated using the opensource software jmol
\url{http://jmol.sourceforge.net/} which is a java based software that is open source.   
For example see \url{http://bioinf.ucd.ie/people/aedin/jmoltest/test.html}.
}
\value{
jmol3D output is a html file that can be viewed in any web browser that supports jmol \url{http://jmol.sourceforge.net/}.
}

\author{Aedin Culhane  }
\note{ 
Note jmol must be downloaded on a local machine.  Instructions on installing this are available at  \url{http://jmol.sourceforge.net/}.

}

}


\seealso{ }
\examples{

}
\keyword{internal}% at least one, from doc/KEYWORDS

