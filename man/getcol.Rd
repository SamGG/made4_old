\name{getcol}
\alias{getcol}
\title{Specialised colour palette with set of 21 maximally contrasting colours}
\description{
 Special colour palette developed to maximise the contrast between
 colours. Colours were selected for visualising groups of points on xy or xyz plots 
 on a white background. Because of this, there are few pastel colours are in this palette. 
 \code{getcol} contains 2 palettes of 12 and 21 colours. 
}
\usage{
getcol(nc = c(1:3), palette = NULL, test = FALSE)
}

\arguments{
  \item{nc}{Numeric. Integer or vector in range 1 to 21. This selects colours from palette}
  \item{palette}{A character to select either palette "colours1" or
    "colours2". \code{colours1} contains 12 colours, \code{colours2}
    contains 21 colours}
  \item{test}{A logical, if TRUE a plot will be drawn to display the
    palettes colours1, colours2 and any selected colours. }
}
\details{
  Colours1 contains the 12  colours,"red","blue"
  ,"green","cyan","magenta","yellow", "grey","black","brown", "orange", "violet", "purple"). These were choosen, as these are compatible with rasmol and chime, that are used in html3D. Colours2
  contains 21 colours. These were selected so as to maximise the
  contrast between groups.

  For other colour palettes in R, see \code{colors}, \code{palette}, \code{rainbow},  \code{heat.colors},
  \code{terrain.colors}, \code{topo.colors} or \code{cm.colors}.

  Also see the library \code{RColorBrewer}

}
\value{
A vector containing a list of colours.

}

\author{Aedin Culhane }


\seealso{See also \code{colors}, \code{palette}, \code{rainbow},  \code{heat.colors},
  \code{terrain.colors}, \code{topo.colors} or \code{cm.colors}, \code{RColorBrewer} }
\examples{
getcol(3)
getcol(c(1:7))
getcol(10, test=TRUE)
getcol(c(1:5, 7, 15, 16), palette="colours2",test=TRUE)
}
\keyword{ color }% at least one, from doc/KEYWORDS
\keyword{ hplot }% __ONLY ONE__ keyword per line
