\name{chime3D}
\alias{chime3D}

\title{Produce web graph of 3D graph that can be viewed using Chime web browser plug-in from a pdb file}
\description{
\code{chime3D} produces a html web page with a 3D graph of a pdb file that can be rotated and manipulated in a web browser that 
supports the chime plug-in. 
}
\usage{
chime3D(pdbfilename, classvec = NULL, title = NULL, filename = "output.html", point.size=40, ...)
}

\arguments{
  \item{pdbfilename}{Input pdb filename, this is an output file from \code{html3D}.}
  \item{classvec}{Factor or vector which describes classes in the df. Default is NULL. If specified each group 
   will be coloured in contrasting colours}
  \item{title}{Character, the title (header) of the web page saved if writehtml is TRUE. The default is NULL.}
  \item{filename}{Character. The filename of the html to be saved. The default is 
   "output.html".}
  \item{point.size}{Numeric, size of the points in plot. Default is 40}
  \item{\dots}{ further arguments passed to or from other methods }
}
\details{
 Produces a html file, of a 3D graph which can be rotated using the FREEWARE chime (win, MacOS). 
 Will colour samples by classvec if given one, and will produce chime script to highlight groups, spin on/off, 
and include button for restore for example see \url{http://bioinf.ucd.ie/research/microarrays/}.
}
\value{
chime3D output is a html file that can be viewed in any web browser that supports the Chime plug-in.
}

\author{Aedin Culhane  }
\note{ 
Note chime is only available on windows or Mac OS currently. Using the chime plug-in on Linux 
is slightly complicated but is available if the CrossOver Plug-in is installed. Instructions on installing this and chime on Linux 
are available at  \url{http://mirrors.rcsb.org/SMS/STINGm/help/chime_linux.html}.


If you wish to view a 3D graph in Rasmol, you will need to execute a Rasmol script similar to 
\preformatted{

load pdbfilename.pdb; 
set axes on; 
select off; 
connect;
set ambient 40; 
rotate x 180; 
select *;  
spacefill 40
}

}


\seealso{ }
\examples{

}
\keyword{internal}% at least one, from doc/KEYWORDS

