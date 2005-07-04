\name{html3D}
\alias{html3D}

\title{Produce web page with a 3D graph that can be viewed using Chime web browser plug-in, and/or a
pdb file that can be viewed using Rasmol}
\description{
\code{html3D} produces a pdb file that can be viewed using the freeware protein structure viewer Rasmol
and a html web page with a 3D graph that can be rotated and manipulated in a web browser that 
supports the chime web browser plug-in. 
}
\usage{
html3D(df, classvec = NULL, writepdb = FALSE, filenamebase = "output", 
       writehtml = FALSE, title = NULL, scaled=TRUE,xyz.axes=c(1:3), ...)
}


\arguments{
  \item{df}{A \code{\link{matrix}} or \code{\link{data.frame}} containing the x,y,z coordinates. 
    Typically the output from \code{\link[made4:bga]{bga}} such as the \$ls or \$co files, or other xyz
    coordinates (\$li or \$co) produced by PCA, COA or other \code{\link[ade4:dudi]{dudi}}
    }
  \item{classvec}{\code{factor} or \code{vector} which describes classes in the df. Default is NULL. 
   If specified each group will be coloured in contrasting colours}
  \item{writepdb}{Logical. The default is FALSE. If TRUE a file will be saved which can be read into Rasmol.}
  \item{writehtml}{Logical. The default is FALSE, If TRUE a web html file will be saved which can be 
    viewed in any web browser than supports chime.}
  \item{filenamebase}{Character. The basename of the html or pdb file(s) to be saved. The 
    default is "output", which will save files output.pdb, output.html, if writepdb or writehtml are TRUE respectively.}
  \item{title}{Character, the title (header) of the web page saved if writehtml is TRUE. 
  The default is NULL.}
  \item{scaled}{Logical indicating whether the data should be scaled for best fit. The default is TRUE}
  \item{xyz.axes} {vector indicating which axes to use for x, y and z axes. By default, the first 3 columns of df.}
  \item{\dots}{ further arguments passed to or from other methods }
}
\details{
 Produces a html file, of a 3D graph which can be rotated using the FREEWARE chime (win, MacOS). Chime 
 can be downloaded from  \url{http://www.mdlchime.com/}. 

 \code{html3D} will colour samples by classvec if given one, and will produce chime script to highlight groups, 
 spin on/off, and include button for restore for example see \url{http://bioinf.ucd.ie/research/BGA/supplement.html}

 \code{html3d} calls \code{\link[made4:chime3D]{chime3D}} to produce the html web page with a 3D graph.
}
\value{
\code{html3D} produces the pdb output file which can be read in Rasmol or other molecular structure viewers.
\code{html3D} produces a html file with a 3D graph that can be rotated and manipulated in a web browser that 
supports the chime web browser plug-in. 
}

\author{ Aedin Culhane  }
\note{ 
Note chime is only available on windows or Mac OS currently. Using the chime plug-in on Linux 
is slightly complicated but is available if the CrossOver Plug-in is installed.  
Instructions on installing this and chime on Linux are available at  
\url{http://mirrors.rcsb.org/SMS/STINGm/help/chime_linux.html}

If you wish to view a 3D graph in Rasmol, you will need to execute a Rasmol script similar to 

\preformatted{
load pdbfilename.pdb; 
set axes on; select off; 
connect;set ambient 40; 
rotate x 180; select *;  
spacefill 40
}

\code{html3D} calls \code{chime3D} to produce the html file from the pdb file.

The author would like to thank Willie Taylor, The National Institute for Medical Research, London, UK
for help with the awk command on which this function is based.

}

\seealso{ }
\examples{
data(khan)
if (require(ade4, quiet = TRUE)) {
khan.bga<-bga(khan$train, khan$train.classes)
}

out.3D <-html3D(khan.bga$bet$ls, khan.bga$fac, writepdb=TRUE, 
filenamebase ="Khan" , writehtml=TRUE)

\dontrun{
browseURL(paste("file://", file.path(paste(getwd(),"/khan.html", 
sep="")), sep=""))
}
}
\keyword{hplot}% at least one, from doc/KEYWORDS

