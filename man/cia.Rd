
\name{cia}
\alias{cia}
\alias{plot.cia}
\title{Coinertia analysis: Explore the covariance between two datasets}
\description{Performs CIA on two datasets as described by Culhane et al., 2003. Used for meta-analysis
of two or more datasets.}
\usage{
cia(df1, df2, cia.nf=2, cia.scan=FALSE, nsc=TRUE,...)
plot.cia(x, nlab = 10, axes1 = 1, axes2 = 2, genecol = "gray25", 
         genelabels1 = rownames(ciares$co), genelabels2 = rownames(ciares$li), ...)

}

\arguments{
  \item{df1}{The first dataset.  A \code{\link{matrix}}, \code{\link{data.frame}}, 
     \code{\link[Biobase:exprSet-class]{exprSet}} or \code{\link[marrayClasses:marrayRaw-class]{marrayRaw}}.  
     If the input is gene expression data in a \code{\link{matrix}} or \code{\link{data.frame}}. The 
     rows and columns are expected to contain the variables (genes) and cases (array samples) 
     respectively.}
  \item{df2}{The second dataset.  A \code{\link{matrix}}, \code{\link{data.frame}}, 
     \code{\link[Biobase:exprSet-class]{exprSet}} or \code{\link[marrayClasses:marrayRaw-class]{marrayRaw}}.  
     If the input is gene expression data in a \code{\link{matrix}} or \code{\link{data.frame}}. The 
     rows and columns are expected to contain the variables (genes) and cases (array samples) 
     respectively.}
  \item{cia.nf}{Integer indicating the number of coinertia analysis
    axes to be saved. Default value is 2.}
  \item{cia.scan}{Logical indicating whether the coinertia analysis
    eigenvalue (scree) plot should be shown so that the number of axes, 
    \code{cia.nf} can be selected interactively. Default value is FALSE.}
  \item{nsc}{A logical indicating whether coinertia analysis should be
    performed using two non-symmetric correspondence analyses \code{\link[ade4:dudi.nsc]{dudi.nsc}}. 
    The default=TRUE is highly recommended. If FALSE, COA \code{\link[ade4:dudi.coa]{dudi.coa}} 
    will be performed on df1, and row weighted COA \code{\link[made4:dudi.rwcoa]{dudi.rwcoa}} 
    will be performed on df2 using the row weights from df1. }
  \item{x}{An object of class \code{cia}, containing the CIA projected coordinates to be plotted. 
    }
  \item{nlab}{Numeric. An integer indicating the number of variables (genes) to be labelled on plots}
  \item{axes1}{Integer, the column number for the x-axis. The default is 1}
  \item{axes2}{Integer, the column number for the y-axis. The default is 2.}
  \item{genecol}{Character, the colour of genes (variables). The default is "gray25".}
  \item{genelabels1, genelabels2}{A vector of variables labels, by default the row.names of each input matrix
   df1, and df2 are used}

 
  \item{\dots}{further arguments passed to or from other methods}
}

\details{

CIA has been successfully applied to the cross-platform comparison (meta-analysis) of microarray 
gene expression datasets (Culhane et al., 2003). Please refer to this paper and the vignette for help
in interpretation of the output from CIA.

Co-inertia analysis (CIA) is a multivariate method that identifies trends or co-relationships
in multiple datasets which contain the same samples. That is the rows or columns of the matrix have to 
be weighted similarly and thus must be "matchable".  In \code{cia}, it is assumed that the analysis is being performed
on the microarray cases, and thus the columns will be matched between the 2 datasets. Thus please
ensure that the order of cases (the columns) in df1 and df2 are equivalent before performing CIA.

CIA simultaneously finds ordinations (dimension reduction diagrams) from the datasets that 
are most similar. It does this by finding successive axes from the two datasets with maximum covariance. 
CIA can be applied to datasets where the number of variables (genes) far exceeds the 
number of samples (arrays) such is the case with microarray analyses.

\code{cia} calls \code{\link[ade4:coinertia]{coinertia}} in the ADE4 package. For more information on 
coinertia analysis please refer to \code{\link[ade4:coinertia]{coinertia}}  and several recent reviews (see below).

In the paper by Culhane et al., 2003, the datasets df1 and df2 are transformed using COA and Row weighted COA respectively, before
coinertia analysis.  It is now recommended to perform non symmetric correspondence analysis (NSC) rather than correspondence analysis 
(COA) on both datasets.  

\bold{The RV coefficient}

In the results, in the object \code{cia} returned by the analysis, \$coinertia\$RV gives the RV coefficient.
This is a measure of global similarity between the datasets, and is a number between 0 and 1. The closer it
is to 1 the greater the global similarity between the two datasets. 

\bold{Plotting and visualising cia results}

\code{plot.cia} draws 3 plots.   

The first plot uses \code{S.match.col} to plots the projection (normalised scores \$mY 
and \$mX) of the samples
from each dataset onto the one space.  Cases (microarray samples) from one dataset are represented by circles, 
and cases from the second dataset are represented by arrow tips. Each circle and arrow is joined by a line, 
where the length of the line is proportional to the divergence between the gene expression profiles of that 
sample in the two datasets.  A short line shows good agreement between the two 
datasets.  

The second two plots call \code{plot.genes} are show the projection of the variables (genes, \$li and \$co) 
from each dataset in the new space. It is important to note both the direction of project of Variables 
(genes) and cases (microarray samples). Variables and cases that are projected in the same direction 
from the origin have a positive correlation (ie those genes are upregulated in those microarray samples)


\emph{Please refer to the help on \code{\link[made4:bga]{bga}} for further discussion on graphing and visualisation
functions in MADE4.}


}
}
\value{
An object of the class \code{cia} which contains a list of length 4.
  \item{call}{list of input arguments, df1 and df2} 
  \item{coinertia}{A object of class "coinertia", sub-class \code{dudi}. See
    \code{\link[ade4:coinertia]{coinertia}} }
  \item{coa1}{Returns an object of class "coa" or "nsc", with sub-class
    \code{\link[ade4:dudi]{dudi}}. See \code{\link[ade4:dudi.coa]{dudi.coa}} or \code{\link[ade4:dudi.nsc]{dudi.nsc}}}
  \item{coa2}{Returns an object of class "coa" or "nsc", with sub-class \code{\link[ade4:dudi]{dudi}}.
  See \code{\link[ade4:dudi.coa]{dudi.coa}} or \code{\link[ade4:dudi.nsc]{dudi.nsc}}}

}
\references{ Culhane AC, et al., 2003 Cross platform comparison and
  visualisation of gene expression data using co-inertia analysis. BMC Bioinformatics. 4:59   }
\author{Aedin Culhane }
\note{ 

}

\seealso{ See also  \code{\link[ade4:coinertia]{coinertia}},
  \code{plot.cia}}
\examples{
data(NCI60)
print("This will take a few minutes, please wait...")

if (require(ade4, quiet = TRUE)) {
# Example data are "G1_Ross_1375.txt" and "G5_Affy_1517.txt"
coin <- cia(NCI60$Ross, NCI60$Affy)
}
coin
# ciares$RV will give the RV-coefficient, the greater (scale 0-1) the better   
cat(paste("The RV coefficient is a measure of global similarity between the datasets.\n",
"The two datasets analysed are very similar. ",
"The RV coefficient of this coinertia analysis is: ", coin$coinertia$RV,"\n", sep= ""))
plot(coin)
plot(coin, classvec=NCI60$classes[,2], clab=0, cpoint=3)


}
\keyword{multivariate}
\keyword{hplot}

