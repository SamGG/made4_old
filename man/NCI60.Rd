\name{NCI60}
\alias{NCI60}
\docType{data}
\title{Microarray gene expression profiles of the NCI 60 cell lines}
\description{
NCI60 is a dataset of gene expression profiles of 60 National Cancer Institute (NCI)
cell lines. These 60 human tumour cell lines are derived from patients with leukaemia, melanoma, 
along with, lung, colon, central nervous system, ovarian, renal, breast and prostate cancers. 
This panel of cell lines have been subjected to several different DNA microarray studies using
both Affymetrix and spotted cDNA array technology. This dataset contains subsets from one 
cDNA spotted (Ross et al., 2000) and one Affymetrix (Staunton et al., 2001) study, and 
are pre-processed as described by Culhane et al., 2003.

}
\usage{data(NCI60)}
\format{
  The format is: List of 3
\itemize{
 \item{\$Ross:}{\code{\link{data.frame}} containing 1375 rows and 60 columns. 
  1375 gene expression log ratio measurements of the NCI60 cell lines.}
  
 \item{\$Affy:}{\code{\link{data.frame}} containing 1517 rows and 60 columns. 
  1517 Affymetrix gene expression average difference measurements of the NCI60 cell lines.}
 
 \item{\$classes:}{Data \code{\link{matrix}} of 60 rows and 2 columns.  
  The first column contains the names of the 60 cell line which were analysed.  
  The second column lists the 9 phenotypes of the cell lines, which are 
  BREAST, CNS, COLON, LEUK, MELAN, NSCLC, OVAR, PROSTATE, RENAL.}
  }
}
\details{
The datasets were processed as described by Culhane et al., 2003.

The Ross \code{data.frame} contains gene expression profiles of each cell lines in the NCI-60 panel, 
which were determined using spotted cDNA arrays containing 9,703 human cDNAs (Ross et al., 2000). 
The data were downloaded from The NCI Genomics and Bioinformatics Group Datasets resource 
\url{http://discover.nci.nih.gov/datasetsNature2000.jsp}. The updated version of this dataset 
(updated 12/19/01) was retrieved. Data were provided as log ratio values. 


In this study, rows (genes) with greater than 15% of values missing were deemed unreliable 
and were removed from analysis, reducing the dataset to 5643 spot values per cell line. 
Remaining missing values were imputed using a K nearest neighbour method, with 16 neighbours 
and a Euclidean distance metric (Troyanskaya et al., 2001). The dataset NCI60\$Ross 
contains the subsets of 1375 reported by Scherf et al., 2000.


The Affy data were derived using high density Hu6800 Affymetrix microarrays containing 
7129 probe sets (Staunton et al., 2001). The dataset was downloaded from the Whitehead Institute 
Cancer Genomics supplemental data to the paper from Staunton et al., \url{http://www-genome.wi.mit.edu/mpr/NCI60/}, where the data were provided as average difference (perfect match-mismatch) values. As described by 
Staunton et al.,  an expression value of 100 units was assigned to all average difference values 
less than 100. Genes whose expression was invariant across all 60 cell lines were not considered, 
reducing the dataset to 4515 probe sets. This dataset NCI60\$Affy of 1517 probe set, contains genes 
in which the minimum change in gene expression across all 60 cell lines was greater than 500 average 
difference units.  Data were logged (base 2) and median centred.

}
\source{
These pre-processed datasets were available as a supplement to the paper:

Culhane AC, Perriere G, Higgins DG. Cross-platform comparison and visualisation of gene expression data 
using co-inertia analysis. BMC Bioinformatics. 2003 Nov 21;4(1):59.
\url{http://www.biomedcentral.com/1471-2105/4/59}

}
\references{
Culhane AC, Perriere G, Higgins DG. Cross-platform comparison and visualisation of gene expression 
data using co-inertia analysis. BMC Bioinformatics. 2003 Nov 21;4(1):59.

Ross DT, Scherf U, Eisen MB, Perou CM, Rees C, Spellman P, Iyer V, Jeffrey SS, Van de Rijn M, 
Waltham M, Pergamenschikov A, Lee JC, Lashkari D, Shalon D, Myers TG, Weinstein JN, Botstein D, 
Brown PO: Systematic variation in gene expression patterns in human cancer cell lines.  
Nat Genet 2000, 24:227-235

Scherf U, Ross DT, Waltham M, Smith LH, Lee JK, Tanabe L, Kohn KW, Reinhold WC, Myers TG, 
Andrews DT, Scudiero DA, Eisen MB, Sausville EA, Pommier Y, Botstein D, Brown PO, 
Weinstein JN: A gene expression database for the molecular pharmacology of cancer.Nat Genet 2000, 
24:236-244.

Staunton JE, Slonim DK, Coller HA, Tamayo P, Angelo MJ, Park J, Scherf U, Lee JK, Reinhold WO, 
Weinstein JN, Mesirov JP, Lander ES, Golub TR: Chemosensitivity prediction by transcriptional 
profiling. Proc Natl Acad Sci U S A 2001, 98:10787-10792. 

Troyanskaya O, Cantor M, Sherlock G, Brown P, Hastie T, Tibshirani R, Botstein D, Altman RB:
Missing value estimation methods for DNA microarrays. Bioinformatics 2001, 17:520-525. 
}

\examples{
data(NCI60)
summary(NCI60)

}
\keyword{datasets}
