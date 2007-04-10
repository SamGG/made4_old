"heatplot"<-
function (dataset, dend = TRUE, lowcol = "green", highcol = "red", Colv=NULL, Rowv=NULL, cols.default=TRUE, ...) 
{
    data <- array2ade4(dataset)
    data<-as.matrix(data)
    distEisen <- function(x, use = "pairwise.complete.obs") {
        co.x <- cor(x, use = use)
        dist.co.x <- 1 - co.x
         #ac.x <- acos(co.x)
        return(as.dist(dist.co.x)) 
    }

    cols <- function(low = lowcol, high = highcol, ncolors = 123) {
        low <- col2rgb(low)/255
        if (is.character(high)) 
            high <- col2rgb(high)/255
        col <- rgb(seq(low[1], high[1], len = ncolors), seq(low[2], 
            high[2], len = ncolors), seq(low[3], high[3], len = ncolors))
        return(col)
    }

    cols.gentleman<-function() {
      # Colors defined by Huber, Li & Gentleman, Chapter 10, Bioinformatics & Computational
      # Biology Solutions using R & Bioconductor (2006)
      library(RColorBrewer)
      hmcol<-colorRampPalette(brewer.pal(10, "RdBu"))(256)
      return(hmcol)
    }
    if (cols.default) plotcols= cols.gentleman()  else plotcols= cols()
 
    if (!dend) {
        if (is.null(Colv)) Colv <- NA
        if (is.null(Rowv)) Rowv <- NA
    }

    if (dend) {
        if (is.null(Colv)) Colv <- as.dendrogram(hclust(distEisen(data), method = "ave"))
        if (is.null(Rowv)) Rowv <- as.dendrogram(hclust(distEisen(t(data)), method = "ave"))
    }


    heatmap(data, Colv = Colv, Rowv = Rowv, col = plotcols,...)
}



  

 


 