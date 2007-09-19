heatplot<-function (dataset, dend = c("both", "row", "column", "none"),  cols.default = TRUE, lowcol = "green", highcol = "red", scale="row",  classvec=NULL, classvec2=NULL,  ...) 
{
    library(gplots)
    data <- array2ade4(dataset)
    data <- as.matrix(data)
 

    distEisen <- function(x, use = "pairwise.complete.obs") {
        co.x <- cor(x, use = use)
        dist.co.x <- 1 - co.x
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
    cols.gentleman <- function() {
        library(RColorBrewer)
        hmcol <- colorRampPalette(brewer.pal(10, "RdBu"))(256)
        return(rev(hmcol))
    }

    if (cols.default) 
        plotcols = cols.gentleman()
    else plotcols = cols()

    #dendrogram = c("both","row","column","none"),

    # backward compliance
    
    
    if (dend==TRUE) dend="both"
    if (dend==FALSE) dend="none"

    dend <- match.arg(dend)
    
    Colv <- FALSE
    Rowv <- FALSE

    dend=tolower(dend)

    if (dend%in% c("row", "r")) Rowv= as.dendrogram(hclust(distEisen(t(data)), method = "ave"))
    if (dend%in% c("column", "col", "c")) { 
             Colv=as.dendrogram(hclust(distEisen(data), method = "ave"))
             dend="column"}
    if (dend%in% c("both","TRUE")) { 
         Colv=as.dendrogram(hclust(distEisen(data), method = "ave"))
         Rowv= as.dendrogram(hclust(distEisen(t(data)), method = "ave"))
         dend="both"
	}
    

    
 
    RSideColors= CSideColors =NULL
    if (any(!is.null(classvec), !is.null(classvec2))) {
       
       proc.classvec<-function(classvec){ 
         classvec=as.factor(classvec)
         SideCols= factor(classvec, labels=getcol(length(levels(classvec))))
         SideCols= as.character(SideCols)
         nSC = length(SideCols)
         return(list(nSC, SideCols))
	 }

       if (!is.null(classvec)) {
                out = proc.classvec(classvec)
		nSC= out[[1]]
                SideCols = out[[2]]
		if (!nSC %in% dim(data)) print("Can't use classvec, length not equal to nrow or ncol in data")
                if (nSC == nrow(data)) RSideColors=SideCols 
		if (nSC == ncol(data)) CSideColors= SideCols
		}
       if (!is.null(classvec2)) {
                out = proc.classvec(classvec2)
		nSC= out[[1]]
                SideCols = out[[2]]
                if (!nSC %in% dim(data)) print("Can't use classvec2, length not equal to nrow or ncol in data")
                if (nSC == nrow(data)) RSideColors= SideCols 
		if (nSC == ncol(data)) CSideColors= SideCols
		}
       
       if (all(is.null(RSideColors),is.null(CSideColors))) heatmap.2(data, Colv = Colv, Rowv = Rowv, col = plotcols,   scale=scale, trace="none",  density.info="none", dendrogram=dend,...)
       if (all(!is.null(RSideColors),is.null(CSideColors))) heatmap.2(data, Colv = Colv, Rowv = Rowv, col = plotcols, scale=scale, trace="none",  density.info="none", RowSideColors= RSideColors,dendrogram=dend,...)
       if (all(is.null(RSideColors),!is.null(CSideColors))) heatmap.2(data, Colv = Colv, Rowv = Rowv, col = plotcols, scale=scale, trace="none",  density.info="none",  ColSideColors= CSideColors,dendrogram=dend,...)
       if (all(!is.null(RSideColors),!is.null(CSideColors))) heatmap.2(data, Colv = Colv, Rowv = Rowv, col = plotcols, scale=scale, trace="none",  density.info="none", RowSideColors= RSideColors, ColSideColors= CSideColors, dendrogram=dend, ...)       
       }
    else 
        heatmap.2(data, Colv = Colv, Rowv = Rowv, col = plotcols, scale=scale, trace="none",  density.info="none", dendrogram=dend, ...)
    


}
