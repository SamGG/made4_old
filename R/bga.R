"bga" <-
function(dataset, classvec, type="coa",...){
        # This function runs BGA only.

        # checkfac (classvec)
        classvec<-checkfac(classvec)             
	nclasses=length(levels(classvec))

        # Run the ordination part
        data.ord <- ord(dataset, type=type, classvec=classvec)
        data.tr.ord <-t.dudi(data.ord$ord)

        # Run Between Group analysis, and return class dudi.bga
	data.tr.bet<-between(data.tr.ord,classvec, scannf=FALSE, nf=nclasses-1)
        res<-list(ord=data.tr.ord, bet=data.tr.bet, fac=classvec)
        class(res) <- c(type, "bga")
	return(res)
	}


"plot.bga" <-
function(x, axes1=1, axes2=2, arraycol=NULL, genecol="gray25", nlab=10, genelabels= NULL,...){
       # Produce a graph of arrays, genes, biplot and eigenvalues for graphing between results
   
       dudi.bga<-x

       if (!inherits(dudi.bga, "bga")) 
          stop("Object of class bga expected")

       if (is.null(arraycol)) arraycol=getcol(1:length(levels(dudi.bga$fac)))
       if (is.null(genelabels)) genelabels=rownames(dudi.bga$bet$co)

       if (dudi.bga$bet$nf==1) {
      	 	par(mfrow=c(1,3))			        # Display 2x2 graphs
       		between.graph(dudi.bga,  ax=1, hor=FALSE, ...)
       		graph1D(dudi.bga$bet$ls, ax=1, classvec=dudi.bga$fac,col=arraycol, ...)    # Draw plot of arrays
       		graph1D(dudi.bga$bet$co, ax=1, s.nam=genelabels, n=nlab, ...)   # Draw plot of genes label top genes  
      		
        }



       if (dudi.bga$bet$nf>1) {
       		par(mfrow=c(2,2))			        # Display 2x2 graphs
       		s.var(dudi.bga$bet$ls,  xax = axes1, yax = axes2, col = as.vector(factor(dudi.bga$fac, labels=arraycol)), ...)
      		s.groups(dudi.bga$bet$ls, classvec=dudi.bga$fac, col=arraycol, xax = axes1, yax = axes2, ...)    # Draw plot of arrays
      		plotgenes(dudi.bga$bet$co, varlabels=genelabels,nlab=nlab, colpoints=genecol, axes1=axes1,  axes2=axes2,...)   # Draw plot of genes label top genes
      		s.groups(dudi.bga$bet$ls, dudi.bga$fac,cellipse=0,col=arraycol,
               	 add.plot=TRUE, xax = axes1, yax = axes2,...)  # To gene plot, add arrays
       		scatterutil.eigen(dudi.bga$bet$eig) 	        # Draw plot of eigenvalues
	}
	
	}


"between.graph" <-function (x, ax = 1, cols = NULL, hor = TRUE, scaled=TRUE, centnames=NULL, varnames=NULL, ...) 
{
    if (!inherits(x, "bga")) 
        stop("Object of class dudi.bga expected")
    bels <- x$bet$ls
    beli <- x$bet$li
    classvec <- x$fac
    if (is.null(cols)) 
        cols <- getcol(1:length(levels(classvec)))
    btt <- function(x) {
        for (i in c(1e-05, 1e-04, 0.001, 0.01, 0.1, 1, 10, 100, 
            1000, 10000, 1e+05)) {
            if (min(x) >= min(i * (-1)) && max(x) <= max(i)) {
                scalefactor = i
                return(scalefactor)
            }
        }
    }
    li.nam <- row.names(beli)
    s.nam <- row.names(bels)

    if (!is.null(centnames))   li.nam = centnames
    if (!is.null(varnames))   s.nam =  varnames

    if (scaled) {
    bels.scale <- btt(bels[, ax])
    bels <- (bels * 10)/bels.scale
    beli <- (beli * 10)/bels.scale
    }
    beli <- beli[, ax]
    bels <- bels[, ax]
    margin = 1
    par(usr = c(floor(min(bels, beli)) - margin, ceiling(max(bels, 
        beli)) + margin, -2, 3))
    graph1D(bels, ax = ax, hor = hor, s.nam = s.nam, col = factor(classvec, 
        labels = cols), width = c(-2, 3), scaled = scaled, ...)
    if (hor) {
        points(beli, rep(1.5, length(beli)), ...)
        for (i in c(1:length(beli))) {
            text(beli[i], 1.7, adj = c(0.5, 0), li.nam[i])
        }
        abline(h = 1.5)
        for (i in c(1:length(levels(classvec)))) {
            Ind = which(classvec == levels(classvec)[i])
            bels.sub <- bels[Ind]
            sapply(bels.sub, segments, y0 = 0, x1 = beli[i], 
                y1 = 1.5, col = cols[i], ...)
        }
    }
    if (!hor) {
        points(rep(1.5, length(beli)), beli, ...)
        for (i in c(1:length(beli))) {
            text(1.7, beli[i], adj = c(0.5, 0), li.nam[i])
        }
        abline(v = 1.5)
        for (i in c(1:length(levels(classvec)))) {
            Ind = which(classvec == levels(classvec)[i])
            bels.sub <- bels[Ind]
            sapply(bels.sub, segments, x0 = 0, y1 = beli[i], 
                x1 = 1.5, col = cols[i], ...)
        }
    }
}
