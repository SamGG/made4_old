"ord" <-
function(dataset, type="coa", classvec=NULL, ord.nf=NULL, trans=FALSE, ...){
        # This function runs ordination (PCA, or COA) on gene expression data
  
        # array2ade4(dataset, if coa or nsc needs to be positive)
        posdudi=c("coa", "nsc")
        testpos<- ifelse(type %in% posdudi, TRUE, FALSE)           
        data.tr<-array2ade4(dataset, pos=testpos, trans=trans, ...)

	if (!is.data.frame(data.tr))
		stop("Problems transposing data")
        
        # determine no of eigenvalues for ordination and run ordination
        if (is.null(ord.nf)) ord.nf<-ifelse(nrow(data.tr)<ncol(data.tr), nrow(data.tr)-1, ncol(data.tr)-1)
   
        data.tr.ord<-switch(type,
                "coa" = dudi.coa(data.tr,scannf=FALSE, nf=ord.nf),
                "pca" = dudi.pca(data.tr,scannf=FALSE, nf=ord.nf),
                "nsc" = dudi.nsc(data.tr,scannf=FALSE, nf=ord.nf),
                stop(paste("The type of transformation", type, "was not recognised. Permitted are: coa, pca, nsc", sep=" ")) 
                )

        # Run Ordination analysis, and return class dudi.bga
        res<-list(ord=data.tr.ord, fac=classvec)
        class(res) <- c(type, "ord")
	return(res)
	}


"plot.ord" <-
function(x, axis1=1, axis2=2, arraycol=NULL, genecol="gray25", nlab=10, genelabels= NULL, arraylabels=NULL,classvec=NULL, ...){
       # Produce a graph of arrays, genes, biplot and eigenvalues for graphing between results
   

       if (!inherits(x, "ord")) 
          stop("Object of class ord expected")

       dudi.ord<-x$ord
       fac<-x$fac

       if (!is.null(classvec)) fac= checkfac(classvec)

       if (is.null(genelabels)) genelabels=rownames(dudi.ord$li)
       if (is.null(arraylabels)) arraylabels= rownames(dudi.ord$co)


       par(mfrow=c(2,2))  # Display 2x2 graphs

       # If there is class information on samples, colour these

       if (!is.null(fac) && !length(arraycol)==length(levels(fac)))  arraycol=getcol(1:length(levels(fac)))

        if (is.null(arraycol) | length(arraycol)==1) {
		cols.array=rep(arraycol, nrow(dudi.ord$co))  # Colours for s.var  for only 1 colour
		biplot="type1"
	}
       

      	if (length(arraycol)>1) {
		cols.array =as.vector(factor(fac, labels=arraycol))  # Colours for s.var for classvec with > 1 colour
		biplot="type2"
	}

	# Draw eigenvalue plots
       	scatterutil.eigen(dudi.ord$eig) 	        # Draw plot of eigenvalues
		

 	# Draw arrays
	s.var(dudi.ord$co,  xax = axis1, yax = axis2, col = cols.array ,label=arraylabels, ...) 

	# Draw genes 
        plotgenes(dudi.ord$li, genelabels=genelabels,nlab=nlab, colpoints=genecol, axis1=axis1,  axis2=axis2,...)   # Draw plot of genes label top genes

	
       	# do biplot
	if (inherits(dudi.ord, "coa")) {
    		plotgenes(dudi.ord$li, genelabels=genelabels,nlab=nlab, colpoints=genecol, axis1=axis1,  axis2=axis2, ...)   # Draw plot of genes label top genes	
				
		if (biplot=="type2") {
       			s.groups(dudi.ord$co, fac, cellipse=0,col=arraycol,add.plot=TRUE,  xax = axis1, yax = axis2,  ...)  # To gene plot, add arrays
			}

		if (biplot=="type1") {
			s.var(dudi.ord$co, col=rep(arraycol, nrow(dudi.ord$li)), add.plot=TRUE, label=arraylabels, xax = axis1, yax = axis2,...)  # To gene plot, add arrays
			}
		}
      	
	
	}


