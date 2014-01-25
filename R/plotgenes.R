"plotgenes" <-
function(coord, nlab=10, axis1=1, axis2=2, genelabels=row.names(coord), boxes=TRUE, colpoints="black",  ...){
	# This function plots a graph, with output from genes (ie only labelling genes at ends of axis).
        # This function calls s.var, and parameters such as colpoints can be passed to it.
       

        # If labels are obtained using annaffy or annotate, Symbol often contains "" values
        # In which case s.var will fall over. Thus replace empty values with a -
  
      if (!inherits(coord, "data.frame")) {
          if (inherits(coord, "bga")){coord = coord$bet$co }

           if (inherits(coord, "between"))  coord = coord$li
           if (inherits(coord, "ord")) {
              coord = coord$ord$li
              }
           if  (inherits(coord, "dudi"))  coord = coord$li
        }
        
  
  	if(!inherits(labels, "character")) genelabels<-as.vector(genelabels)
        genelabels[genelabels==""]<- "-"  # Replace any null labels with "-"

        # Call the function genes() to get the subset of labels required
	specInd <- genes(coord,nlab, axis1, axis2)
	specLab <- genelabels[specInd]
	specMat <- coord[specInd,]

        # Plot using s.var, the first call of s.var uses points, the second uses
        # scatterutil.eti to draw the labels
        
	s.var(coord, xax = axis1, yax = axis2, clabel=0, colpoints=rep(colpoints, nrow(coord)),...)
	s.var(specMat, xax = axis1, yax = axis2, label=specLab,  boxes=boxes, cpoint=0, add.plot=TRUE, ...)
	}



"genes" <-
function(dudivar,n=5, axis1=1, axis2=2){
	# This is just a quick function to make 2D graphs of variables prettier, by only
	# labelling specific genes, ie those at the extreme ends of axis
	# dudivar is variable, $li file from a dudi (COA, PCA etc)
	# n is the number of genes to be labelled, for example top 5, 10 etc 
	# axis1 and axis2 are the axis to be drawn, for example principal component 1 and 2
        # This function is not run on its own but is called by plotgenes()

	#cat(axis1,axis2)
	len=nrow(dudivar)
	f1<-rank(dudivar[,axis1])
	f2<-rank(dudivar[,axis2])
	fInd <- 1:len
	fMat <- cbind(fInd, f1, f2)

	getTop <- function(x, n){
                # This function written by JM
		if(x[2]<=n|x[2]>len-n|x[3]<=n|x[3]>len-n){
			out <- x[1]
		}
		else{
			out <- -100
		}
	return(out)
	}

	specInd <- apply(fMat, 1, getTop, n)
	specInd <- specInd[specInd > 0]
	return(specInd)
}
