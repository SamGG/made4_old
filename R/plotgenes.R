"plotgenes" <-
function(dudivar, nlab=10, axes1=1, axes2=2, varlabels=row.names(dudivar), boxes=TRUE, colpoints="black",  ...){
	# This function plots a graph, with output from genes (ie only labelling genes at ends of axes).
        # This function calls s.var, and parameters such as colpoints can be passed to it.
       

        # If labels are obtained using annaffy or annotate, Symbol often contains "" values
        # In which case s.var will fall over. Thus replace empty values with a -
  
  	if(!inherits(labels, "character")) varlabels<-as.vector(varlabels)
        varlabels[varlabels==""]<- "-"  # Replace any null labels with "-"

        # Call the function genes() to get the subset of labels required
	specInd <- genes(dudivar,nlab, axes1, axes2)        
	specLab <- varlabels[specInd]
	specMat <- dudivar[specInd,]

        # Plot using s.var, the first call of s.var uses points, the second uses
        # scatterutil.eti to draw the labels
        
	s.var(dudivar, axes1, axes2, clab=0, colpoints=rep(colpoints, nrow(dudivar)),...)
	s.var(specMat, axes1, axes2, label=specLab,  boxes=boxes, cpoint=0, add.plot=TRUE, ...)
	}



"genes" <-
function(dudivar,n=5, axes1=1, axes2=2){
	# This is just a quick function to make 2D graphs of variables prettier, by only
	# labelling specific genes, ie those at the extreme ends of axes
	# dudivar is variable, $li file from a dudi (COA, PCA etc)
	# n is the number of genes to be labelled, for example top 5, 10 etc 
	# axes1 and axes2 are the axes to be drawn, for example principal component 1 and 2
        # This function is not run on its own but is called by plotgenes()

	#cat(axes1,axes2)
	len=nrow(dudivar)
	f1<-rank(dudivar[,axes1])
	f2<-rank(dudivar[,axes2])
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
