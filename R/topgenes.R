"topgenes" <-
function(x, n=10, axis=1, labels=row.names(x), ends="both",...){
	# Read a label or annotation file, and takes index from genes1d

         if (!inherits(x, "data.frame")) {
            if (inherits(x, "bga")) x = x$bet$co 

            if (inherits(x, "between"))  x = x$li
            if (inherits(x, "ord")) x = x$ord$li
           
            if  (inherits(x, "dudi"))  x = x$li
            
        }
        
  
        if(!inherits(labels, "character")) labels<-as.vector(labels)
        labels[labels==""]<- "-"  # Replace any null labels with "-"


	i<-switch(ends, "both"=1, "neg"=2, "pos"=3)
	if(!is.null(dim(labels))) top<-labels[genes1d(x,n=n,axis=axis, listgenes=TRUE)[[i]],]
	if(is.null(dim(labels)))  top<-labels[genes1d(x,n=n,axis=axis, listgenes=TRUE)[[i]]]

	return(top)
	}



"genes1d" <-
function(array,n=5, axis=1, listgenes=FALSE){
        # This is similar to genes, but returns an index of genes at the ends of one axes    
        # array is data.coa$li file, and n is the top 5, 10 etc values
        # *** This function is not run on its own but is called by function topgenes ***

        if (is.vector(array)) {
             len=length(array)
             f1=rank(array)
             }

        if (is.data.frame(array) | is.matrix(array)) {
             len=nrow(array)
              f1<-rank(array[,axis])
            }

        fInd <- 1:len
        fMat <- cbind(fInd, f1)

        getTop <- function(x, n){
                if(x[2]<=n|x[2]>len-n){
                        out <- x[1]
			##print(paste(x[1],x[2],sep="\t"))

                }
                else{
                        out <- -100
                }

	  return(out)
         }
	
          specInd <- apply(fMat, 1, getTop, n)
          specInd <- specInd[specInd > 0]

	 if (listgenes) {
		specInd<-list(all=specInd)
	  	specInd$axis.neg <- fMat[,1][order(fMat[,2])][1:n]
	  	specInd$axis.pos <- fMat[,1][order(fMat[,2])][len:(len-n+1)]		
		}


	  return(specInd)
	  
}

