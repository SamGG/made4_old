"array2ade4" <-
function(dataset, pos=FALSE,  trans=TRUE){

        if (!is.data.frame(dataset)) dataset<-getdata(dataset)  # Allows matrix, data.frame, exprSet, marrayRaw to be read as data.frame
        
        if (any(is.na(dataset)))
             stop("Arraydata must not contain NA values. Use impute.knn in library(impute), KNNimpute from Troyanskaya et al., 2001 or LSimpute from Bo et al., 2004 to impute missing values\n")

   
	# COA needs table of positive data, will add real no to make +ve
	if(pos){
               if (any(dataset < 0)) {
                   num<-round(min(dataset)-1)
                   dataset<-dataset+abs(num)
		  }
	       }

        if(trans) {
               # Transpose matrix  (as BGA, CIA expects the samples to be in the rows)
               # dudi.nsc should not be transposed, use t.dudi instead to ensure row weight are equal
                  
               dataset<-t(dataset)		
               dataset<-data.frame(dataset)
               if (!is.data.frame(dataset)) stop("Problems checking dataset")
        }
        
        data.out<-dataset        
        return(data.out)
	}
