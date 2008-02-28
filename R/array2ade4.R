"array2ade4" <-
function(dataset, pos=FALSE,  trans=FALSE){

        # Allows matrix, data.frame, ExpressionSet, marrayRaw to be read as data.frame
        if (!is.data.frame(dataset)) dataset<-getdata(dataset)
 
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
               # There is a horrible bug is dudi.pca/coa etc, if a dataset with vars>>cases is given
               # It can end abruptly crashing the session. This is a bug in sweep
               # There will now use t.dudi rather than transpose the data
              
               # using t convert data.frame to matrix and messes up affymetrix probe ID names
               # It changes all of the "-" to "." in probeids like AFFX-CreX-5_at
               # So save names and change the names of the transposed matrix

               colnam= colnames(dataset)
               rownam = rownames(dataset)               
               dataset<-t(dataset)		
               dimnames(dataset) = list(colnam, rownam) 
               
               # convert matrix to data.frame for ade4
               dataset <- as.data.frame(dataset)
               
               if (!is.data.frame(dataset)) stop("Problems checking dataset")
        }
        
        data.out<-dataset        
        return(data.out)
	}
