"cia" <-
function(df1, df2, cia.nf=2, cia.scan=FALSE, nsc=TRUE, ...){
      	# If nsc=FALSE, this function performs CIA on two datasets as described
      	# by Culhane et al., 2003, using COA on two datasets, df1, df2, which
      	# are subjected to COA and Row weighted COA respectively,
      	# and then coinertia analysis, 
        # It is now recommended to perform NSC rather than COA on both datasets, nsc=TRUE.

      	# Example data are "G1_Ross_1375.txt" and "G5_Affy_1517.txt"
        #print(cia.nf)
        if(nsc){
          # Coinertia analysis using non symmetric COA
 
          df1<-array2ade4(df1, pos=TRUE, trans=FALSE)
          df2<-array2ade4(df2, pos=TRUE, trans=FALSE)

          coa1<-t(dudi.nsc(df1, scannf=FALSE, nf=cia.nf))
          coa2<-t(dudi.nsc(df2, scan=FALSE, nf=cia.nf))
          #print(cia.nf)
          coin<-coinertia(coa1, coa2, nf=cia.nf, scan=cia.scan, ...)
        }

        if(!nsc){
          df1<-array2ade4(df1, pos=TRUE)
          df2<-array2ade4(df2, pos=TRUE)
        
          coa1 = dudi.coa(df1, scannf=FALSE, nf=cia.nf)
	  coa2 = dudi.rwcoa(df2, rowweights=coa1$lw, scannf=FALSE, nf=cia.nf)
          coin<-coinertia(coa1, coa2, scan=cia.scan, nf=cia.nf, ...)
        }
        
        # ciares$RV will give the RV-coefficient, the greater (scale 0-1) the better   
        call=match.call()
	ciares=list("call"=call, "coinertia"=coin, "coa1"=coa1, "coa2"=coa2)

   
        class(ciares) <- "cia"
	return(ciares)		
	}


"plot.cia" <-
function(x, nlab=10,axis1=1, axis2=2,genecol="gray25",genelabels1=rownames(ciares$co), genelabels2=rownames(ciares$li), ... ){
        # Graph from coinertia analysis, using draw to plot variables (nlab.. no of labelled genes)
        coin<-x
        #print(coin$call)
        if (!inherits(coin, "cia")) 
          stop("Object of class cia expected")

        ciares<-coin$coinertia
        if (!inherits(ciares, "coinertia")) 
          stop("Object of class coinertia expected")

        if (ciares$nf==1) {
        warnings("One axis only : not yet implemented")
        return(invisible()) }
              
	layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE)) 	# Display 3 graphs, 1 on top, 2 below
	s.match.col(ciares$mX, ciares$mY, xax = axis1, yax = axis2,
                label=row.names(ciares$mX), sub=paste("CIA of df1", coin$call[2],"and df2", coin$call[3], sep=" "), ...) # Draw plot matching two normed scatters
	plotgenes(ciares$co, genelabels=genelabels1,nlab=nlab,colpoints=genecol, axis1=axis1,
             axis2=axis2, sub=paste("variables df1", coin$call[2], sep= " "))		 # Draw plot of genes label, top 5 genes
	plotgenes(ciares$li,genelabels=genelabels2,nlab=nlab,colpoints=genecol, axis1=axis1,
             axis2=axis2, sub=paste("variables df2", coin$call[3], sep= " "))		 # Draw plot of genes label, top 5 genes
             	}

