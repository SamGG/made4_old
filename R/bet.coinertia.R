"bet.coinertia" <-
function(df1, df2, fac1, fac2, cia.nf=2, type="nsc", ...){
        #between group coinertia analysis using PCA.
    
        fac1<-checkfac(fac1)
        fac2<-checkfac(fac2)
        if(!length(levels(fac1)==length(levels(fac2)))) stop("The same number of classes is required for between class coinertia analysis")
           
        nf=length(levels(fac1))-1
       
               
        forsw<-function(dudi.bet.cia, ...){
             # Calculates the sample weights in nsc between group coinertia analysis
             # Thus can produce graph of between group centre, as well as the samples in these groups
             # This is similar to the $li and $ls coordinates in a between analysis
             # Note the $cw and not $lw are used below, as dudi not transposed.

             if (!inherits(dudi.bet.cia, "bet.cia"))  stop("Object of class bet.cia, between class coinertia expected")
             dudiC<-dudi.bet.cia$coin
             
             if (!inherits(dudiC, "coinertia"))
               stop("Object of class coinertia expected")


             normalise.w <- function(X, w) {
               # Function from coinerta analysis
               f2 <- function(v) sqrt(sum(v * v * w)/sum(w))
               norm <- apply(X, 2, f2)
               X <- sweep(X, 2, norm, "/")
               return(X)
             }

             # for nsc, dudiX and dudiY are transposed.

             if (!inherits(dudi.bet.cia, "nsc")) stop("Object of class nsc bet.cia, between class coinertia expected")

             if(inherits(dudi.bet.cia, "nsc")){
               if (!inherits(dudi.bet.cia$nsc1, "dudi"))  stop("Object of class dudi expected")
               dudiX<-t(dudi.bet.cia$nsc1)
             
               if (!inherits(dudi.bet.cia$nsc2, "dudi")) stop("Object of class dudi expected")
               dudiY<-t(dudi.bet.cia$nsc2)
             }
             
             U <- as.matrix(dudiC$c1) * unlist(dudiC$cw)
             U<- data.frame(as.matrix(dudiX$tab)%*% U)
             sX <- U

             
             U <- normalise.w(U, dudiX$lw)	
             names(U) <- paste("NormSx", (1:dudiC$nf), sep = "")
             smX <- U
             
             U <- as.matrix(dudiC$l1) * unlist(dudiC$lw)
             U<- data.frame(as.matrix(dudiY$tab)%*%U)
             sY<-U

             U <- normalise.w(U, dudiY$lw)
             names(U) <- paste("NormSy", (1:dudiC$nf), sep = "")
             smY <- U
             
             # Thus return the coordinates and normed coordinate scored of the individual samples
             
             return(list("sX"=sX, "sY"=sY, "smX"=smX, "smY"=smY))
        }
           
        out=switch(type,
               "nsc"= {
                       df1<-array2ade4(df1, pos=TRUE, trans=FALSE)
                       df2<-array2ade4(df2, pos=TRUE, trans=FALSE)
                       nsc1<-dudi.nsc(df1, scannf=FALSE, ...)
                       bet1<-bca(t(nsc1), fac1, scannf=FALSE, nf=nf, ...)
                       nsc2<-dudi.nsc(df2, scannf=FALSE, ...)
                       bet2<-bca(t(nsc2), fac2, scannf=FALSE, nf=nf,...)
                       coin<-coinertia(bet1,bet2, scannf=FALSE, nf=cia.nf, ...)
                       out<-list("coin"=coin, "nsc1"=nsc1, "nsc2"=nsc2, "bet1"=bet1, "bet2"=bet2)
                       class(out)<-c("bet.cia", "nsc")
                     },
               "pca"={
                     df1<-array2ade4(df1)
                     df2<-array2ade4(df2)
                     pca1<-dudi.pca(df1, scan=FALSE)
                     bet1<-bca(pca1, fac1, scannf=FALSE, nf=nf, ...)
                     pca2<-dudi.pca(df2, scannf=FALSE, ...)
                     bet2<-bca(pca2, fac2, scannf=FALSE, nf=nf, ...)
                     coin<-coinertia(bet1,bet2, scannf=FALSE, nf=cia.nf, ...)
                     out<-list("coin"=coin, "pca1"=pca1, "pca2"=pca2, "bet1"=bet1, "bet2"=bet2)
                     class(out)<-c("bet.cia", "pca")},
          
               stop(paste("The type of transformation", type, "was not recognised. Permitted are: pca or nsc", sep=" "))
             )
           
	return(out)
	}
