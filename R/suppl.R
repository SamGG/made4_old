"suppl" <-
function(dudi.bga, supdata,supvec=NULL, assign=TRUE, ...){
        # Calculate the assigment of test samples using simple weighted mean method described by Culhane et al., 2002
          
        assign.thres<-function(thresres, newcoord, closest.centre, ...){               
          if(!inherits(thresres, "thresres")) stop("thresres should be output file from threshold")

          thres.matrix<-thresres$thresholds
          thres.wts<-thresres$wts

          if (!length(thres.matrix)==length(thres.wts)) stop("thresholds and weights matrix of unequal dimesions")
        
          if (!ncol(newcoord)==ncol(thres.matrix)) stop("no of axes incorrect")
          nclasses= nrow(thres.matrix)
          if(!nclasses== (ncol(newcoord)+1))stop("Incorrect no of classes")
          ntest<-nrow(newcoord)
          
          ass<-matrix(NA, nrow=ntest, ncol=1)
          
          
          for (i in c(1:ntest)) {         
            coord<-newcoord[i,]
            ass[i]<-closest.centre[i]
            t.ass<-c(NA)
            for (j in c(1:nclasses)) {
              wt<- thres.wts[j,]
              thr<-thres.matrix[j,]
              # The weights matrix, is the no of training samples assigned by each axis coordinate
              # Use to calculate which axes discriminate which group.
              t.ass[j]<-sum(1-wt[which((thr<0 &  coord<thr))], wt[which(thr>0 &  coord>thr)])
            }
            
            new.ass<-if(max(t.ass)>=1) which.max(t.ass)
            if(!is.null(new.ass))ass[i]<-new.ass
            
          }
          return(ass)             
        }       
     


        suppl.coord<-function(dudi.bga, supdata, ...){
          # This returns the co-ordinates of supplementary (test) samples using suprow() and rowproj()
          # The input is dudi.bga, a list contain the results of the ordination and between analysis and the classvec
          # and supdata, a data.frame (or matrix etc) of test data. It must contain the same no of
          # variables (genes) as the training data.  Suppl.coord, calls suprow (input, ordination results, and supdata)
          # to transform the supdata (centred, normalised, chi-sq) similar to the training data. This data is then
          # projected onto the between axes using rowproj (input, between() results, and $tabsup from suprow).
          
          if (!inherits(dudi.bga, "bga"))
            stop("Object of class bga expected. This is obtained by running bga()")
          
          posdudi=c("coa", "nsc")
          testpos<- (inherits(dudi.bga, posdudi))
          
          data.tr<-array2ade4(supdata, pos=testpos,...)
          if (!is.data.frame(data.tr)) stop("Problems transposing data")

          #   Suprow should call suprow.coa or suprow.pca depending on class(dudi.ord)
          suptrans<-suprow(dudi.bga$ord, data.tr)
          supResults<-rowproj(dudi.bga$bet, suptrans$tabsup)

          return(supResults$lisup)
	}	


        rowproj<-function (x, Xsup, ...) {
          # This projects points onto a dudi, without any transformation. 
          # This is required for projection of points on a between analysis
          # First use suprow to tranform the table (chi-square for coa, or centred/normalised 
          # for PC). Then use rowproj.  

          Xsup <- data.frame(Xsup)
          if (!inherits(x, "dudi")) 
            stop("Object of class 'dudi' expected")
          if (!inherits(Xsup, "data.frame")) 
            stop("Xsup is not a data.frame")
          if (ncol(Xsup) != ncol(x$tab)) 
            stop("non convenient col numbers")
          coosup <- as.matrix(Xsup) %*% as.matrix(x$c1)  # modification of suprow.default for projection of points without mass
          coosup <- data.frame(coosup, row.names = row.names(Xsup))
          names(coosup) <- names(x$li)
          return(list(tabsup=Xsup, lisup=coosup))             
        }


        elucidean.dist<-function(vec1,vec2){
           # This take in two vectors, vec1 and vec2 and calculates the elucidean distance between them
           # vec1 must be a matrix or data.frame, vec2 can be 

          checkvec<-function(x) {
            x<-matrix(as.numeric((x)))
            if(!is.numeric(x)) stop(paste("class is", class(x), "Not a matrix",  sep=" "))
            return(x)
          }
          vec1<-checkvec(vec1)
          vec2<-checkvec(vec2)
          
          if(!nrow(vec1)==nrow(vec2)) stop("unequal vector lengths")
          
          eucl<-function(x,y){
            sqdiff<-function(x,y) (x-y)^2
            return(sqrt(sum(sqdiff(x,y))))
          }
          
          dist<-eucl(vec1,vec2)
                                      
          return(dist)
	}

        centdist<-function(coord.beli, coord.test, ...) {
	  # Reads in the .beli, centres of groups from bga analysis and co-ordinates of a test point
	  # It tests the closest centre to the test point
        
          nclasses= nrow(coord.beli)
	  naxes = nclasses -1
          if (!naxes == ncol(coord.beli)) stop("no of axes is not equal to the no of classes -1!")
          
          
          centroid.dist<-matrix(NA, ncol=nclasses, nrow=nrow(coord.test))
          colnames(centroid.dist)<-rownames(coord.beli)

          for (i in c(1:nclasses)){
            cent<-coord.beli[i,]
            if (!naxes == length(cent)) stop("no of axes not right")
            dist<-apply(coord.test, 1, elucidean.dist, vec2=coord.beli[i,]) 
            centroid.dist[,i]<-dist        
          }
          
          closest.class<-apply(centroid.dist, 1, which.min)
          #This is the class (1,2,3 etc) to which the co-ordinates is closest
          
          return(list(closest.class=closest.class, distances=centroid.dist))
        } 

        threshold<-function(coord.bels, classvec,...){
          # Calculates a weighted mean. Given .bels datafile and classvector.
          # We used a simple weighted mean to determine when 
          # a projected sample fell within/outside each group. See Culhane et al., 2002 for
          # details. This was a very crude measure and could be improved using ML or probabilities.
          
          if (is.vector(classvec)) classvec<-factor(classvec)
          if (!is.factor(classvec)) stop("classvec not a factor") 
          nclasses=length(levels(classvec))

          naxes = nclasses -1
          if (!naxes == ncol(coord.bels)) stop("no of axes is not equal to the no of classes -1!")

          calc.threshold<-function(coord.bels, classfac, ...){ 
            nclasses<-length(levels(classfac))
            data1<-coord.bels[which(classfac==levels(classfac)[i])]
            data2<-coord.bels[which(!classfac==levels(classfac)[i])]

            # Calculate a simple weighted mean threshold have checked formula.. it is working correctly
            threshold=sum(prod(mean(data1),sd(data2)), prod(mean(data2),sd(data1)))/sum(sd(data1), sd(data2))
            weight= length(which(data1>threshold))/length(data1)
            return(cbind("thres"=threshold, "weight"=weight))            
          }

          thresholds<-matrix(NA, nrow=nclasses, ncol=naxes)
          wts<-matrix(NA, nrow=nclasses, ncol=naxes)
          for (i in c(1:nclasses)) {
            thres<-apply(coord.bels, 2, calc.threshold, classfac=classvec)
            thresholds[i,]<-thres[1,]
            wts[i,]<-thres[2,]

          }
          dimnames(thresholds)<-list(levels(classvec),paste("Thres", colnames(coord.bels), sep=""))
          dimnames(wts)<-list(levels(classvec),paste("WT",colnames(coord.bels),sep=""))
          out<-list("thresholds"=thresholds, "wts"=wts)
          class(out)<-c(class(out),"thresres")
          return(out)  
	}

        
        suppl.res<-suppl.coord(dudi.bga, supdata,...)
        
	if (assign){
          thres<-threshold(dudi.bga$bet$ls, dudi.bga$fac)
          closest.cent<-centdist(dudi.bga$bet$li, suppl.res)
          thres.res<-assign.thres(thres, suppl.res, closest.cent$closest.class)

          ## OLD VERSION... BUGGER  
          #Format results for output
          #if (!length(closest.cent$closest.class)==1) {
          #  cc=factor(closest.cent$closest.class, labels=levels(dudi.bga$fac))
          #  pc=factor(thres.res, labels=levels(dudi.bga$fac))
          #  out<-cbind("projected"=suppl.res, "closest.centre"=cc, "predicted"=pc)
          #}
          # # For Jackknifing (suppl nrow=1.. so slightly different)
          #if (length(closest.cent$closest.class)==1)  {
          #  out=cbind("projected"=suppl.res, "closest.centre"=closest.cent$closest.class, "predicted"=thres.res)
          #}


          # New version 
          # Format results for output  (Ian checked with Khan/Golub/florent's data)
          # Seems to work 4th Nov 2004
            cc = levels(dudi.bga$fac)[closest.cent$closest.class]
            pc = levels(dudi.bga$fac)[thres.res]
            out <- cbind(projected = suppl.res, closest.centre = cc,predicted = pc)

        }

        if (!assign) out<-suppl.res
        
        # Add true class
        if(!is.null(supvec)) out=cbind(out, "true.class"=supvec)
        
	return(out)
	}
