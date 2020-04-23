"do3d" <-
function(dataset, x=1, y=2, z=3, angle=40, classvec=NULL, classcol=NULL, col=NULL, cex.lab=0.3, pch=19, cex.symbols=1,
         ...){
         # calls scatterplot3d, but will colour samples given classvec.
         if(!is.null(classvec)) {
          classvec<-checkfac(classvec)
          if(is.null(classcol)) classcol= getcol(c(1:length(levels(classvec))))
          col= factor(classvec, labels=classcol)
          }
        if(is.null(col)) col="red"
        
      	xlab=paste("F",x, sep="")
	ylab=paste("F",y, sep="")
	zlab=paste("F",z, sep="")

	scatterplot3d::scatterplot3d(dataset[,x],dataset[,y],dataset[,z], color=col,
	pch=pch,cex.symbols=cex.symbols, angle=angle, xlab=xlab, ylab=ylab, zlab=zlab, 
	cex.lab=cex.lab,sub=paste("Rotation Angle:", angle, sep=" ", ...))
	}
