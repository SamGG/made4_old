"rotate3d" <-
function(dataset, x=1, y=2, z=3, beg=180, end=360, step=12, savefiles=FALSE,  classvec=NULL,
                                 classcol=NULL, col=NULL,...){
        # This functions called do3d. do3d produces plots using scatterplot3d.  
	seq=seq(beg,end,step)
        
	a<-round(sqrt(length(seq)))
	par(mfrow=c(a,a+1))
	for (angle in seq) (do3d(dataset, x=x, y=y, z=z, angle=angle, classvec=classvec,
                                 classcol=classcol, col=col,...))
        
        if(savefiles){
          dev.copy(pdf, paste("rotate3d_axes", x, y, z, "-saving", length(seq),".pdf",sep=""))
          dev.off(which = dev.cur())
          }
	}
