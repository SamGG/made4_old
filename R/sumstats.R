"sumstats" <-
function(array, xax=1, yax=2) {
        # In ordination, such as PCA or COA, variables and cases in the same direction from origin, (same slope)
        # have strong associations. Those projected further from the origin (>dist) have strongest assoc.
        # This returns the slopes and dist from origin of a list of co-ordinates x and y.
        angle<-function(x){ 
           if(x[1]<0) return(270-(atan(x[2]/x[1])*180/pi))
           if(x[1]>0) return(90-(atan(x[2]/x[1])*180/pi))
          }

	slope<-function(x){return(x[2]/x[1])}
	dists<-function(x){return(sqrt(x[1]^2 + x[2]^2))}
	nam<-paste(colnames(array[,c(xax, yax)])[1], colnames(array[,c(xax, yax)])[2], sep="+")
	ss<-apply(array[,c(xax, yax)], 1, slope)
	ds<-apply(array[,c(xax, yax)], 1, dists)
        ang<-apply(array[,c(xax,yax)],1,angle)

	out<-cbind(ss,ang, ds)
	colnames(out)<-c(paste("Slope", nam, sep=":"),paste("Angle(deg)", nam, sep=":"),paste("Dist",nam,  sep=":"))
	return(out)
	}
