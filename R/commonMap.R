"commonMap" <-
function(x, y, hor=TRUE,cex=1.5, scaled=TRUE, ...) {         
        # MAP OUT THE GENES IN COMMON 
        # This graphs a 1D graph, x and y are the coordinates from two different analyses
        # but the rows of each vectors correspond (ie common genes)
        # Ailis Fagan (Ailis.Fagan@ucd.ie) and Aedin Culhane, 18th May 2004
  
        #multiply (*1000) to a bigger number so it's viewable clearly on the plot
    
        btt<- function(x){
           for (i in  c(1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1, 1e1, 1e2, 1e3, 1e4,1e5)){
             if(is.na(x)) return(x)
             if(min(x)>=min(i*(-1))&& max(x)<=max(i)) return(x*1000/i)
             }
         }
        if (scaled) {
        	x<-sapply(x,btt)
	        y<-sapply(y,btt)
              }


        plot.new() 
	#set the screen size
	par(usr=c(floor(min(x,y, na.rm=TRUE)-50),ceiling(max(x,y, na.rm=TRUE)+50),-2,2))
        #print(par()$usr)
	#plot points, the first set of points (x) are plotted at +1, the second set (y) at -1
       
	for(i in 1:(length(x))){points(x[i],1, cex=cex,...)}
	for(i in 1:(length(y))){points(y[i],-1, cex=cex,...)}

        # draw horizontal lines
        if(hor) {abline(h=-1, ...); abline(h=1,...)}
        
	#join the points with lines
	for(i in 1:(length(x))){segments(x[i], 1, y[i], -1, ...)}
}
