"graph1D" <- 
function (dfx,  classvec=NULL, ax = 1, hor=FALSE, s.nam=row.names(dfx), n=NULL, scaled=TRUE, col="red", width=NULL, ...) {

    s.nam<-as.vector(s.nam)
    
    if(!is.null(n)) {
      spec.ind<-genes1d(dfx, n=n)
      print(spec.ind)
	}

    btt<- function(x){
           for (i in  c(1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1, 1e1, 1e2, 1e3, 1e4,1e5)){
             if(min(x)>=min(i*(-1))&& max(x)<=max(i)) return(x*10/i)
             }
         } 


    plot.new()

 
    if (is.null(classvec)) {
	if (length(col)==1) col=as.character(rep(col, length(dfx)))
	}

    if (!is.null(classvec)) {
        cols=col
	if (!length(cols) == length(levels(classvec))) {
          #print("Using default colours in getcol() as number of colours < the number of levels in classvec")
          cols = getcol(1:length(levels(classvec)))
          }

	col = as.character(factor(classvec, labels = cols))

	}
    
    if (!is.vector(dfx)) dfx<- dfx[, ax]
    if (scaled) dfx<-btt(dfx)

    if (is.null(width)) width=c(-2,1)
  
    margin=1

    if(!hor) {
        par(usr = c(width[1],  width[2], (floor(min(dfx))-margin), (ceiling(max(dfx))+margin)))

    	for (i in c(1:length(dfx))) {
        	points(0, dfx[i], col = col[i], ...)
        	if (is.null(n)) text(-0.2, dfx[i], s.nam[i], adj = c(1, 0.5), ...)
    		}

	if(!is.null(n)) {
        	for (i in spec.ind) text(-0.2, dfx[i], s.nam[i], adj = c(1, 0.5), ...)
		}

    	abline(v = 0)
    }

    if(hor){

        par(usr = c((floor(min(dfx))-margin), (ceiling(max(dfx))+margin), width[1], width[2]))
    	for (i in c(1:length(dfx))) {
  		points(dfx[i], 0, col = col[i], ...)
        	if (is.null(n)) text(dfx[i], -0.2, s.nam[i], adj = c(0, 0.5), srt = -90, ...)
    		}

    	if(!is.null(n)) {
        	for (i in spec.ind) text(dfx[i], -0.2, s.nam[i], adj = c(0, 0.5), srt = -90,  ...)
    		}

    	abline(h = 0)
    	}

}

 

