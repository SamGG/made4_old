"comparelists" <-
function(dx,dy, ...) {
        # Given two vectors, report on there similarity and difference
        # Often used if comparing genelists after filtering
  
        if(!is.vector(dx)) 
             stop("the first vector is not a vector")

        if(!is.vector(dy)) 
             stop("the second vector is not a vector")
	
	inter<-intersect(dx, dy)
	setd <- setdiff(dx, dy)

	xiny<-length(dx[dx%in%dy])
	yinx<-length(dx[dy%in%dx])

	
        compres<-list("intersect"=inter, "Set.Diff"=setd, "XinY"=xiny, "YinX"=yinx, "Length.X"=length(dx), 
        "Length.Y"=length(dy))
        class(compres)="comparelists"
	return(compres)
}


"print.comparelists"<-
      function(x, ...)  {
        if (!inherits(x, "comparelists")) 
                  stop("to be used with 'comp.res' object")


        cat("Items in X:", x$Length.X, "\n")
        cat("Items in Y:", x$Length.Y, "\n\n")

	cat("No of vecX in vecY", x$XinY, "\n")
        cat("No of vecY in vecX", x$YinX, "\n\n")

 	cat("Intersection of sets is", length(x$intersect), "\n")
        cat("Difference in sets is", length(x$Set.Diff), "\n")
 

      }


