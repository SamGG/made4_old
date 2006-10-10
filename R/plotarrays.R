plotarrays<-function(coord, axis1 = 1, axis2 = 2, arraylabels = NULL, classvec=NULL,  graph = "auto", ...){

    if (!is.null(classvec)) {
           classvec = checkfac(classvec)
           arraycol = getcol(1:length(levels(classvec)))
        }


    if (inherits(coord, "cia")) {
      if (is.null(arraylabels)) arraylabels= row.names(coord$coinertia$mX)
      s.match.col(coord$coinertia$mX, coord$coinertia$mY, xax = axis1, yax = axis2, classvec=classvec, label=arraylabels, ...)
      }
    if (inherits(coord, "coinertia")) { 
         if (is.null(arraylabels)) arraylabels= row.names(coord$mX)
         s.match.col(coord$mX, coord$mY, xax = axis1, yax = axis2, classvec=classvec, label=arraylabels, ...)
         }
    if (!inherits(coord, "cia")) {
        if (!inherits(coord, "data.frame")) {
          if (inherits(coord, "bga")){
              if (is.null(classvec)) classvec= coord$fac
              coord = coord$bet$ls
              arraycol = getcol(1:length(levels(classvec)))
              }
              
           if (inherits(coord, "between"))  coord = coord$ls
           if (inherits(coord, "ord")) {
              if (is.null(classvec)) {if (!is.null(coord$fac)) {
                        classvec= coord$fac
                        arraycol = getcol(1:length(levels(classvec)))}
               }
              coord = coord$ord$co
              }
           if  (inherits(coord, "dudi"))  coord = coord$co
        }

    #print(classvec)
        colpts=NULL
        
        switch(graph, "auto",
      "s.var" = {if (!is.null(classvec)) colpts = as.numeric(classvec)}   ,
      "s.groups" = {if (is.null(classvec)) print("Need to specify groups")} ,
      "s.match.col"= {if (!inherits(coord, "cia") | !inherits(coord, "coinertia"))
          print("Type of class cia or coinertia expected") },
      stop(paste("The type of plot",
        graph, "was not recognised. Permitted are: auto, s.var, s.groups, s.match.col",
        sep = " ")))

     if (is.null(arraylabels)) arraylabels= row.names(coord)

     if (!is.null(classvec)&&is.null(colpts)) {
          s.groups(coord, classvec=classvec, col=arraycol,  xax = axis1, yax = axis2,  ... )
        }

     if (!is.null(colpts)){
          s.var(coord, colpoints = colpts, label=arraylabels,  xax = axis1, yax = axis2, ...)
          }
          
     if (is.null(classvec)) {
        s.var(coord,   xax = axis1, yax = axis2, label=arraylabels, ...)
      }

    }
    }
