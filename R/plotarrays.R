plotarrays<-function(coord, axes1 = 1, axes2 = 2, arraylabels = NULL, classvec=NULL,  graph = "auto", ...){

    if (!is.null(classvec)) {
           classvec = checkfac(classvec)
           arraycol = getcol(1:length(levels(classvec)))
        }
    if (inherits(coord, "cia")) {
      s.match.col(coord$coinertia$mX, coord$coinertia$mY, xax = axes1, yax = axes2, classvec=classvec)
      }
    if (inherits(coord, "coinertia")) s.match.col(coord$mX, coord$mY, xax = axes1, yax = axes2, classvec=classvec)

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

     if (!is.null(classvec)&&is.null(colpts)) {
          s.groups(coord, classvec=classvec, col=arraycol,  xax = axes1, yax = axes2,  ... )
        }

     if (!is.null(colpts)){
          s.var(coord, colpoints = colpts, label=arraylabels,  xax = axes1, yax = axes2, ...)
          }
          
     if (is.null(classvec)) {
        s.var(coord,   xax = axes1, yax = axes2, label=arraylabels, ...)
      }

    }
    }
