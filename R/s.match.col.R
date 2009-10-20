"s.match.col" <-
function (df1xy, df2xy, xax = 1, yax = 2, pch = 20, cpoint = 1, 
    label = row.names(df1xy), clabel = 1, edge = TRUE, xlim = NULL, 
    ylim = NULL, grid = TRUE, addaxes = TRUE, cgrid = 1, include.origin = TRUE, 
    origin = c(0, 0), sub = "", csub = 1.25, possub = "bottomleft", 
    pixmap = NULL, contour = NULL, area = NULL, add.plot = FALSE, col=rep(1, nrow(df1xy)),  
    classvec=NULL,  ...)
{  # line width can be changed par(lwd=2)
   # edited to it accepts colours
   # edited so it include a factor (for colouring purposes)

    arrow1 <- function(x0, y0, x1, y1, len = 0.1, ang = 15, lty = 1, 
        edge, col=col) {
        d0 <- sqrt((x0 - x1)^2 + (y0 - y1)^2)
        if (d0 < 1e-07) 
            return(invisible())
        segments(x0, y0, x1, y1, lty = lty, col=col)
        h <- strheight("A", cex = par("cex"))
        if (d0 > 2 * h) {
            x0 <- x1 - h * (x1 - x0)/d0
            y0 <- y1 - h * (y1 - y0)/d0
            if (edge) 
                arrows(x0, y0, x1, y1, ang = ang, len = len, 
                  lty = 1, col=col)
        }
    }
    df1xy <- data.frame(df1xy)
    df2xy <- data.frame(df2xy)
    if (!is.data.frame(df1xy)) 
        stop("Non convenient selection for df1xy")
    if (!is.data.frame(df2xy)) 
        stop("Non convenient selection for df2xy")
    if (any(is.na(df1xy))) 
        stop("NA non implemented")
    if (any(is.na(df2xy))) 
        stop("NA non implemented")
    n <- nrow(df1xy)
    if (n != nrow(df2xy)) 
        stop("Non equal row numbers")
    opar <- par(mar = par("mar"))
    on.exit(par(opar))
    par(mar = c(0.1, 0.1, 0.1, 0.1))
    coo <- scatterutil.base(dfxy = rbind.data.frame(df1xy, df2xy), 
        xax = xax, yax = yax, xlim = xlim, ylim = ylim, grid = grid, 
        addaxes = addaxes, cgrid = cgrid, include.origin = include.origin, 
        origin = origin, sub = sub, csub = csub, possub = possub, 
        pixmap = pixmap, contour = contour, area = area, add.plot = add.plot)
  
    

    if (!is.null(classvec)) {
         classvec<-as.factor(classvec)
         #print(col)
         if (length(col)== length(levels(classvec))) col<-as.vector(factor(classvec, labels=col))
        }
  
    if(is.factor(col))col=as.vector(col)

    
    for (i in 1:n) {
        arrow1(coo$x[i], coo$y[i], coo$x[i + n], coo$y[i + n], 
            lty = 1, edge = edge, col=col[i])
    }
   if (cpoint > 0)  # Edited so accepts colours
     {
            points(coo$x[1:n], coo$y[1:n], pch = pch, cex = par("cex") * cpoint, 
                col = col[1:n])
  
        }

    if (clabel > 0) # Edited so accepts colours
      {
        a <- (coo$x[1:n] + coo$x[(n + 1):(2 * n)])/2
        b <- (coo$y[1:n] + coo$y[(n + 1):(2 * n)])/2
        scatterutil.eti(a, b, label, clabel, coul=col, ...)
    }

    box()
}
