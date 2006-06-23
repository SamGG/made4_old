"overview"<-
function(dataset, labels=NULL, title="", classvec=NULL, ...){

        cols=NULL
        layout(matrix(c(1,1, 2,3), 2, 2, byrow = TRUE))
        if (is.null(labels)) labels=colnames(dataset)
        labels=as.character(labels)
        if (!is.null(classvec))  {
           cols= getcol(as.numeric(classvec))
           #print(cols)
           layout(matrix(c(1,1,2,2, 3,4), 3, 2, byrow = TRUE), heights=c(1.5,0.1,2))
            }
        distEisen <- function(x, use="pairwise.complete.obs") {
          co.x <- cor(x, use=use)
          dist.co.x <- 1-co.x
          return(as.dist(dist.co.x)) 
          }
        
         colhc <- function(hc, classvec) {
          margins= par()$mar
         # print(margins)
          par(mar = c(0.5, margins[1], 0, margins[2]))
          nc= length(as.character(classvec))
          colInd = hc$order
          image(cbind(1:nc),  col = cols[colInd], axes = FALSE)
           par(mar = margins)
          }

        if(!inherits(dataset, "AffyBatch")) {
          dataset<-array2ade4(dataset, trans=FALSE)                
          hc=hclust(distEisen(dataset), method="ave")
          plot(hc, hang=-1, labels=labels)  # cor dist and average linkage

          if (!missing(classvec)) colhc(hc, classvec)
          boxplot(dataset, main=paste("boxplot", title, sep=" "), names=labels,par(las=2), col=cols)
          hist(as.matrix(dataset),xlab="", main=paste("Histogram", title, sep=" "))
        }

        if(inherits(dataset, "AffyBatch")) {
           # Affybatch class dealt with differently, as it has its own boxplot and hist 
           hc= clust(distEisen(exprs(dataset)), method="ave")
          plot(hc, hang=-1, labels=labels)  # cor dist and average linkage
              if (!missing(classvec)) colhc(hc, classvec)
          boxplot(dataset, main=paste("boxplot", title, sep=" "),  names=labels, par(las=2), col=cols)
          hist(dataset)
        }

     


        }





