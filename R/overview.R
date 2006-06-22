"overview"<-
function(dataset, label=NULL, title="", ...){

        layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
        distEisen <- function(x, use="pairwise.complete.obs") {
          co.x <- cor(x, use=use)
          dist.co.x <- 1-co.x
          return(as.dist(dist.co.x)) 
          }
        
        if(!inherits(dataset, "AffyBatch")) {
          dataset<-array2ade4(dataset, trans=FALSE)                
          plot(hclust(distEisen(dataset), method="ave"), hang=-1, label=label)  # cor dist and average linkage
          boxplot(dataset, main=paste("boxplot", title, sep=" "), names=label)
          hist(as.matrix(dataset),xlab="", main=paste("Density", title, sep=" "))
        }

        if(inherits(dataset, "AffyBatch")) {
           # Affybatch class dealt with differently, as it has its own boxplot and hist 
           plot(hclust(distEisen(exprs(dataset)), method="ave"), hang=-1, label=label)  # cor dist and average linkage
           
          boxplot(dataset, main=paste("boxplot", title, sep=" "),  names=label)
          hist(dataset)
        }

        }


