overview<- function(dataset, labels = NULL, title = "", classvec = NULL, hc = TRUE, boxplot = TRUE, hist = TRUE, returnTree=FALSE) 
{

	# Need to update this in made4. Then this will be removed from here and just source the Bioc one.
    cols = NULL
    if (sum(boxplot, hist, hc)==3) layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
    if (sum(boxplot, hist, hc)==2) layout(matrix(c(1, 1, 2, 2), 2, 2, byrow = TRUE))
    
    classvec=as.factor(classvec)
    if (is.null(labels)) 
        labels = colnames(dataset)
    if (!is.null(labels)) 
        labels = as.character(labels)
    if (!is.null(classvec)) {
        cols = getcol(as.numeric(classvec))
        layout(matrix(c(1, 1, 2, 2, 3, 4), 3, 2, byrow = TRUE), 
            heights = c(1.5, 0.1, 2))
    }
    distEisen <- function(x, use = "pairwise.complete.obs") {
        co.x <- cor(x, use = use)
        dist.co.x <- 1 - co.x
        return(as.dist(dist.co.x))
    }
    colhc <- function(hc, classvec) {
        margins = par()$mar
        par(mar = c(0.5, margins[1], 0, margins[2]))
        nc = length(as.character(classvec))
        colInd = hc$order
        image(cbind(1:nc), col = cols[colInd], axes = FALSE)
        par(mar = margins)
    }
    if (hc) {
         if (!inherits(dataset, "matrix")) {
            if (!inherits(dataset, "AffyBatch")) {
              dataset <- array2ade4(dataset, trans = FALSE)
              }
            if (inherits(dataset, "AffyBatch")) {
               dataset = exprs(dataset)
               }
            }
        hc = hclust(distEisen(dataset), method = "ave")
        plot(hc, hang = -1, labels = labels, main = paste("Histogram", 
            title, sep = " "), sub="", xlab="")
        if (!missing(classvec)) 
            colhc(hc, classvec)
    }


    if (boxplot) {
        if (!inherits(dataset, "AffyBatch")) {
            dataset <- array2ade4(dataset, trans = FALSE)
        }
        boxplot(dataset, main = paste("boxplot", title, sep = " "), 
            names = labels, par(las = 2), col = cols)
    }
    if (hist) {
        if (!inherits(dataset, "AffyBatch")) {
            dataset <- array2ade4(dataset, trans = FALSE)
            dataset = as.matrix(dataset)
        }
        hist(dataset, xlab = "", main = paste("Histogram", title, 
            sep = " "))
    }
    
    if (returnTree) return(hc)
}
