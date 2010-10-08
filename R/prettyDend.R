prettyDend<- function (dataset, labels = NULL, title = "", classvec = NULL,
    covars = 1, returnTree = FALSE, getPalette=getcol, ...)
{
    cols = NULL

    layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
    if (is.null(labels))
        labels = colnames(dataset)
    if (!is.null(labels))
        labels = as.character(labels)
    if (!is.null(classvec)) {
        nc = length(covars)
        if (nc > 1)
            classvec = as.data.frame(classvec[, covars])
        layout(matrix(1:c(nc + 1), nc + 1, 1, byrow = TRUE),
            heights = c(1.5, rep(0.1, nc)))
    }
    distEisen <- function(x, useopt = "pairwise.complete.obs") {
        co.x <- cor(x, use = useopt)
        dist.co.x <- 1 - co.x
        return(as.dist(dist.co.x))
    }
    colhc <- function(hc, classvec) {
        margins = par()$mar
        classvec = as.character(classvec)
        classvec[is.na(classvec)] = " "
        classvec = as.factor(classvec)
        groups = as.character(levels(classvec))
        groups = groups[order(groups, decreasing = TRUE)]
        cols =  getPalette(length(groups))
        if (length(groups)==2)  cols = c("black", "grey60")
       # if (length(groups)==3)  cols = c("black", "grey60", "yellow")
        fac.col = cbind(groups, cols)
        fac.col[fac.col[, 1] == " ", 2] = "white"
        cols = unlist(sapply(classvec, function(x) fac.col[grep(x,
            fac.col[, 1]), 2]))

       # for margins (mar() or outter marigins (oma), bottom, left, top, right
        par(mar = c(0.5, margins[1], 0, margins[2]))
        #par(oma=c(0,0.1,0,0) )
        nc = length(as.character(classvec))
        colInd = hc$order
        image(cbind(1:nc), col = cols[colInd], axes = FALSE)

        par(mar = margins)
        nr = nrow(fac.col)

        print(fac.col)
        #par(xpd=TRUE)

        for (i in 1:nr) mtext(fac.col[i,1], side =2, cex=0.7, col=fac.col[i,2], las=2, padj=i-2, adj=1)

##        legend(fac.col[(nr:1), 1], text.col = fac.col[(nr:1),2], bg = "grey2", x = 0, y = 0, horiz = TRUE)
    }
    if (!inherits(dataset, "AffyBatch")) {
        dataset <- array2ade4(dataset, trans = FALSE)
        hc = hclust(distEisen(dataset), method = "ave")
        plot(hc, hang = -1, labels = labels, xlab = "", sub = "",
            main = title)
        if (!missing(classvec)) {
            if (!is.data.frame(classvec))
                colhc(hc, classvec)
            if (is.data.frame(classvec)) {
                apply(classvec, 2, function(x) {
                  colhc(hc, x)
                })
            }
            if (returnTree)
                return(hc$order)
        }
    }
}
