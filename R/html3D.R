html3D<-function (df, classvec = NULL, writepdb = FALSE, filenamebase = "output", 
    writehtml = FALSE, title = NULL, scaled = TRUE, xyz.axes = c(1:3), 
    ...) 
{
    if (ncol(df) < 3) 
        stop("need 3 columns to create 3D plot")
    df <- df[, xyz.axes]
    btt <- function(x) {
        for (i in c(1e-05, 1e-04, 0.001, 0.01, 0.1, 1, 10, 100, 
            1000, 10000, 1e+05)) {
            if (min(x) >= min(i * (-1)) && max(x) <= max(i)) 
                return(x * 10/i)
        }
    }
    if (!is.null(classvec)) classvec=checkfac(classvec)
    addaxes <- function() {
        XX = matrix(c(c(2, 4, -2, -4), c(0, 0, 0, 0), c(0, 0, 
            0, 0)), ncol = 3, dimnames = list(NULL, LETTERS[24:26]))
        axescord = rbind(c(0, 0, 0), XX, XX[, c(3, 1, 2)], XX[, 
            c(3, 2, 1)])
        axescord = cbind(Ind = 1:nrow(axescord), axescord)
        axesvec = as.factor(c("AXA", unlist(lapply(c("XXX", "YYY", 
            "ZZZ"), function(x) {
            rep(c("AXA", x), 2)
        }))))
        formataxes <- function(x) {
            return(sprintf(paste("ATOM  %5.0f  %2s  %3s X %3.0f     %7.3f %7.3f %7.3f  1.00 %5.2f", 
                sep = ""), x[1], c("CA", "X", "Y", "Z")[x[5]], 
                substr(levels(axesvec)[x[5]], 1, 3), x[1], x[2], 
                x[3], x[4], x[4]))
        }
        axescord = cbind(axescord, as.numeric(axesvec))
        axes = apply(axescord, 1, formataxes)
        axes[14] = "TER"
        return(axes)
    }
    formatline <- function(x, classvec, ...) {
        if (!is.null(classvec)) {
            return(sprintf(paste("ATOM  %5.0f  ID  %3s %1s %3.0f     %7.3f %7.3f %7.3f  1.00 %5.2f", 
                sep = ""), x[1], substr(levels(classvec)[x[5]], 
                1, 3), LETTERS[x[5]], x[1], x[2], x[3], x[4], 
                x[4]))
        }
        if (is.null(classvec)) {
            return(sprintf(paste("ATOM  %5.0f  ID  SAM %5.0f     %7.3f %7.3f %7.3f  1.00 %5.2f", 
                sep = ""), x[1], x[1], x[2], x[3], x[4], x[4]))
        }
    }
    bels <- df
    if (scaled) {
        bels <- btt(bels)
    }
    rn <- row.names(bels)
    bels <- cbind(Ind = 1:nrow(bels), bels)
    if (!is.null(classvec)) 
        bels = cbind(bels, vec = as.numeric(classvec))
    pdb <- c(addaxes(), apply(bels, 1, formatline, classvec = classvec))
    if (writehtml) {
        pdbfilename = paste(filenamebase, ".pdb", sep = "")
        write(pdb, pdbfilename)
        htmlfilename = paste(filenamebase, ".html", sep = "")
        chime3D(pdbfilename, classvec = classvec, title = title, 
            filename = htmlfilename, ...)
    }
    if (writepdb) {
        pdbfilename = paste(filenamebase, ".pdb", sep = "")
        write(pdb, pdbfilename)
    }
    return(as.matrix(pdb))
}