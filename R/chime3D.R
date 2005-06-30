chime3D<-function (pdbfilename, classvec = NULL, title = NULL, filename = "output.html", 
    point.size = 40, cols=NULL, ...) 
{
    outfile <- file(filename, "w")
    if(!is.null(classvec)) classvec=checkfac(classvec)
    if (is.null(title)) 
        title = filename
    cat("<html>\n <head><title>", title, "</title></head>\n", 
        "<body bgcolor=\"#FFFFFF\">\n", "<h1 align=\"center\">", 
        title, "</h1>\n", file = outfile, sep = " ")
    cat("<TABLE CELLPADDING=0 CELLSPACING=6 BORDER=0 WIDTH=850 align=center>\n", 
        file = outfile, sep = "\n")
    cat("<i><U>NOTE:</U> If you can't see the figure, try refreshing your browser otherwise download a FREE copy of", 
        "<a href='http://www.mdlchime.com/' target='_blank'>Chime</a></i>.", 
        "<br><b>Rotate</b> using your <font color ='green'>left</font>", 
        "mouse button. <b>Zoom</b> by pressing the <font color=\"green\">shift key </font> while using your <font color='green'>", 
        "left</font> mouse button. </i>\n", file = outfile, sep = "\n")
    cat("<TR> <TD COLSPAN=4> <HR ALIGN=CENTER WIDTH=100% COLOR=GREEN NOSHADE SIZE=5></TD></TR>\n", 
        file = outfile, sep = "\n")
    cat("<TR> <TD WIDTH=300 CELLPADDING=5 VALIGN=TOP>\n", file = outfile, 
        sep = "\n")
    cat("\n<p><b>Spin Graph</b>\n<li>Start continuous spinning", 
        "<embed type='application/x-spt' width=12 height=12 button=push \ntarget='graph' script='spin on'></li>\n", 
        "<li> Stop continuous spinning ", "<embed type='application/x-spt' width=12 height=12 button=push \ntarget='graph' script='spin off'></li></P>\n", 
        file = outfile, sep = "\n")
    cat("\n<p><b>Restore</b><li>original rotation and zoom", 
        "<embed type='application/x-spt' width=12 height=12 button=push \ntarget='graph'", 
        "script='reset; rotate x 180'></li></p\n", file = outfile, 
        sep = "\n")
    doclasscol <- function(classvec) {
        cat("\n<p><b>Colour classses</b>\n", file = outfile, 
            sep = "/n")
        nclass <- length(levels(classvec))
        letts <- LETTERS[1:nclass]
        graphcols  = cols
        if (is.null(graphcols)) graphcols <- getcol(nc = c(1:nclass), palette = "colours1")
        for (i in c(1:nclass)) {
            cat("\n<li>", levels(classvec)[i], " samples (", 
                graphcols[i], ")\n", "<embed type='application/x-spt' \n width=12 height=12 button=push target='graph'\n", 
                " script='select *; colour atoms grey; select *", 
                letts[i], "; colour atoms ", graphcols[i], "'></li>\n", 
                file = outfile, sep = "")
        }
        cat("\n<p><b>Restore</b><li>Original Colours\n", "<embed type='application/x-spt' \n width=12 height=12 button=push target='graph' \n script='", 
            file = outfile, sep = "")
        for (i in c(1:nclass)) {
            cat(" select *", letts[i], "; colour atoms ", graphcols[i], 
                ";\n", file = outfile, sep = "")
        }
        cat("'></li></p>\n", file = outfile, sep = "")
        cat("</p>", file = outfile, sep = "/n")
    }
    if (!is.null(classvec)) 
        doclasscol(classvec)
    cat("</TD>\n\n<TD WIDTH=5 BGCOLOR=GREEN><BR></TD> <TD WIDTH=10><BR></TD>\n", 
        file = outfile, sep = "\n")
    cat("<TD valign=TOP WIDTH=450 BGCOLOR=white>\n", file = outfile, 
        sep = " ")
    cat("<embed  src =", pdbfilename, "name = 'graph'\n", file = outfile, 
        sep = " ")
    cat(" script= 'select *; spacefill ", point.size, "; set ambient ", 
        point.size, ";\nrotate x 180;select *X; connect; colour bonds black; spacefill off;\nselect none;\n select XXX3;label F1;\n select YYY7; label F2;\n select ZZZ11; label F3;\n colour labels black;\nselect none;", 
        file = outfile, sep = " ")
    if (!is.null(classvec)) {
        nclass <- length(levels(classvec))
        letts <- LETTERS[1:nclass]
        graphcols  = cols
        if (is.null(graphcols)) graphcols <- getcol(nc = c(1:nclass), palette = "colours1")
   
        for (i in c(1:nclass)) {
            cat("select *", letts[i], "; colour atoms ", graphcols[i], 
                ";\n", file = outfile, sep = "")
        }
        cat("'\n", file = outfile, sep = "")
    }
    if (is.null(classvec)) 
        cat("select SAM; colour atoms red'\n", file = outfile, 
            sep = " ")
    cat("\nbgcolor = white", "display 3d =true", "height = 600 width =600>\n</TD>\n", 
        file = outfile, sep = "\n")
    cat("</tr>\n</table>\n</html>", file = outfile, sep = "\n")
    close(outfile)
}

