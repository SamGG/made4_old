"html3D" <-
function (df, classvec = NULL, writepdb = FALSE, filenamebase = "output",
    writehtml = FALSE, title = NULL, scaled = TRUE, xyz.axes = c(1:3),
    ...)
{

      # produces a "fake" rasmol file, so 3d coordinates 
      # can be view using rasmol or other "molecular" viewers
      # Thanks to Prof. Willie Taylor for the awk command


    if (ncol(df) < 3)
        stop("need 3 columns to create 3D plot")
    df <- df[, xyz.axes]
    btt <- function(x) {
      # Generally rasmol expects coordinates in the angstrom range, 
      # multiple coordinates by factor
      # to get into rasmol realistic range

        for (i in c(1e-05, 1e-04, 0.001, 0.01, 0.1, 1, 10, 100,
            1000, 10000, 1e+05)) {
            if (min(x) >= min(i * (-1)) && max(x) <= max(i))
                return(x * 10/i)
        }
    }
    addaxes <-function() {
      XX = matrix(c(c(2,4,-2,-4), c(0,0,0,0), c(0,0,0,0)), ncol=3, dimnames=list(NULL, LETTERS[24:26]))
      axescord =  rbind(c(0,0,0),XX, XX[,c(3,1,2)],XX[,c(3,2,1)])
      axescord  = cbind(Ind = 1:nrow(axescord), axescord)
      axesvec= as.factor(c("AXA",unlist(lapply(c("XXX", "YYY", "ZZZ"), function(x){rep(c("AXA",x),2)}))))

          formataxes<-function(x) {
      # A R version of the famous awk lines with thanks to 
      # Prof. Willie Taylor, London, UK
                return(sprintf(paste("ATOM  %5.0f  %2s  %3s X %3.0f     %7.3f %7.3f %7.3f  1.00 %5.2f",
                sep = ""), x[1], c("CA", "X", "Y", "Z")[x[5]], substr(levels(axesvec)[x[5]], 1, 3),  x[1], x[2], x[3], x[4], x[4]))
                }
      #axesno <- axesvec
      #levels(axesno)<-c(21,23:26)
      #axescord  = cbind(axescord, as.numeric(as.character(axesno)))
      #axescord  = cbind(axescord, rep(26, nrow(axescord)))
      axescord  = cbind(axescord, as.numeric(axesvec))
      #print(axescord)
      axes= apply(axescord, 1, formataxes)
      axes[14]="TER"
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
    pdb <- c(addaxes(), apply(bels, 1, formatline, classvec=classvec))
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





"chime3D" <-
function(pdbfilename, classvec=NULL, title=NULL, filename="output.html", point.size=40, ...){

    # Produces a html file, of a 3D graph which can be rotated 
    # using the FREEWARE chime (win, MacOS)
    # Will colour samples by classvec if given one, and will 
    # produce chime script to
    # highlight groups, spin on/off, and include button for restore
    # for example see http://bioinf.ucd.ie/research/microarrays/
 
    outfile <- file(filename, "w")
    if(is.null(title))title=filename
    cat("<html>\n <head><title>", title, "</title></head>\n", 
        "<body bgcolor=\"#FFFFFF\">\n", 
        "<h1 align=\"center\">", title, "</h1>\n", file = outfile, sep = " ")

    cat("<TABLE CELLPADDING=0 CELLSPACING=6 BORDER=0 WIDTH=850 align=center>\n",file=outfile, sep="\n")
    # Chime details
    cat("<i><U>NOTE:</U> If you can't see the figure, try refreshing your browser otherwise download a FREE copy of", 
      "<a href='http://www.mdlchime.com/' target='_blank'>Chime</a></i>.", "<br><b>Rotate</b> using your <font color ='green'>left</font>",
       "mouse button. <b>Zoom</b> by pressing the <font color=\"green\">shift key </font> while using your <font color='green'>",
        "left</font> mouse button. </i>\n", file=outfile, sep="\n")

    
    # Green hr 
    cat("<TR> <TD COLSPAN=4> <HR ALIGN=CENTER WIDTH=100% COLOR=GREEN NOSHADE SIZE=5></TD></TR>\n",file=outfile, sep="\n")
    
    # Column 1 of table - chime options
    cat("<TR> <TD WIDTH=300 CELLPADDING=5 VALIGN=TOP>\n",file=outfile, sep="\n")
   

    # Spinning on and off
    cat("\n<p><b>Spin Graph</b>\n<li>Start continuous spinning",
        "<embed type='application/x-spt' width=12 height=12 button=push \ntarget='graph' script='spin on'></li>\n",
	"<li> Stop continuous spinning ",
        "<embed type='application/x-spt' width=12 height=12 button=push \ntarget='graph' script='spin off'></li></P>\n" ,
        file=outfile, sep="\n")
    
    # Restore original roation and zoom
    cat("\n<p><b>Restore</b><li>original rotation and zoom",
        "<embed type='application/x-spt' width=12 height=12 button=push \ntarget='graph'", 
		"script='reset; rotate x 180'></li></p\n", file=outfile, sep="\n")

    # Colours for classes,
    doclasscol<-function(classvec)  {
        cat("\n<p><b>Colour classses</b>\n", file=outfile, sep="/n")
        nclass<-length(levels(classvec))
        if (nclass<=9) (palette = "colours1")
        if (nclass >9) (palette = rainbow(nclass))
        letts<-LETTERS[1:nclass]
        graphcols<-getcol(nc=c(1:nclass), palette = palette)
        
        # Colour each class option
        for (i in c(1:nclass)){
          cat("\n<li>", levels(classvec)[i], " samples (", graphcols[i], ")\n",
              "<embed type='application/x-spt' \n width=12 height=12 button=push target='graph'\n", 
	            " script='select *; colour atoms grey; select *", letts[i],"; colour atoms ", graphcols[i], "'></li>\n", file=outfile, sep="")
         }
        # restore orginal colours
        cat("\n<p><b>Restore</b><li>Original Colours\n",
            "<embed type='application/x-spt' \n width=12 height=12 button=push target='graph' \n script='",file=outfile, sep="")
        
        for (i in c(1:nclass)){ cat(" select *",letts[i], "; colour atoms ", graphcols[i], ";\n", file=outfile, sep="")}
        
        cat("'></li></p>\n", file=outfile, sep="")

        # Finish section
        cat("</p>", file=outfile, sep="/n")
     }
    
    
    if(!is.null(classvec)) doclasscol(classvec)
    
    # Table columns 2,3 (spacers)
    cat("</TD>\n\n<TD WIDTH=5 BGCOLOR=GREEN><BR></TD> <TD WIDTH=10><BR></TD>\n", file=outfile, sep="\n")

    # Table Column 4
    cat("<TD valign=TOP WIDTH=450 BGCOLOR=white>\n", file=outfile, sep=" ")
    
    # Load graph onto web site
    cat("<embed  src =", pdbfilename, "name = 'graph'\n", file=outfile, sep=" ")
    cat(" script= 'select *; spacefill ", point.size,
    "; set ambient ", point.size,
    ";\nrotate x 180;select *X; connect; colour bonds black; spacefill off;\nselect none;\n select XXX3;label F1;\n select YYY7; label F2;\n select ZZZ11; label F3;\n colour labels black;\nselect none;", file=outfile, sep=" ")


    if(!is.null(classvec)){
        nclass<-length(levels(classvec))
        letts<-LETTERS[1:nclass]
        if (nclass<=9) (palette = "colours1")
        if (nclass >9) (palette = rainbow(nclass))

        graphcols<-getcol(nc=c(1:nclass), palette = palette)
        for (i in c(1:nclass)){
          cat("select *",letts[i], "; colour atoms ", graphcols[i], ";\n", file=outfile, sep="")
           }
      cat("'\n", file=outfile, sep="")
      }
    if(is.null(classvec)) cat("select SAM; colour atoms red'\n",  file=outfile, sep=" ")

    cat("\nbgcolor = white","display 3d =true", "height = 600 width =600>\n</TD>\n",file=outfile, sep="\n")

    cat("</tr>\n</table>\n</html>",file=outfile, sep="\n")
    close(outfile)
}



"jmol3D" <-
function(df, classvec=NULL, title=NULL, jmoldir="../jmol/Jmol.js", 
         filename="output.html", point.size=40, xyz.axes =c(1:3),scaled=TRUE, ...){
   
    # Produces a html file, of a 3D graph which can be rotated 
    # using the open source software jmol
    # Will colour samples by classvec if given one, and will 
    # produce chime script to
    # highlight groups, spin on/off, and include button for restore
    # for example see http://bioinf.ucd.ie/people/aedin/jmoltest/test.html
 
   makepdb<-function(df, classvec = NULL, scaled = TRUE, xyz.axes = c(1:3)){
   if (ncol(df) < 3)
        stop("need 3 columns to create 3D plot")
    df <- df[, xyz.axes]
    btt <- function(x) {
      # Generally rasmol expects coordinates in the angstrom range, 
      # multiple coordinates by factor
      # to get into rasmol realistic range

        for (i in c(1e-05, 1e-04, 0.001, 0.01, 0.1, 1, 10, 100,
            1000, 10000, 1e+05)) {
            if (min(x) >= min(i * (-1)) && max(x) <= max(i))
                return(x * 10/i)
        }
    }
    addaxes <-function() {
      XX = matrix(c(c(2,4,-2,-4), c(0,0,0,0), c(0,0,0,0)), ncol=3, dimnames=list(NULL, LETTERS[24:26]))
      axescord =  rbind(c(0,0,0),XX, XX[,c(3,1,2)],XX[,c(3,2,1)])
      axescord  = cbind(Ind = 1:nrow(axescord), axescord)
      axesvec= as.factor(c("AXA",unlist(lapply(c("XXX", "YYY", "ZZZ"), function(x){rep(c("AXA",x),2)}))))

          formataxes<-function(x) {
      # A R version of the famous awk lines with thanks to 
      # Prof. Willie Taylor, London, UK
                return(sprintf(paste("\"ATOM  %5.0f  %2s  %3s X %3.0f     %7.3f %7.3f %7.3f  1.00 %5.2f\\n\" +\n",
                sep = ""), x[1], c("CA", "X", "Y", "Z")[x[5]], substr(levels(axesvec)[x[5]], 1, 3),  x[1], x[2], x[3], x[4], x[4]))
                }
      #axesno <- axesvec
      #levels(axesno)<-c(21,23:26)
      #axescord  = cbind(axescord, as.numeric(as.character(axesno)))
      #axescord  = cbind(axescord, rep(26, nrow(axescord)))
      axescord  = cbind(axescord, as.numeric(axesvec))
      #print(axescord)
      axes= apply(axescord, 1, formataxes)
      axes[14]= paste("\"", "TER", "\\n\" +\n", sep="")
      return(axes)
      }


    formatline <- function(x, classvec, ...) {
        if (!is.null(classvec)) {
            return(sprintf(paste("\"ATOM  %5.0f  ID  %3s %1s %3.0f     %7.3f %7.3f %7.3f  1.00 %5.2f\\n\" +\n",sep = ""), x[1], substr(levels(classvec)[x[5]],
                1, 3), LETTERS[x[5]], x[1], x[2], x[3], x[4],
                x[4]))
        }
        if (is.null(classvec)) {
            return(sprintf(paste("\"ATOM  %5.0f  ID  SAM %5.0f     %7.3f %7.3f %7.3f  1.00 %5.2f\\n\" +\n",
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
    pdb <- c(addaxes(), apply(bels, 1, formatline, classvec=classvec))
    return(pdb)
    }


    outfile <- file(filename, "w")

    if(is.null(title))title=filename

    cat("<html>\n <head><title>", title, "</title> <script src=", file = outfile, sep = " ")
    cat("\"",jmoldir,"\"", sep="", file=outfile)
    cat("></script></head>\n",   "<body bgcolor=\"#FFFFFF\">\n", 
        "<h1 align=\"center\">", title, "</h1>\n", file = outfile, sep = " ")

    cat("<TABLE CELLPADDING=0 CELLSPACING=6 BORDER=0 WIDTH=850 align=center>\n",file=outfile, sep="\n")
    # Jmol details
    cat("<i><U>NOTE:</U> If you can't see the figure, try refreshing your browser otherwise you will need to have jmol installed on your webserver.  See ", 
      "<a href='http://jmol.sourceforge.net/' target='_blank'>Jmol homepage</a></i>.", "<br><b>Rotate</b> using your <font color ='green'>left</font>",
       "mouse button. <b>Zoom</b> by pressing the <font color=\"green\">shift key </font> while using your <font color='green'>",
        "left</font> mouse button. </i>\n", file=outfile, sep="\n")

    # Green hr 
    cat("<TR> <TD COLSPAN=4> <HR ALIGN=CENTER WIDTH=100% COLOR=GREEN NOSHADE SIZE=5></TD></TR>\n",file=outfile, sep="\n")
    
    # Column 1 of table - chime options
    cat("<TR> <TD WIDTH=300 CELLPADDING=5 VALIGN=TOP>\n",file=outfile, sep="\n")

  # Table columns 2,3 (spacers)
    cat("</TD>\n\n<TD WIDTH=5 BGCOLOR=GREEN><BR></TD> <TD WIDTH=10><BR></TD>\n", file=outfile, sep="\n")

    # Table Column 4
    cat("<TD valign=TOP WIDTH=450 BGCOLOR=white>\n", file=outfile, sep=" ")


    # Load graph onto web site
    cat("<script>\n jmolInitialize(", file=outfile, sep=" ")
    cat("\"", dirname(jmoldir),"\"", file=outfile, sep="")
    cat("); // REQUIRED\n ", file=outfile, sep=" ")

    parms= "set axes on; wireframe off; color background white; select *; spacefill 40;color axes black; select *A; color red; select *B; color yellow; select *C; color blue; select *D; colour orange; set ambient 40; rotate x 180'"

    molfile = makepdb(df=df, classvec=classvec, scaled = scaled, xyz.axes = xyz.axes)
    cat("var mymol = \n", molfile, sep= " ", file=outfile)

    cat("\n ", "jmolAppletInline(600, mymol, '", parms, file=outfile, sep="")

    cat(");\njmolDebugAlert(true);\n</script>", file=outfile, sep="\n")
    cat("</tr>\n</table>\n</body>\n</html>",file=outfile, sep="\n")
    close(outfile)
    print("done")

}



