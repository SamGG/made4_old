"html3D" <-
function(df, classvec=NULL, writepdb=FALSE, filenamebase="output", writehtml=FALSE,
                       title=NULL, scaled=TRUE, xyz.axes=c(1:3), ...){

      #produces a "fake" rasmol file, so 3d coordinates can be view using rasmol
      #Thanks to Prof. Willie Taylor for the awk command
  
      #Generally rasmol expects coordinates in the angstrom range, multiple coordinates by factor
      #to get into rasmol realistic range

       if (ncol(df)<3) stop("need 3 columns to create 3D plot")

       df<-df[,xyz.axes]

       btt<- function(x){
           for (i in  c(1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1, 1e1, 1e2, 1e3, 1e4,1e5)){
             if(min(x)>=min(i*(-1))&& max(x)<=max(i)) return(x*10/i)
             }
         }

      # A R version of the famous awk lines with thanks to Prof. Willie Taylor, London, UK
      formatline<-function(x){
        if(!is.null(classvec)){
  
             return(sprintf(paste("ATOM  %5.0f  ID  %3s %1s %3.0f     %7.3f %7.3f %7.3f  1.00 %5.2f", sep=""),
                            x[1], substr(levels(classvec)[x[5]],1,3), LETTERS[x[5]],x[1], x[2], x[3], x[4], x[4]))
             }
        if(is.null(classvec)){
             return(sprintf(paste("ATOM  %5.0f  ID  SAM %5.0f     %7.3f %7.3f %7.3f  1.00 %5.2f", sep=""),
                            x[1],x[1], x[2], x[3], x[4], x[4]))
             }
       }

       bels<-df
       if (scaled) {bels<-btt(bels)}

       rn<-row.names(bels)
       bels<-cbind(Ind=1:nrow(bels), bels)
       if(!is.null(classvec)) bels=cbind(bels,vec=as.numeric(classvec))

       #print(bels)
       pdb<-apply(bels, 1, formatline)
       #print(pdb)
       # Will call chime3D, which produces a html page, in which graph can be rotates using chime (Win/MacOS specific)
       if(writehtml) {
         pdbfilename=paste(filenamebase, ".pdb", sep="")
         write(pdb, pdbfilename)
         htmlfilename=paste(filenamebase, ".html", sep="")
         chime3D(pdbfilename, classvec=classvec, title=title, filename=htmlfilename,...)
       }

       # Write the "fake" pdb file
       if(writepdb) {
         pdbfilename=paste(filenamebase, ".pdb", sep="")
         write(pdb, pdbfilename)
       }     
       return(as.matrix(pdb))
	}





"chime3D" <-
function(pdbfilename, classvec=NULL, title=NULL, filename="output.html", point.size=40, ...){

    # Produces a html file, of a 3D graph which can be rotated using the FREEWARE chime (win, MacOS)
    # Will colour samples by classvec if given one, and will produce chime script to
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
    cat("<TR> <TD WIDTH=200 CELLPADDING=5 VALIGN=TOP>\n",file=outfile, sep="\n")
   

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
	      " script='select *; colour atoms grey; select *", letts[i],"; colour atoms ",
              graphcols[i], "'></li>\n", file=outfile, sep="")
         }
        # restore orginal colours
        cat("\n<p><b>Restore</b><li>Original Colours\n",
            "<embed type='application/x-spt' \n width=12 height=12 button=push target='graph' \n script='",
            file=outfile, sep="")
        
        for (i in c(1:nclass)){ cat(" select *",letts[i], "; colour atoms ", graphcols[i], ";", file=outfile, sep="")}
        
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
    cat(" script= 'set axes on; select off; connect;set ambient ", point.size, ";  rotate x 180;  \n select *;  spacefill ", point.size, ";" ,file=outfile, sep=" ")


    if(!is.null(classvec)){
        nclass<-length(levels(classvec))
        letts<-LETTERS[1:nclass]
        if (nclass<=9) (palette = "colours1")
        if (nclass >9) (palette = rainbow(nclass))

        graphcols<-getcol(nc=c(1:nclass), palette = palette)
        for (i in c(1:nclass)){
          cat("\n select *",letts[i], "; colour atoms ", graphcols[i], ";", file=outfile, sep="")
           }
        cat("'\n", file=outfile, sep="")
      }
    if(is.null(classvec)) cat("colour atoms red'\n",  file=outfile, sep=" ")
    
    cat("\nbgcolor = black"," display 3d =true", "height = 600 width =600>\n</TD>\n",file=outfile, sep="\n")
    
    cat("</tr>\n</table>\n</html>",file=outfile, sep="\n")
    close(outfile)
}




