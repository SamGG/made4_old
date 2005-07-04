getcol <- function (nc = c(1:3), palette = NULL, test = FALSE) {
       # Aedin's colour palette... My fav colours for plotting
       # If run with test=T,get plot of all colours available

    if (!is.numeric(nc)) 
        stop("enter the range of colors eg c(1,3,4) or c(1:4)")
    if (length(nc) == 1) {
        if (nc > 1) 
            nc = c(1:nc)
    }
   if (length(nc) > 500) stop(paste("You requested", length(nc), "colours. 
        This seems a lot of colours. Please select fewer class colours"))
    colours1 = c("red", "blue", "green", "yellow", "magenta", 
        "cyan", "black", "grey", "brown", "orange", "violet", "purple")
    colours2 = c("#E41A1C", "#0000FF", "#00FF00", "#FFA500", 
        "#E7298A", "#000080", "#00FFFF", "#008000", "#999999", 
        "#800080", "#008080", "#000000", "#FF4500", "#FFFF00", 
        "#4DAF4A", "#800000", "#A65628", "#F781BF", "#1B9E77", 
        "#808000", "#377EB8")

    if (!is.null(palette)){
           palette=switch(palette, colours1 = colours1, colours2 = colours2)
           }

    if (is.null(palette)) {
        palette= colours1
        if (length(nc) > length(colours1)) palette = colours2
        }

    if (length(nc) > length(palette)) {
        print("More than 21 colours selected, some colours maybe replicated")  
        n = length(nc)
          big.palette = palette
        while (n > 0) {
            big.palette = c(big.palette, palette)
            n = n - length(palette)
            }
        palette=big.palette
        }
    outcol = palette[nc]
    if (test) {
        par(mfrow = c(3, 1))
        image(1:length(colours1), 1, as.matrix(1:length(colours1)), 
            col = colours1, main = "9 colours in default palette: colours1", 
            xlab = "", ylab = "", yaxt = "n")
        image(1:length(colours2), 1, as.matrix(1:length(colours2)), 
            col = colours2, main = "21 colours in default palette: colours2", 
            xlab = "", ylab = "", yaxt = "n")
        image(1:length(outcol), 1, as.matrix(1:length(outcol)), 
            col = outcol, main = paste(length(outcol), "colours in selected palette", 
                sep = " "), xlab = "", ylab = "", yaxt = "n")
    }
    return(outcol)
}

