.First.lib <- function(libname, pkgname) {

   # Only works for windows now
    if(interactive() && .Platform$OS.type == "windows" &&
             .Platform.GUI == "Rgui"){
        addVigs2WinMenu("ade4")
    }
}

