"checkfac" <-
function(classvec){
        if (!is.factor(classvec))classvec<-factor(classvec)
        if (!is.factor(classvec))
		stop("Problems reading factor classvec")
        return(classvec)
        }
