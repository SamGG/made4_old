"dudi.rwcoa" <-
function(df,  rowweights = rep(1/nrow(df),nrow(df)) , ...) {
        # Row weighted COA, calls forrwcoa function
     	if (!is.data.frame(df))
	 	stop("data.frame expected")
        
    	rwcoa<-dudi.coa(forrwcoa(df, rowweights=rowweights), ...)
        class(rwcoa)<-c("rwcoa", class(rwcoa))              
    	return(rwcoa)
  	}
