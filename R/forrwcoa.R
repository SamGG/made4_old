"forrwcoa" <-
function(df, rowweights = rep(1/nrow(df),nrow(df))) {
        # For row weighted correspondence analysis 
	# where df is a data frame rowweights a weighting row vector (by default the uniform weighting)
	# The function returns a data frame which COA computes the required weighting.
	# Use 'dudi.coa' on this result. Written by D. Chessel & A.B. Dufour June 2003 for A. Culhane
	# example
	# data(atlas)
	# coa1=dudi.coa(atlas$birds,scannf=F)
	# coa2=dudi.coa(forrwcoa(atlas$birds),scannf=F)

	if (!is.data.frame(df))
	 	stop("data.frame expected")
	if (any(rowweights <0)) stop ("negative entries in row weighting")
	if (length(rowweights)!= nrow(df)) stop ("non convenient length for 'rowweights'")
      	lig <- nrow(df)
     	col <- ncol(df)
      	if (any(df < 0))
	 	stop("negative entries in table")
      	if ((N <- sum(df)) == 0)
	 	stop("all frequencies are zero")
      	df <- df/N
      	row.w <- apply(df, 1, sum)
      	df <- df/row.w
      	df <- df*rowweights
      	return(df)
      	}
