"getdata" <-
function(arraydata) {      
        # To run ade4 the arraydata needs to be in a data.frame format.
        y = switch(class(arraydata),
                    matrix    = { if (!is.numeric(arraydata))
                                     stop("Arraydata was found to be a matrix, but is not numeric.")
                                   data.frame(arraydata)
                                 },
                   data.frame = {  if (!all(sapply(arraydata, is.numeric)))
                                     stop("Arraydata was found to be a data.frame, but contains non-numeric columns.")
                                   arraydata
                                 },
                  ExpressionSet = {data.frame(affy::exprs(arraydata))},
                  marrayRaw = {
			          {
                                  nrslides = as.integer(ncol(arraydata@maRf))
                                  nrspots = as.integer(nrow(arraydata@maRf))
                                  tmp = matrix(NA, nrow = nrspots, ncol = 2 * nrslides)
                                  tmp[, (1:nrslides) * 2 - 1] = arraydata@maGf - arraydata@maGb
                                  tmp[, (1:nrslides) * 2] = arraydata@maRf - arraydata@maRb
                                  tmp.names = vector(mode = "character", length = 2 * nrslides)
                                  tmp.names[(1:nrslides) * 2 - 1] = paste("G",colnames(arraydata@maGf),sep="_")
                                  tmp.names[(1:nrslides) * 2] = paste("R",colnames(arraydata@maRf),sep="_")
                                  colnames(tmp) = tmp.names
                                  }
                                as.data.frame(tmp)
                               },
               RangedSummarizedExperiment= SummarizedExperiment::assay(arraydata),
		SummarizedExperiment =  SummarizedExperiment::assay(arraydata),

               stop(paste("Arraydata has class ", class(arraydata), ". Permitted are: matrix, data.frame, ExpressionSet, marrayRaw", sep=""))
        )  ## end of switch statement
  
        return(y)
}
