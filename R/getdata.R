"getdata" <-
function(arraydata) {
        # Edited from vsn function getIntensityMatrix() from W. Huber
        # To run ade4 the arraydata needs to be in a data.frame format.
        y = switch(class(arraydata),
                    matrix    = { if (!is.numeric(arraydata))
                                     stop("Arraydata was found to be a matrix, but is not numeric.")
                                   data.frame(arraydata)
                                 },
                   data.frame = {  if (!all(sapply(arraydata, is.numeric)))
                                     stop("Arraydata was found to be a data.frame, but contains non-numeric columns.")
                                   intensities
                                 },
                  ExpressionSet = { if (require(affy, quiet = TRUE)) data.frame(exprs(arraydata))
                                },
		   exprSet = { if (require(affy, quiet = TRUE))   data.frame(exprs(arraydata))
                                },

                  marrayRaw = {
                               if (require(affy, quiet = TRUE)) {
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
                   stop(paste("Arraydata has class ", class(arraydata), ". Permitted are: matrix, data.frame, exprSet, marrayRaw", sep=""))
        )  ## end of switch statement
  
        return(y)
        }
