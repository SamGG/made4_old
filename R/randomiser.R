"randomiser" <-
function(ntrain=77, ntest=19){
	#sampling to create random training and test data, to test for training,test set bias
        # input is two integers, which are the no of arrys in the training and test dataset
        # It returns two vector, which can be used to index a new training and test dataset
  
	ind<-c(1:sum(ntrain+ntest))
	newind<-sample(ind)
	n<-length(newind)
	newtrain<-newind[1:ntrain]
	newtest<-newind[(ntrain+1):n]
	return(list(train=newtrain,test=newtest))
	}
