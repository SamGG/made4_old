"bga.suppl" <-
function(dataset, supdata, classvec, supvec=NULL, suponly=FALSE, type="coa", ...){
        # Runs bga and suppl. Calculates assignments of test samples. Requires test and training dataset
	data.bga<-bga(dataset,classvec, type=type)
        suppl.res<-suppl(data.bga, supdata, supvec,...)
        if(!suponly) {
           data.bga$suppl<-suppl.res
           out<-data.bga }
        if(suponly) out=suppl.res
        return(out)
       }
