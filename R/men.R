men <-
function(y) {
	mn<-vector()
	if(length(y)>0) for(i in 1:length(y)) if(strsplit(y[i],", ")[[1]][1]<strsplit(y[i],", ")[[1]][2]) mn<-append(mn, y[i]) ;rm(i)
	return(mn)
}
