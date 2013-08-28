dhc <-
function(x) {
Lt<-list()
for(i in 1:length(x)) Lt[[length(Lt)+1]]<-strsplit(levels(factor(x[i])),", ")[[1]]
	return(Lt[[1]]) 
}
