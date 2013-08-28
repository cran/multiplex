orden <-
function(Pr, prsep=", ") {
#
for(i in 1:length(Pr)) {
	if(isTRUE(as.numeric(strsplit(Pr[i],prsep)[[1]][1]) > as.numeric(strsplit(Pr[i],prsep)[[1]][2]))==TRUE) Pr[i] <- swp(Pr[i])
}; rm(i)
	return(Pr) }
