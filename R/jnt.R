jnt <-
function(PR, prsep=", ") { 
vec <- vector()
for(i in 1:length(PR)) { vec <- append(vec,strsplit(PR[i],prsep)[[1]]) }; rm(i)
vec <- levels(factor(vec))
if(length(vec)==1) jpr <- vec
if(length(vec)>1) jpr <- paste(vec[1],vec[2],sep=prsep)
if(length(vec)>2) { for(i in 3:length(vec)) jpr <- paste(jpr,vec[i],sep=prsep); rm(i) }
	return(jpr) 
}
