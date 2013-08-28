hierar <-
function(W, x, type=c("person", "relation")) {

if(isTRUE(attr(W, "class")=="Rel.Box")==FALSE) stop("\"W\" must be a \"Rel.Box\" class.")

if(isTRUE(is.null(W$lbs)==TRUE)==FALSE) {
if(isTRUE(is.numeric(x)==TRUE)==TRUE) {
	if(isTRUE(length(W$lbs)<x)==TRUE) stop("\"x\" exceeds the order of the specified network")
}
if(isTRUE(is.character(x)==TRUE)==TRUE) {
	if(isTRUE(x%in%W$lbs)==FALSE)  stop("\"x\" is not an actor in the specified network")
}
} else if(isTRUE(is.null(W$lbs)==TRUE)==TRUE) {
	if(isTRUE(dim(W$W)[1]<x)==TRUE) stop("\"x\" exceeds the order of the specified network")
}

ifelse(isTRUE(is.numeric(x)==TRUE), X<-x, X<-which(W$lbs==x) )

rele <- unique(t(W$W[X,,]))


switch(match.arg(type), 
# CREATE the Data Frames for the Ego's Person Hierarchy (with NA's)
person= {
Ph<-as.data.frame(array(0,dim=c(nrow(t(rele)),nrow(t(rele))) ))
},
# CREATE the Data Frames for the Ego's Relation Hierarchy (with NA's)
relation= {
Rh<-as.data.frame(array(0,dim=c(nrow(rele),nrow(rele))))
}
)


switch(match.arg(type), 
#### AGORA P X P PARTIAL ORDERING  (COLUMNS)
person= {
for (j in 1:nrow(t(rele))) { 
for (i in 1:nrow(t(rele))) { 
	if((as.numeric(any(rele[,i]<rele[,j]))==1 && as.numeric(any(rele[,j]<rele[,i]))==0) | as.numeric(all(rele[,i]==rele[,j]))==1) Ph[i,j] <- 1 
}
}; rm(j)
# and checking that at least one j for which Ai(j,k)!=0   (p.243 Breiger & Pattison, 1986)
for (i in 1:nrow(t(rele))) { if(sum(rele[,i])==0) Ph[i,]<-0 }; rm(i)
},
#### AGORA R X R PARTIAL ORDERING  (ROWS)
relation= {
for (j in 1:nrow(rele)) { 
for (i in 1:nrow(rele)) { 
	if((as.numeric(any(rele[i,]<rele[j,]))==1 && as.numeric(any(rele[j,]<rele[i,]))==0) | as.numeric(all(rele[i,]==rele[j,]))==1) Rh[i,j] <- 1 
#	if(as.numeric(any(rele[j,]<rele[i,]) | all(any(rele[j,]<rele[i,])))==0) Rh[i,j] <- 1 
}
}; rm(j)
}
)


switch(match.arg(type), 
# Make the (square) Data Frames of the Ego's Person Hierarchy as Matrices, and PUT Attributes on it...
person= {
ph<-as.matrix(Ph)#; rm(Ph, rele)
},
# Make the (square) Data Frames of the Ego's Relation Hierarchy as Matrices, and PUT Attributes on it! Leave the Diagonals...
relation= {
rh<-as.matrix(Rh)#; rm(Rh, rele)

}
)




switch(match.arg(type), 
## PUT Attributes on it!
person= {
	if(isTRUE(is.null(W$lbs)==FALSE)) dimnames(ph)[[1]]<-dimnames(ph)[[2]]<-W$lbs
ph <- list(ph=ph, pers=x)
##
	return(ph)
##
},
relation= {
	if(isTRUE(is.null(dimnames(W$W)[[3]])==FALSE)) attributes(rh)$dimnames[[2]]<-attributes(rh)$dimnames[[1]]<-dimnames(rele)[[1]]
#	#attributes(rh)$dimnames[[2]]<-attributes(rh)$dimnames[[1]]<-attr(W$W,'dimnames')[[3]]
##
#ifelse(isTRUE(is.numeric(x)==TRUE), rh<-list(rh=rh, rel=attr(W$W,"dimnames")[[3]][X]), rh<-list(rh=rh, rel=X ))
rh <- list(rh=rh, pers=x)
#
	return(rh)
##
}
)
#

}
