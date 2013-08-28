cngr <-
function (S, PO=NULL, unique=FALSE) {
#
#
    if(isTRUE(attr(S, "class")[1]=="Semigroup")==FALSE) stop("\"S\" should be an object of a \"Semigroup\" class.")
s<-S
    if(isTRUE(attr(S, "class")[2]=="symbolic")==TRUE) {
	s <- convert(s, SemigroupClass=TRUE)
#	warning("Semigroup is in \'symbolic\' form.")
    } else { NA }
#

if(isTRUE(s$ord==1)==FALSE) {
mat<-matrix(0,nrow=s$ord,ncol=s$ord)
for(i in 1:s$ord) {
	mat[which(s$S==i)[1]]<-i
};rm(i)
inc <- levels(factor(orden(transf(mat))))


#
clus<-data.frame(matrix(ncol=s$ord,nrow=0))
for(i in 1:length(inc)) {#s$ord
	clus[i,] <- as.vector(sprt(s$S,strsplit(inc[i],", ")[[1]][1],strsplit(inc[i],", ")[[1]][2]))
	#print(sprt64(s$S,strsplit(inc[i],", ")[[1]][1],strsplit(inc[i],", ")[[1]][2]))
}; rm(i)
#
if(isTRUE(unique==TRUE)==TRUE) {
x <- unique(clus)
} else {
x <- clus
}

colnames(x) <- rownames(x) <- NULL
#
x <- data.matrix(x)
#
cg<-list(); for(i in 1:nrow(x)) cg[[i]]<-as.vector(x[i,]) 
#for(i in 1:nrow(x)) print(as.vector(x[i,]))

} else {
cg <- rep(1, s$dim)
}

ifelse(isTRUE(is.null(PO))==FALSE, lst <- list(S=S$S,PO=PO,clu=cg), lst <- list(S=S$S,clu=cg))
#ifelse(isTRUE(is.null(PO))==FALSE, lst <- list(S=S$S,PO=PO,clu=x), lst <- list(S=S$S,clu=x))
#lst <- list(S=S$S,clu=x)


ifelse(isTRUE(is.null(PO))==FALSE, class(lst)<-c("Congruence", "PO.Semigroup",attr(S, "class")[2]), class(lst)<-c("Congruence", "A.Semigroup",attr(S, "class")[2]))
#class(lst)<-c("Congruence",attr(S, "class"))

return(lst)
#	return(data.matrix(x))
#
}
