rel.sys <-
function(x, bonds = c("entire","strong","weak") ) { #, prsep=", "


bd <- bundles(x,collapse=FALSE)
#
switch(match.arg(bonds),entire = lbd <- bd,
			strong = lbd <- list(bd$recp, bd$txch, bd$mixed, bd$full),
			weak = lbd <- list(bd$asym, bd$tent) )

if(is.na(dim(x)[3])==FALSE) {
stb<-list()
for(k in 1:dim(x)[3]) {
	tmp<-vector()
	for(i in 1:length(lbd)) {
		tmp<-append(tmp,lbd[[i]][k])
	}; rm(i)
	stb[[k]]<-as.vector(unlist(tmp))
}; rm(k)
} else {
	stb<-vector()
	for(i in 1:length(lbd)) {
		stb<-append(stb,lbd[[i]])
	}; rm(i)
}

if(length(stb)>0) {
tmp<-vector()
for(k in 1:length(stb)) {
	for(i in 1:length(stb[[k]])) {
if(length(stb[[k]])>0) {
		tmp<-append(tmp,dhc(stb[[k]][i]))
}
	};rm(i)
};rm(k)
} else {
tmp <- stb <- logical(0)
}

if(is.na(dim(x)[3])==FALSE) {
ifelse(is.null(dimnames(x)[[3]])==FALSE, attr(stb,'names')<-dimnames(x)[[3]], attr(stb,'names')<-1:dim(x)[3] )
}

class(stb) <- "Rel.System"

#ifelse(isTRUE(is.null(dimnames(x)[[1]])==TRUE)==TRUE, return(list(ord=nlevels(factor(tmp)), actors=1:dim(x)[1], ties=stb, bond.type=bonds)),
#						      return(list(ord=nlevels(factor(tmp)), actors=dimnames(x)[[1]][which(dimnames(x)[[1]]%in%levels(factor(tmp)))], ties=stb, bond.type=bonds)))

return(list(ord=dim(x)[1], nodes=dimnames(x)[[1]], sys.ord=nlevels(factor(tmp)), incl=dimnames(x)[[1]][which(dimnames(x)[[1]]%in%levels(factor(tmp)))],
	    excl=dimnames(x)[[1]][which(!(dimnames(x)[[1]]%in%levels(factor(tmp))))], bond.type=bonds, size=length(unlist(stb)), ties=stb))

}
