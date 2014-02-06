rm.isol <-
function(x, diag.incl=TRUE)#
{

if(isTRUE(dim(x)[3]>1)==TRUE) {

tmp<-x

ifelse(isTRUE(is.null(dimnames(x)[[1]]))==TRUE & isTRUE(is.null(dimnames(x)[[2]]))==TRUE, dimnames(x)[[2]]<-dimnames(x)[[1]]<-1:dim(x)[1], NA)
#warning("No labels are provided in \'x\'")


mpx<-data.frame(matrix(0,ncol=ncol(x),nrow=nrow(x)))
#
mpx<-x[,,1]+x[,,2]
if(isTRUE(dim(x)[3]>2)==TRUE) {
for(m in 3:dim(x)[3]) {
	mpx<-mpx+x[,,m]
	#ifelse(m==dim(x)[3], NA, mpx<-mpx+x[,,m])
#	if(isTRUE(dim(x)[3]>2)==TRUE) mpx<-mpx+x[,,dim(x)[3]]
}; rm(m)
}
#mpx
if(isTRUE(diag.incl==FALSE)==TRUE) diag(mpx) <- 0 
#

px<-mpx; rm(mpx)
out<-vector()
#k<-1
for (i in 1:nrow(px)) {
	if(isTRUE(sum(px[i,]+px[,i])==0)==TRUE) {
		out[length(out)+1] <- i
#		out[k]<-i; k<-k+1
	}
}; rm(i)
#out
#d<-
for (j in out) {
for (k in 1:nrow(px)) {
	px[,j]<-px[j,]<-NA
}
}; rm(j)

#
if(is.null(dimnames(x))==TRUE) dimnames(px)[[1]] <- 1:dim(x)[1]
lb<-dimnames(px)[[1]]
for (l in out) {
	lb[l]<-NA
}; rm(l)

#
pxx<-x
#
for(m in 1:dim(x)[3]) {
	pxx[which(is.na(lb)),,m]<-NA
	pxx[,which(is.na(lb)),m]<-NA
};rm(m)


mx <- array(dim=c((nrow(px)-length(out)),(nrow(px)-length(out)),dim(x)[3]))
#

for(m in 1:dim(x)[3]) {
#
npx <- data.frame(matrix(0,ncol=(nrow(px)-length(out)),nrow=0))
colnames(npx) <- as.vector(na.exclude(lb))
#
for (i in 1:nrow(px)) {
	ifelse(isTRUE(all(is.na(pxx[i,,m]))==FALSE)==TRUE, npx[i,] <- as.vector(na.exclude(pxx[i,,m])), NA)
}; rm(i)
#
mx[,,m] <- as.matrix(na.exclude(npx)) #as.matrix(npx)   # 
#
};rm(m)
#
if(is.null(dimnames(x))==FALSE) dimnames(mx)[[1]] <- dimnames(mx)[[2]] <- as.vector(na.exclude(lb))

if(isTRUE(is.null(dimnames(tmp)[[1]]))==TRUE & isTRUE(is.null(dimnames(tmp)[[2]]))==TRUE) {
dimnames(mx) <- NULL
warning("No labels are provided in \'x\'")
}
#attr(mx,"dimnames")[[3]] <- NULL


if(is.null(dimnames(x)[[3]])==FALSE) dimnames(mx)[[3]] <- dimnames(x)[[3]]

return(mx)

} else {

if(isTRUE(diag.incl==FALSE)==TRUE) {
dg<-diag
diag(x) <- 0 
}

if(isTRUE(sum(x)==0)==TRUE) { x 
	} else {
px<-x
out<-vector()
#k<-1
for (i in 1:nrow(px)) {
	if(isTRUE(sum(px[i,]+px[,i])==0)==TRUE) {
		out[length(out)+1] <- i
#		out[k]<-i; k<-k+1
	}
}; rm(i)
#out
#d<-nrow(px)
for (j in out) {
for (k in 1:nrow(px)) {
	px[,j]<-px[j,]<-NA
}
}; rm(j)
#px
#
lb<-dimnames(px)[[1]]
for (l in out) {
	lb[l]<-NA
}; rm(l)
#
npx<-data.frame(matrix(0,ncol=(nrow(px)-length(out)),nrow=0))
colnames(npx)<-as.vector(na.exclude(lb))
#
for (i in 1:nrow(px)) {
	ifelse(isTRUE(all(is.na(px[i,]))==FALSE)==TRUE, npx[i,]<-as.vector(na.exclude(px[i,])), NA)
}; rm(i)
mx<-as.matrix(na.exclude(npx))
dimnames(mx)[[1]]<-dimnames(mx)[[2]]<-as.vector(na.exclude(lb))

#ifelse(isTRUE(is.null(dimnames(tmp)[[1]]))==TRUE & isTRUE(is.null(dimnames(tmp)[[2]]))==TRUE, dimnames(mx) <- NULL, NA)

mx
}
}

}
