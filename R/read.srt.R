read.srt <-
function(file, header=TRUE, sep="\t", attr=FALSE, toarray=TRUE, dichot=FALSE, labels=NULL) #, obj=FALSE
{

ifelse(isTRUE(is.data.frame(file)==FALSE)==TRUE, x<-read.table(file, header = header, sep = sep ), x<-file)


if(isTRUE(attr==TRUE)==TRUE) {
	if(isTRUE(ncol(x)==2)==TRUE) {
	x <- as.data.frame(cbind(as.vector(x[,1]), as.vector(x[,1]), as.vector(x[,2])))
#	x <- x[2:nrow(x),]
} else if(isTRUE(ncol(x)>2)==TRUE) {
#	x[,2:ncol(x)]<-dichot(x[,2:ncol(x)])
	x<-cbind(x[,1],x[,1],x[,2:ncol(x)])
	#x <- x[2:nrow(x),]
}
	attr(x,"names")[1:2] <- c("n","n")
} #else if(isTRUE(attr==TRUE)==FALSE) {
#	NA 
#}

if(isTRUE(toarray==TRUE)==TRUE) {

R<-(ncol(x)-2) 
if(R==0) stop("You must specify at least one relation.")

if(is.null(labels)==TRUE) {
ifelse(isTRUE(R==1)==TRUE, MAT<-array(0,dim=c(length(levels(factor(as.vector(unlist(x[,1:2]))))),length(levels(factor(as.vector(unlist(x[,1:2]))))))),
	MAT<-array(0,dim=c(length(levels(factor(as.vector(unlist(x[,1:2]))))),length(levels(factor(as.vector(unlist(x[,1:2]))))),R)) )
	dimnames(MAT)[[1]]<-dimnames(MAT)[[2]]<-levels(factor(as.vector(unlist(x[,1:2]))))
#
} else {
ifelse(isTRUE(R==1)==TRUE, MAT<-array(0,dim=c(length(labels),length(labels))),
	MAT<-array(0,dim=c(length(labels),length(labels),R)) )
	dimnames(MAT)[[1]]<-dimnames(MAT)[[2]]<-labels
}
#
ifelse(isTRUE(R==1)==FALSE, dimnames(MAT)[[3]]<-attr(x,'names')[3:ncol(x)], NA)
#

Dims<- attr(MAT, "dimnames")[[1]]


if(isTRUE(ncol(x)>3)==TRUE) {
#
for (r in 3:ncol(x)) {
#if(isTRUE(is.factor(x[,r])==TRUE)==TRUE) stop("Relations cannot be \"factors\".")
rel<-which(x[,r]!=0)
#
rrel<-x[rel,]
#
X<-integer(length(Dims))
for (i in 1:length(Dims))	{
	X[i]<- sum(as.numeric(rrel[,1]==Dims[i]))
}; rm(i)
attr(X,"names")<-Dims 
#
xx<-vector()
for (i in 1:length(Dims)) {	
	ifelse( X[i] != 0, xx[i]<-i, xx[i]<-NA)
}; rm(i) 
attr(xx,"names")<-Dims 
xx<-(na.omit(xx))
xx<-as.vector(attr(xx,"names"))#; xx
#
nX<-X[which(X>0)]
if(isTRUE(length(xx)>1)==TRUE) {
YY<-vector()
for (i in 1:length(xx))  {   ## cuantos actores tienen outdegree
YY<-rrel[,2][which(rrel[,1]==attr(nX,'names')[i])]# los receivers por sender
if(isTRUE(length(YY)>1)==TRUE) {
	for (j in 1:length(YY))  {   ## por receiver
		MAT[(which((as.vector(rownames(MAT))==xx[i]),arr.ind=TRUE)),(which(as.vector(colnames(MAT)==YY[j]),arr.ind=TRUE)), (r-2)] <- as.integer(rrel[,r][which(rrel[,1]==attr(nX,'names')[i])])[j]  ## asigna valor
	}
	} else if(isTRUE(length(YY)==1)==TRUE) {
		MAT[(which((as.vector(rownames(MAT))==xx[i]),arr.ind=TRUE)),(which(as.vector(colnames(MAT)==YY),arr.ind=TRUE)), (r-2)]  <- as.integer(rrel[,r][which(rrel[,1]==attr(nX,'names')[i])])

	}
}; rm(i)
rm(xx,YY)
} else if(isTRUE(length(xx)==1)==TRUE) {
#diag(MAT) <- as.vector(X)
YY<-rrel[,2][which(rrel[,1]==attr(nX,'names'))] # los receivers por sender
#
if(isTRUE(length(YY)>1)==TRUE) {
	for (j in 1:length(YY))  {   ## por receiver
		MAT[(which((as.vector(rownames(MAT))==xx),arr.ind=TRUE)),(which(as.vector(colnames(MAT)==YY[j]),arr.ind=TRUE)), (r-2)] <- as.integer(rrel[,r][which(rrel[,1]==attr(nX,'names'))])[j]  ## asigna valor
	}; rm(j)
} else if(isTRUE(length(YY)==1)==TRUE) {
		MAT[(which((as.vector(rownames(MAT))==xx),arr.ind=TRUE)),(which(as.vector(colnames(MAT)==YY),arr.ind=TRUE)), (r-2)]  <- as.integer(rrel[,r][which(rrel[,1]==attr(nX,'names'))])
}
rm(xx,YY)

}

}; rm(r)

} else if(isTRUE(ncol(x)==3)==TRUE) {

rel<-which(x[,3]!=0)
#
rrel<-x[rel,]
X<-integer(length(Dims))
for (i in 1:length(Dims))	{
	X[i]<- sum(as.numeric(rrel[,1]==Dims[i]))
}; rm(i)
attr(X,"names")<-Dims 
#
xx<-vector()
for (i in 1:length(Dims)) {	
	ifelse( X[i] != 0, xx[i]<-i, xx[i]<-NA)
}; rm(i) 
attr(xx,"names")<-Dims 
xx<-(na.omit(xx))
xx<-as.vector(attr(xx,"names"))#; xx
#
nX<-X[which(X>0)]
#if(isTRUE(length(xx)>1)==TRUE) {
YY<-vector()

if(isTRUE(length(xx)>1)==TRUE) {

for (i in 1:length(xx))  {   ## cuantos actores tienen outdegree
YY<-rrel[,2][which(rrel[,1]==attr(nX,'names')[i])] #  los receivers por sender
#	for (j in 1:length(YY))  {   ## por receiver
		MAT[which(dimnames(MAT)[1][[1]]==YY),which(dimnames(MAT)[2][[1]]==YY)] <- as.integer(as.vector(rrel[,3][i]))

#		MAT[(which((as.vector(rownames(MAT))==xx[i]),arr.ind=TRUE)),(which(as.vector(colnames(MAT)==YY),arr.ind=TRUE)), 1] <- as.integer(rrel[,3][which(rrel[,1]==attr(nX,'names')[i])])   ## asigna valor
#	}
}
rm(i); rm(xx,YY)
} else if(isTRUE(length(xx)==1)==TRUE) {

YY<-rrel[,2][which(rrel[,1]==attr(nX,'names'))] # los receivers por sender
#
if(isTRUE(length(YY)>1)==TRUE) {
	for (j in 1:length(YY))  {   ## por receiver
		MAT[(which((as.vector(rownames(MAT))==xx),arr.ind=TRUE)),(which(as.vector(colnames(MAT)==YY[j]),arr.ind=TRUE)), 1] <- as.integer(rrel[,3][which(rrel[,1]==attr(nX,'names'))])[j]  ## asigna valor
	}; rm(j)
} else if(isTRUE(length(YY)==1)==TRUE) {
		MAT[which(dimnames(MAT)[1][[1]]==YY),which(dimnames(MAT)[2][[1]]==YY)] <- as.integer(as.vector(rrel[,3]))
		#as.integer(rrel[,3][which(rrel[,1]==attr(nX,'names'))])  ## asigna valor
}
rm(xx,YY)
}

}


if(isTRUE(dichot==TRUE)==TRUE) {
#
	MAT<-dichot(MAT)
}

return(MAT)

}

if(isTRUE(dichot==TRUE)==TRUE) {
	x[,3:ncol(x)]<-dichot(x[,3:ncol(x)])
}

return(x)

}
