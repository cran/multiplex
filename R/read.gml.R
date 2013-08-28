read.gml <-
function(file, directed=TRUE) #
{

arx <- scan(file, what="character", nlines=-1, quiet=TRUE)

### NODES ###
n <- length(grep("node",arx,fixed=TRUE))
#
lb <- vector()
for(i in 2:(n+1)) {
	lb[length(lb)+1] <- arx[grep("label",arx,fixed=TRUE)[i]+1]
}; rm(i)
#lb


### EDGES ###
edg <- arx[grep("edge",arx,fixed=TRUE)[1]:length(arx)]
#
e <- length(grep("source",edg,fixed=TRUE))
#
sr <- vector()
for(i in 1:e) {
	sr[length(sr)+1] <- edg[grep("source",edg,fixed=TRUE)[i]+1]
}; rm(i)
sr <- as.numeric(sr)+1
#sr
#
tg <- vector()
for(i in 1:e) {
	tg[length(tg)+1] <- edg[grep("source",edg,fixed=TRUE)[i]+3]
}; rm(i)
tg <- as.numeric(tg)+1
#tg


st<-vector()
for(i in 2:length(grep("graphics",edg,fixed=TRUE,value=TRUE))) {
tmp <- edg[(grep("graphics",edg,fixed=TRUE)[(i-1)]+2):(grep("graphics",edg,fixed=TRUE)[i]-7)]
	ifelse(isTRUE(length(tmp[grep("style",tmp,fixed=TRUE)+1])==0)==FALSE,st[length(st)+1]<-tmp[grep("style",tmp,fixed=TRUE)+1],st[length(st)+1]<-"default")
}; rm(i)
# y el último
tmp <- edg[tail(grep("graphics",edg,fixed=TRUE),1):length(edg)]
ifelse(isTRUE(length(tmp[grep("style",tmp,fixed=TRUE)+1])==0)==FALSE,st[length(st)+1]<-tmp[grep("style",tmp,fixed=TRUE)+1],st[length(st)+1]<-"default")
#length(st)

ndf <- cbind(sr,tg,st)

pr <- vector()
for(i in 1:nrow(ndf)) pr[length(pr)+1] <- paste(sr[i],tg[i],sep=", ")

z <- max(tabulate(factor(pr)))

mat <- array(0, dim=c(n,n,z))
#
if(isTRUE(nlevels(factor(st))>1)==TRUE) {
for(k in 1:nlevels(factor(st))) {
mdf<-subset(ndf,ndf[,3]==levels(factor(st))[k])
pr <- vector()
for(i in 1:nrow(mdf)) {
	pr[length(pr)+1] <- paste(mdf[i,1],mdf[i,2],sep=", ")
}; rm(i)
mat[,,k] <- transf(pr,'listmat',ord=n,labels=c(1:n))
}; rm(k)
} else if(isTRUE(nlevels(factor(st))>1)==FALSE) {
#
mt <- transf(pr,'listmat',ord=n,labels=c(1:n))
df <- data.frame(matrix(ncol = length(as.numeric(mt)), nrow = 0))
for(i in 1:max(mt)) df[i,] <- as.numeric(mt==i)
#
mat[,,1] <- dichot(transf(pr,'listmat',ord=n,labels=c(1:n)))
if(isTRUE(nrow(df)>1)==TRUE) {
for(i in 2:max(tabulate(factor(pr)))) mat[,,i] <- as.numeric(df[i,])
}
#
}


if(isTRUE(directed==FALSE)==TRUE) {
for(i in 1:z) {
mat[,,i] <- (mat[,,i]+t(mat[,,i]))
}; rm(i)
}


dimnames(mat)[[1]] <- dimnames(mat)[[2]] <- lb

   return(mat)

}
