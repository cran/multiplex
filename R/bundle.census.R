bundle.census <-
function(m) {

if(isTRUE(is.array(m)==FALSE)==TRUE) stop("\'m\' should be an array object")

### LABELS 
ifelse(isTRUE(is.null(dimnames(m)[1])==TRUE)==TRUE, LBS<-1:nrow(m), LBS<-dimnames(m)[[1]])
ifelse(sum(as.numeric(dimnames(m)[[1]])==0)==0, lbs<-as.numeric(LBS), lbs<-seq(LBS))

############ FOR EACH ACTOR  ##################
if(is.na(dim(m)[3])==FALSE) {
	x<-transf(m[,,1],'matlist',labels=lbs,prsep=", ",lb2lb=TRUE)
	for(k in 2:dim(m)[3]) x<-append(x,transf(m[,,k],'matlist',labels=lbs,prsep=", ",lb2lb=TRUE)); rm(k)
} else {
	x<-transf(m,'matlist',labels=lbs,prsep=", ",lb2lb=TRUE)
}

##### REMOVE LOOPS ... in 'dfl'   ###
dfl<-data.frame(matrix(ncol=2,nrow=0))
for(i in 1:length(x)) {
	dfl[i,1] <- strsplit(x[i],", ")[[1]][1]
	dfl[i,2] <- strsplit(x[i],", ")[[1]][2]
}; rm(i)
#dfl
#
DF<-data.frame(matrix(ncol=2,nrow=0))
k<-1
for(i in 1:nrow(dfl)) {
	if(isTRUE(dfl[i,1]!=dfl[i,2])==TRUE) DF[k,]<-dfl[i,]; k<-k+1
}; rm(i)
rm(k)


### ALL
out<-list(); inn<-list(); All<-list()
for(i in 1:length(lbs)) {
	out[[i]] <- as.numeric(DF[which(DF[,1]==as.numeric(lbs[i])),2])
	inn[[i]] <- as.numeric(DF[which(DF[,2]==as.numeric(lbs[i])),1])
	All[[i]] <- c(out[[i]],inn[[i]])
}; rm(i)
rm(DF)
#attr(inn,'names')<-attr(out,'names')<-attr(All,'names') <- lbs


###############
### bug FULL
finn<-list(); fout<-list()
for(i in seq(lbs)) {
	finn[[i]] <- which(tabulate(inn[[i]])== dim(m)[3]) 
	fout[[i]] <- which(tabulate(out[[i]])== dim(m)[3])
}; rm(i)
full<-list()
for(i in seq(lbs)) {
	full[[i]] <- intersect(fout[[i]],finn[[i]])
}; rm(i)
rm(finn,fout)
#attr(full,'names')<-lbs


## case: 1 occurence... ASYMMETRIC  ***NOT***  FULL
asym<-list() #full<-list();  
### FOR Actors 1 to N in 'all''
for(i in seq(lbs)) {
	asym[[i]] <- which(tabulate(All[[i]])== 1)
}; rm(i)
#attr(asym,'names')<-lbs


### DOUBLE OCCURENCES...
dobl<-list(); dout<-list()	#; dinn<-list()
for(i in seq(lbs)) {
	dobl[[i]] <- which(tabulate(All[[i]])== 2)
	dout[[i]] <- which(tabulate(out[[i]])== 2)
}; rm(i)


### RECIPROCAL + TIE EXCHANGE
rete<-list()
for(i in 1:length(dobl)) {
	tmprte<-vector()
	for(j in 1:length(dobl[[i]])) {
	if(isTRUE(dobl[[i]][j]%in%which(tabulate(inn[[i]])==1))==TRUE && isTRUE(dobl[[i]][j]%in%which(tabulate(out[[i]])==1))==TRUE) tmprte[length(tmprte)+1] <- dobl[[i]][j]
	}; rm(j)
rete[[i]]<-tmprte
}; rm(i)
rm(tmprte)
#attr(rete,'names')<-lbs


# simplify labels 3d array
if(isTRUE(is.na(dim(m)[3]))==FALSE) {
tmp<-list(); tt<-vector()
if(isTRUE(is.null(dimnames(m)[[3]]))==FALSE) {
for (i in 1:length(dimnames(m)[[3]])) tmp[i]<-dimnames(m)[[3]][i]; rm(i)
#
for (i in 1:length(tmp)) tt[i]<-(strsplit(tmp[[i]],"")[[1]][1]); rm(i)
} else {
for (i in 1:dim(m)[3]) tt[i]<-tmp[i]<-i; rm(i)
}
#
for(k in 1:length(tt)) {
	allr <- paste('all',tt[k],sep='_')
	assign(allr, transf(m[,,k],'matlist',labels=lbs,prsep=", ",lb2lb=TRUE) )
#
tmp<-transf(m[,,k],'matlist',labels=lbs,prsep=", ",lb2lb=TRUE)
tDF<-data.frame(matrix(ncol=2,nrow=0))
for(i in 1:length(tmp)) {
	tDF[i,1] <- strsplit(tmp[i],", ")[[1]][1]
	tDF[i,2] <- strsplit(tmp[i],", ")[[1]][2]
}; rm(i)
rm(tmp)
#
oud<-list(); ind<-list(); ald<-list()
for(i in 1:length(lbs)) {
	oud[[i]] <- as.numeric(tDF[which(tDF[,1]==as.numeric(lbs[i])),2])
	ind[[i]] <- as.numeric(tDF[which(tDF[,2]==as.numeric(lbs[i])),1])
	ald[[i]] <- c(oud[[i]],ind[[i]])
}; rm(i)
assign(allr,ald)
rm(oud,ind,ald,allr); rm(tDF)
}
rm(k)
	} else {
tt <- "R"
tmp <- x
}
#rm(x)

#
### Tie Exchanges_r
if(isTRUE(is.na(dim(m)[3]))==FALSE) {
#k<-1
for(k in 1:length(tt)) {
tmpxchg<-list()
	xchr <- paste('xch',tt[k],sep='_')
for(i in 1:length(rete)) {
	allr <- paste('all',tt[k],sep='_')
	tmpxchr<-vector()
	for(j in 1:length(rete[[i]])) {
		if(isTRUE(rete[[i]][j]%in%which(tabulate(eval(as.name(allr))[[i]])==1))==TRUE) tmpxchr[length(tmpxchr)+1] <- rete[[i]][j]
	}; rm(j)
tmpxchg[[i]] <- tmpxchr
}; rm(i)
#attr(tmpxchg,'names') <- lbs
#
assign(xchr, tmpxchg)
}; rm(k)
rm(tmpxchr); rm(tmpxchg)
#
## juntar xchrs...
### TIE EXCHANGE
xchg<-list(); length(xchg)<-length(lbs)
for(k in 1:length(tt)) {#
	xchr <- paste('xch',tt[k],sep='_')
for(i in 1:length(rete)) {
	vecr <- paste('vec',i,sep='')
	tmpxch<-vector()
	tmpxch <-eval(as.name(xchr))[[i]]
if(sum(tmpxch)>0) {
	assign(vecr,tmpxch)
	xchg[[i]] <- eval(as.name(vecr))
	rm(vecr)
}else{rm(vecr)}
#rm(vecr)
}; rm(i)
}; rm(k)
rm(tmpxch); rm(xchr)
#
#attr(xchg,'names') <- lbs
	} else {
xchg <- logical(0)
}

### RECIPROCAL
recp<-list()
if(isTRUE(is.na(dim(m)[3]))==FALSE) {
for(i in 1:length(rete)) {
	recp[[i]] <- rete[[i]][which(!(rete[[i]]%in%xchg[[i]]))]
}; rm(i)
attr(recp,'names') <- lbs
	} else {
recp <- rete
}


## case: 2 occurence... TIE ENTRAINMENT
if(isTRUE(is.na(dim(m)[3]))==FALSE) {
Eout<-list(); length(Eout)<-length(lbs)
for(i in 1:length(dobl)) {
	tmpout<-vector()
	for(j in 1:length(dobl[[i]])) {
		if(isTRUE(dobl[[i]][j]%in%dout[[i]])==TRUE) tmpout[length(tmpout)+1] <- dobl[[i]][j]
	}; rm(j)
Eout[[i]] <- tmpout
}
rm(tmpout)
#attr(Einn,'names')<-attr(Eout,'names') <- lbs


### TRIPLE+r OCCURENCES...
trpr<-list(); tinn<-list(); tout<-list()
for(i in seq(lbs)) {
	trpr[[i]] <- which(tabulate(All[[i]])> 2)
	tinn[[i]] <- which(tabulate(inn[[i]])> 2)
	tout[[i]] <- which(tabulate(out[[i]])> 2)
};rm(i)
#attr(tinn,'names')<-attr(tout,'names')<-attr(trpr,'names') <- lbs
#trpr
#
## case: 3+ occurences... TUPLE ENTRAINMENT
teinn<-list(); teout<-list(); length(teinn)<-length(lbs); length(teout)<-length(lbs)
for(i in 1:length(trpr)) {
	tmpinn<-vector(); tmpout<-vector()
	for(j in 1:length(trpr[[i]])) {
		if(isTRUE(trpr[[i]][j]%in%tinn[[i]])==TRUE) tmpinn[length(tmpinn)+1] <- trpr[[i]][j]
		if(isTRUE(trpr[[i]][j]%in%tout[[i]])==TRUE) tmpout[length(tmpout)+1] <- trpr[[i]][j]
	}; rm(j)
teinn[[i]] <- tmpinn
teout[[i]] <- tmpout
}
rm(tmpinn,tmpout)
#attr(teinn,'names')<-attr(teout,'names') <- lbs
#
TEinn<-list(); TEout<-list()
for(i in 1:length(trpr)) {
	tmpinn<-vector(); tmpout<-vector() 
	for(j in 1:length(trpr[[i]])) {
		if(isTRUE(!(teinn[[i]][j]%in%out[[i]]))==TRUE) tmpinn[length(tmpinn)+1]<-teinn[[i]][j]
		if(isTRUE(!(teout[[i]][j]%in%inn[[i]]))==TRUE) tmpout[length(tmpout)+1]<-teout[[i]][j]
	}; rm(j)
TEinn[[i]] <- tmpinn
TEout[[i]] <- tmpout
}; rm(i)
rm(tmpinn,tmpout)
#
#attr(TEinn,'names')<-attr(TEout,'names') <- lbs
	} else {
TEinn <- TEout <- Eout <- logical(0)
}


### MIXED (bug: full was included here when exist)
mix<-list()
if(isTRUE(is.na(dim(m)[3]))==FALSE) {
for(i in 1:length(trpr)) {
	mix[[i]] <- trpr[[i]][which(!(trpr[[i]]%in%TEinn[[i]]|trpr[[i]]%in%TEout[[i]]))]
}; rm(i)
#
mixe<-list()
for(i in 1:length(mix)) {
	mixe[[i]] <- mix[[i]][which(!(mix[[i]]%in%full[[i]]))]
#	}
}; rm(i)
#
	} else {
mixe <- logical(0)
}


#return( list(
#BNDLNULL=choose(nrow(m),2), 
#BNDL=choose(nrow(m),2)-(choose(nrow(m),2))-(length(unlist(full))+length(unlist(asym))/2+length(unlist(recp))/2+length(unlist(xchg))/2+(length(unlist(Eout))+length(na.exclude(unlist(TEout))))+length(unlist(mixe))/2)
#NULL=(choose(nrow(m),2))-(length(unlist(full))+length(unlist(asym))/2+length(unlist(recp))/2+length(unlist(xchg))/2+(length(unlist(Eout))+length(na.exclude(unlist(TEout))))+length(unlist(mixe))/2), 
#FULL=length(unlist(full))/2, 
#ASYM=length(unlist(asym))/2, 
#RECP=length(unlist(recp))/2, 
#TXCH=length(unlist(xchg))/2,
#TENTR=(length(unlist(Eout))+length(na.exclude(unlist(TEout)))), 
#MIXED=length(unlist(mixe))/2 ) )


#bc<-cbind(choose(nrow(m),2), abs(choose(nrow(m),2)-(choose(nrow(m),2))-(length(unlist(full))+length(unlist(asym))/2+length(unlist(recp))/2+length(unlist(xchg))/2+(length(unlist(Eout))+length(na.exclude(unlist(TEout))))+length(unlist(mixe))/2)), (choose(nrow(m),2))-(length(unlist(full))/2+length(unlist(asym))/2+length(unlist(recp))/2+length(unlist(xchg))/2+(length(unlist(Eout))+length(na.exclude(unlist(TEout))))+length(unlist(mixe))/2), length(unlist(asym))/2, length(unlist(recp))/2, (length(unlist(Eout))+length(na.exclude(unlist(TEout)))), length(unlist(xchg))/2, length(unlist(mixe))/2, length(unlist(full))/2 )
#colnames(bc)<-c('BNDLSYNULL','BUNDLES','NULL','ASYMM','RECIP','T.ENTR','T.EXCH','MIXED','FULL'); rownames(bc)<-'TOTAL'

bc<-cbind(
abs(choose(nrow(m),2)-(choose(nrow(m),2))-(length(unlist(full))/2+length(unlist(asym))/2+length(unlist(recp))/2+length(unlist(xchg))/2+(length(unlist(Eout))+length(na.exclude(unlist(TEout))))+length(unlist(mixe))/2)), (choose(nrow(m),2))-(length(unlist(asym))/2+length(unlist(recp))/2+length(unlist(xchg))/2+(length(unlist(Eout))+length(na.exclude(unlist(TEout))))+length(unlist(mixe))/2+length(unlist(full))/2), 
length(unlist(asym))/2, 
length(unlist(recp))/2, 
(length(unlist(Eout))+length(na.exclude(unlist(TEout)))), 
length(unlist(xchg))/2, 
length(unlist(mixe))/2, 
length(unlist(full))/2 )
colnames(bc)<-c('BUNDLES','NULL','ASYMM','RECIP','T.ENTR','T.EXCH','MIXED','FULL'); rownames(bc)<-'TOTAL'


return(bc)


}
