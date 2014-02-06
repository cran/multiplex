bundles <-
function(m, collapse=FALSE) { #, noquote=FALSE

### LABELS 
ifelse(isTRUE(is.null(dimnames(m)[1])==TRUE | is.null(dimnames(m)[1][[1]])==TRUE)==TRUE, LBS<-1:nrow(m), LBS<-dimnames(m)[[1]])
#ifelse(sum(as.numeric(dimnames(m)[[1]])==0)==0, lbs<-as.numeric(LBS), lbs<-seq(LBS))
#ifelse(isTRUE(is.numeric(dimnames(m)[[1]])==TRUE)==TRUE, lbs<-as.numeric(LBS), lbs<-seq(LBS))
#if(suppressWarnings(isTRUE(any(is.na(as.numeric(dimnames(m)[[1]])))==TRUE)==FALSE)) {
#	lbs<-as.numeric(LBS)
#	} else if(isTRUE(is.numeric(dimnames(m)[[1]])==TRUE)==FALSE) { 
	lbs<-seq(LBS)
#	}
#lbs<-seq(LBS)
#
if(isTRUE(is.na(dim(m)[3]))==FALSE) {
	if(isTRUE(is.null(dimnames(m)[[3]])==TRUE) | isTRUE(any(duplicated(dimnames(m)[[3]])))==TRUE) dimnames(m)[[3]]<-1:dim(m)[3]
#if(isTRUE(any(duplicated(dimnames(m)[[3]])))==TRUE) 
}

############ FOR EACH ACTOR  ##################
if(is.na(dim(m)[3])==FALSE) {
	x<-transf(dichot(m)[,,1],'matlist',labels=lbs,prsep=", ",lb2lb=TRUE)
	for(k in 2:dim(m)[3]) x<-append(x,transf(dichot(m)[,,k],'matlist',labels=lbs,prsep=", ",lb2lb=TRUE)); rm(k)
} else {
	x<-transf(dichot(m),'matlist',labels=lbs,prsep=", ",lb2lb=TRUE)
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
#
DF <- na.omit(DF)


### ALL
out<-list(); inn<-list(); All<-list()
#### LB2LB NOT ALLOWED YET....
for(i in 1:length(lbs)) {
	out[[i]] <- as.numeric(DF[which(DF[,1]==as.numeric(lbs[i])),2])
	inn[[i]] <- as.numeric(DF[which(DF[,2]==as.numeric(lbs[i])),1])
	All[[i]] <- c(out[[i]],inn[[i]])
}; rm(i)
#rm(DF)
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


### RECIPROCAL + TIE EXCHANGE (CHECK no mixeds..)
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
	assign(allr, transf(dichot(m)[,,k],'matlist',labels=lbs,prsep=", ",lb2lb=TRUE) )
#
tmp<-transf(dichot(m)[,,k],'matlist',labels=lbs,prsep=", ",lb2lb=TRUE)
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


###############################################################################################
#
## SOLVED BUG 'xchg' is now ignored cuando de menor a mayor empieza en la segunda relación, será.. <- transf(dichot(m))
## BUG 'xchg' was taken as 'recp' ?? cuando de menor a mayor empieza en la segunda relación, será.. ???
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
		ifelse(isTRUE(rete[[i]][j]%in%which(tabulate(eval(as.name(allr))[[i]])==1))==TRUE, tmpxchr[length(tmpxchr)+1] <- rete[[i]][j], NA)
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

#### RECIPROCAL
recp<-list()
if(isTRUE(is.na(dim(m)[3]))==FALSE) {
for(i in 1:length(rete)) {
	recp[[i]] <- rete[[i]][which(!(rete[[i]]%in%xchg[[i]]))]
}; rm(i)
#attr(recp,'names') <- lbs
	} else {
recp <- rete
}

###############################################################################################



#### case: 2 occurence... TIE ENTRAINMENT
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
#
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
#####


#### MIXED (bug: full was included here when exist => mix before mixe solves)
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



#########################################################################

As<-vector()
for(i in 1:length(asym)) {
	for(j in 1:length(asym[[i]])) {
	if(isTRUE(length(asym[[i]])!=0)==TRUE) {
		if(isTRUE(is.na(dim(m)[3]))==FALSE) {
			As<-append(As, paste(lbs[i],asym[[i]][j],sep=", "))
		} else {
			ifelse(isTRUE(asym[[i]][j]%in%out[[i]])==TRUE, As<-append(As, paste(lbs[i],asym[[i]][j],sep=", ")), NA)
		}
	}
	}; rm(j)
}; rm(i)
if(isTRUE(is.na(dim(m)[3]))==FALSE) {
AS<-list()
for(k in 1:length(tt)) {
tmp<-vector()
for(i in which(As%in%transf(m[,,k],labels=lbs,prsep=", ",lb2lb=TRUE))) {
	tmp<-append(tmp,As[i])
}; rm(i)
AS[[k]]<-tmp
}; rm(k)
	} else {
AS <- As
}
#AS

Rp<-vector()
for(i in 1:length(recp)) {
	for(j in 1:length(recp[[i]])) {
	if(isTRUE(length(recp[[i]])!=0)==TRUE) {
#		if(isTRUE(lbs[i]<recp[[i]][j])==TRUE)
		Rp<-append(Rp, paste(lbs[i],recp[[i]][j],sep=", "))
	}
	}; rm(j)
}; rm(i)
if(isTRUE(is.na(dim(m)[3]))==FALSE) {
RP<-list()
for(k in 1:length(tt)) {
tmp<-vector()
for(i in which(Rp%in%transf(m[,,k],labels=lbs,prsep=", ",lb2lb=TRUE))) {
	tmp<-append(tmp,Rp[i])
}; rm(i)
RP[[k]]<-tmp
}; rm(k)
	} else {
RP <- Rp
}
#RP


Xc<-vector()
if(isTRUE(is.na(dim(m)[3]))==FALSE) {
for(i in 1:length(xchg)) {
	for(j in 1:length(xchg[[i]])) {
	if(isTRUE(length(xchg[[i]])!=0)==TRUE) {
		if(isTRUE(lbs[i]<xchg[[i]][j])==TRUE)
		Xc<-append(Xc, paste(lbs[i],xchg[[i]][j],sep=", "))
	}
	}; rm(j)
}; rm(i)
XC<-list(); XCt<-list()
for(k in 1:length(tt)) {
tmp<-vector();tmpt<-vector()
for(i in which(Xc%in%transf(m[,,k],labels=lbs,prsep=", ",lb2lb=TRUE))) {
	tmp<-append(tmp,Xc[i])
}; rm(i)
for(i in which(Xc%in%transf(t(m[,,k]),labels=lbs,prsep=", ",lb2lb=TRUE))) {
	tmpt<-append(tmpt, paste(strsplit(Xc[i],", ")[[1]][2],strsplit(Xc[i],", ")[[1]][1],sep=", ") )
#	tmpt<-append(tmpt,Xc[i])
}; rm(i)
XC[[k]]<-tmp
XCt[[k]]<-tmpt
}; rm(k)
XCH<-list()
for(i in 1:length(XC)) {
	XCH[[i]]<-c(XC[[i]],XCt[[i]])
};rm(i)
	} else {
XCH <- Xc
}
#XCH


## BUG Add: na.omit(TEout[[i]])
Et<-vector()
if(isTRUE(is.na(dim(m)[3]))==FALSE) {
for(i in 1:length(Eout)) {
	for(j in 1:length(Eout[[i]])) {
	if(isTRUE(length(Eout[[i]])!=0)==TRUE) {
		Et<-append(Et, paste(lbs[i],Eout[[i]][j],sep=", "))
	}
	}; rm(j)
}; rm(i)
for(i in 1:length(TEout)) {
	for(j in 1:length(TEout[[i]])) {
	if(isTRUE(is.na(na.omit(TEout[[i]]))==FALSE)==TRUE) {
		Et<-append(Et, paste(lbs[i],TEout[[i]][j],sep=", "))
	}
	}; rm(j)
}; rm(i)
ENT<-list()
for(k in 1:length(tt)) {
tmp<-vector()
for(i in which(Et%in%transf(m[,,k],labels=lbs,prsep=", ",lb2lb=TRUE))) {
	tmp<-append(tmp,Et[i])
}; rm(i)
ENT[[k]]<-tmp
}; rm(k)
	} else {
ENT <- Et
}
#ENT


Mx<-vector()
if(isTRUE(is.na(dim(m)[3]))==FALSE) {
for(i in 1:length(mixe)) {
	for(j in 1:length(mixe[[i]])) {
	if(isTRUE(length(mixe[[i]])!=0)==TRUE) {
		if(isTRUE(lbs[i]<mixe[[i]][j])==TRUE) #MX[k,1]<-lbs[i]; MX[k,2]<-mixe[[i]][j]; k<-k+1
			Mx<-append(Mx, paste(lbs[i],mixe[[i]][j],sep=", "))
	}
	}; rm(j)
}; rm(i)
MX<-list(); MXt<-list()
for(k in 1:length(tt)) {
tmp<-vector();tmpt<-vector()
for(i in which(Mx%in%transf(m[,,k],labels=lbs,prsep=", ",lb2lb=TRUE))) {
	tmp<-append(tmp,Mx[i])
}; rm(i)
for(i in which(Mx%in%transf(t(m[,,k]),labels=lbs,prsep=", ",lb2lb=TRUE))) {
	tmpt<-append(tmpt, paste(strsplit(Mx[i],", ")[[1]][2],strsplit(Mx[i],", ")[[1]][1],sep=", ") )
#	tmpt<-append(tmpt,Mx[i])
}; rm(i)
MX[[k]]<-tmp
MXt[[k]]<-tmpt
}; rm(k)
MIX<-list()
for(i in 1:length(MX)) {
	MIX[[i]]<-c(MX[[i]],MXt[[i]])
};rm(i)
	} else {
MIX <- Mx
}
#MIX


Fl<-vector()
if(isTRUE(is.na(dim(m)[3]))==FALSE) {
for(i in 1:length(full)) {
	for(j in 1:length(full[[i]])) {
	if(isTRUE(length(full[[i]])!=0)==TRUE) {
		if(isTRUE(lbs[i]<full[[i]][j])==TRUE)
			Fl<-append(Fl, paste(lbs[i],full[[i]][j],sep=", "))
	}
	}; rm(j)
}; rm(i)
FL<-list(); FLt<-list()
for(k in 1:length(tt)) {
tmp<-vector();tmpt<-vector()
for(i in which(Fl%in%transf(m[,,k],labels=lbs,prsep=", ",lb2lb=TRUE))) {
	tmp<-append(tmp,Fl[i])
}; rm(i)
for(i in which(Fl%in%transf(t(m[,,k]),labels=lbs,prsep=", ",lb2lb=TRUE))) {
	tmpt<-append(tmpt, paste(strsplit(Fl[i],", ")[[1]][2],strsplit(Fl[i],", ")[[1]][1],sep=", ") )
#	tmpt<-append(tmpt,Fl[i])
}; rm(i)
FL[[k]]<-tmp
FLt[[k]]<-tmpt
}; rm(k)
FUL<-list()
for(i in 1:length(FL)) {
	FUL[[i]]<-c(FL[[i]],FLt[[i]])
};rm(i)
	} else {
FUL <- Fl
}
#FUL



### LB2LB
#if(isTRUE(is.numeric(dimnames(m)[[1]])==TRUE)==FALSE) { 
if(isTRUE(is.null(dimnames(m)[[1]])==TRUE)==FALSE) {
#tmp<-AS
if(length(AS)>0) {
for(k in 1:length(AS)) {
for(i in 1:length(AS[[k]])) {
#print(paste(dimnames(m)[[1]][as.numeric(strsplit(AS[[k]][i], ", ")[[1]][1])], dimnames(m)[[1]][as.numeric(strsplit(AS[[k]][i], ", ")[[1]][2])], sep=", "))
if(length(AS[[k]])>0) {
AS[[k]][i]<-paste(dimnames(m)[[1]][as.numeric(strsplit(AS[[k]][i], ", ")[[1]][1])], dimnames(m)[[1]][as.numeric(strsplit(AS[[k]][i], ", ")[[1]][2])], sep=", ")
}
};rm(i)
};rm(k)
}
#
if(length(RP)>0) {
for(k in 1:length(RP)) {
for(i in 1:length(RP[[k]])) {
#print(paste(dimnames(m)[[1]][as.numeric(strsplit(RP[[k]][i], ", ")[[1]][1])], dimnames(m)[[1]][as.numeric(strsplit(RP[[k]][i], ", ")[[1]][2])], sep=", "))
if(length(RP[[k]])>0) {
RP[[k]][i]<-paste(dimnames(m)[[1]][as.numeric(strsplit(RP[[k]][i], ", ")[[1]][1])], dimnames(m)[[1]][as.numeric(strsplit(RP[[k]][i], ", ")[[1]][2])], sep=", ")
}
};rm(i)
};rm(k)
}
#
if(isTRUE(is.na(dim(m)[3]))==FALSE) {
#
if(length(XCH)>0) {
for(k in 1:length(XCH)) {
for(i in 1:length(XCH[[k]])) {
#print(paste(dimnames(m)[[1]][as.numeric(strsplit(XCH[[k]][i], ", ")[[1]][1])], dimnames(m)[[1]][as.numeric(strsplit(XCH[[k]][i], ", ")[[1]][2])], sep=", "))
if(length(XCH[[k]])>0) {
XCH[[k]][i]<-paste(dimnames(m)[[1]][as.numeric(strsplit(XCH[[k]][i], ", ")[[1]][1])], dimnames(m)[[1]][as.numeric(strsplit(XCH[[k]][i], ", ")[[1]][2])], sep=", ")
}
};rm(i)
};rm(k)
}
#
if(length(ENT)>0) {
for(k in 1:length(ENT)) {
for(i in 1:length(ENT[[k]])) {
#print(paste(dimnames(m)[[1]][as.numeric(strsplit(ENT[[k]][i], ", ")[[1]][1])], dimnames(m)[[1]][as.numeric(strsplit(ENT[[k]][i], ", ")[[1]][2])], sep=", "))
if(length(ENT[[k]])>0) {
ENT[[k]][i]<-paste(dimnames(m)[[1]][as.numeric(strsplit(ENT[[k]][i], ", ")[[1]][1])], dimnames(m)[[1]][as.numeric(strsplit(ENT[[k]][i], ", ")[[1]][2])], sep=", ")
}
};rm(i)
};rm(k)
}
#
if(length(MIX)>0) {
for(k in 1:length(MIX)) {
for(i in 1:length(MIX[[k]])) {
#print(paste(dimnames(m)[[1]][as.numeric(strsplit(MIX[[k]][i], ", ")[[1]][1])], dimnames(m)[[1]][as.numeric(strsplit(MIX[[k]][i], ", ")[[1]][2])], sep=", "))
if(length(MIX[[k]])>0) {
MIX[[k]][i]<-paste(dimnames(m)[[1]][as.numeric(strsplit(MIX[[k]][i], ", ")[[1]][1])], dimnames(m)[[1]][as.numeric(strsplit(MIX[[k]][i], ", ")[[1]][2])], sep=", ")
}
};rm(i)
};rm(k)
}
#
if(length(FUL)>0) {
for(k in 1:length(FUL)) {
for(i in 1:length(FUL[[k]])) {
#print(paste(dimnames(m)[[1]][as.numeric(strsplit(FUL[[k]][i], ", ")[[1]][1])], dimnames(m)[[1]][as.numeric(strsplit(FUL[[k]][i], ", ")[[1]][2])], sep=", "))
if(length(FUL[[k]])>0) {
FUL[[k]][i]<-paste(dimnames(m)[[1]][as.numeric(strsplit(FUL[[k]][i], ", ")[[1]][1])], dimnames(m)[[1]][as.numeric(strsplit(FUL[[k]][i], ", ")[[1]][2])], sep=", ")
}
};rm(i)
};rm(k)
}
#
}
#
}
#}
###


if(isTRUE(is.na(dim(m)[3]))==FALSE) {
	attr(FUL,'names')<-attr(MIX,'names')<-attr(ENT,'names')<-attr(XCH,'names')<-attr(RP,'names')<-attr(AS,'names') <- tt #dimnames(m)[[3]]
	}

#print(list(ASYM=AS,RECP=RP,TENT=ENT,TXCH=XCH,MIXED=MIX,FULL=FUL),quote=FALSE)


if(collapse) { 
		lst<-list(asym=as.vector(unlist(AS)),recp=as.vector(unlist(RP)),tent=as.vector(unlist(ENT)),txch=as.vector(unlist(XCH)),
		mixed=as.vector(unlist(MIX)),full=as.vector(unlist(FUL)))
		class(lst) <- "Rel.Bundles"
		return(lst) 
	} else { 
		lst<-list(asym=AS,recp=RP,tent=ENT,txch=XCH,mixed=MIX,full=FUL)
		class(lst) <- "Rel.Bundles"
		return(lst)
	}

#if(noquote) { return(list(asym=noquote(AS),recp=noquote(RP),tent=noquote(ENT),txch=noquote(XCH),mixed=noquote(MIX),full=noquote(FUL))) 
#	} else { return(list(asym=AS,recp=RP,tent=ENT,txch=XCH,mixed=MIX,full=FUL))}

}
