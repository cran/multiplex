summaryBundles <-
function(x, file = NULL, latex = FALSE) { #, prsep = ", "

if(isTRUE(attr(x, "class")=="Rel.Bundles")==FALSE) stop("Data must be a \"Rel.Bundles\" class.")

if(latex) { if(isTRUE(is.null(file))==TRUE) stop("No connection provided.") } else { if(isTRUE(is.null(file))==FALSE) file = NULL  } 

#

## Labels
lb<-vector()
for(i in 1:length(unlist(x))) {
	lb<-append(lb, strsplit(unlist(x)[i],", ")[[1]])
}; rm(i)
lb<-levels(factor(lb))


## Asym
asym<-list()
k<-1
if(length(unlist(x[[k]]))>0) {
q<-1
for(i in 1:length(x[[k]])) {
	for(j in 1:length(x[[k]][[i]])) {
	if(isTRUE(length(x[[k]][[i]])!=0)==TRUE) {
#		print(paste("\\stackrel{\\rightarrow}",attr(x[[k]][i],'names'), " (",x[[k]][[i]][j],")", sep=""))
	if(latex) {
		if(isTRUE(is.null(attr(x[[k]],"names"))==FALSE)) {
		asym[[q]]<-noquote(paste("\\stackrel{\\rightarrow}{\\sf ",attr(x[[k]][i],'names'), "} (",x[[k]][[i]][j],")", sep=""))
		} else {
		asym[[q]]<-noquote(paste("\\stackrel{\\rightarrow}{",'R', "} (",x[[k]][[i]][j],")", sep=""))
		}
	} else {
		if(isTRUE(is.null(attr(x[[k]],"names"))==FALSE)) {
		asym[[q]]<-noquote(paste("->","{",attr(x[[k]][i],'names'), "} (",x[[k]][[i]][j],")", sep=""))
		} else {
		asym[[q]]<-noquote(paste("->","{",'R', "} (",x[[k]][[i]][j],")", sep=""))
		}
	}
	q<-q+1
	}
	}; rm(j)
}; rm(i)
}


## BUG solved! Recp2 <->{2} (0, 0) => un solo reciprocal...
## BUG 1D solved!
## Recp
recp<-list()
k<-2
if(length(unlist(x[[k]]))>0) {
q<-1
if(isTRUE(is.list(x[[k]]))==TRUE) {
for(i in 1:length(x[[k]])) {
	if(isTRUE(length(x[[k]][[i]])!=0)==TRUE) {
	tmp<-transf(x[[k]][[i]],'listmat',lb2lb=TRUE,labels=lb)
	tmp[lower.tri(tmp)]<-NA
	tmp[is.na(tmp)]<-0
	if(isTRUE(sum(tmp)==0)==FALSE) {
	tmp<-transf(tmp,'matlist',lb2lb=TRUE,labels=lb)
	for(j in 1:length(tmp)) {
#		print(paste("\\stackrel{\\leftrightarrow}",attr(x[[k]][i],'names'), " (",tmp[j],")", sep=""))
	if(latex) {
		if(isTRUE(is.null(attr(x[[k]],"names"))==FALSE)) {
		recp[[q]]<-noquote(paste("\\stackrel{\\leftrightarrow}{\\sf ",attr(x[[k]][i],'names'), "} (",tmp[j],")", sep=""))
		} else {
		recp[[q]]<-noquote(paste("\\stackrel{\\leftrightarrow}{",'R', "} (",tmp[j],")", sep=""))
		}
	} else {
		if(isTRUE(is.null(attr(x[[k]],"names"))==FALSE)) {
		recp[[q]]<-noquote(paste("<->","{",attr(x[[k]][i],'names'), "} (",tmp[j],")", sep=""))
		} else {
		recp[[q]]<-noquote(paste("<->","{",'R', "} (",tmp[j],")", sep=""))
		}
	}
	q<-q+1
	}; rm(j)
	}
	#;rm(tmp)
	}
}; rm(i)
} else if(isTRUE(is.list(x[[k]]))==FALSE) {
	tmp<-men(x[[k]])
	for(j in 1:length(tmp)) {
#		print(paste("\\stackrel{\\leftrightarrow}",attr(x[[k]],'names'), " (",tmp[j],")", sep=""))
	if(latex) {
		recp[[q]]<-noquote(paste("\\stackrel{\\leftrightarrow}{",'R', "} (",tmp[j],")", sep=""))
	} else {
		recp[[q]]<-noquote(paste("<->","{",'R', "} (",tmp[j],")", sep=""))
	}
	q<-q+1
	}; rm(j)
}
}


## Tent
tent<-list()
k<-3
if(length(unlist(x[[k]]))>0) {
q<-1
tmp<-vector()
for(l in 1:length(levels(factor(unlist(x[[k]]))))) {
	for(i in 1:length(x[[k]])) {
		if(isTRUE(levels(factor(unlist(x[[k]])))[l]%in%x[[k]][[i]])==TRUE) {
			if(latex) {
				tmp<-append(tmp, paste("\\stackrel{\\rightarrow}{\\sf ",attr(x[[k]][i],'names'),"}", sep="") )
			} else {
				tmp<-append(tmp, paste("->","{",attr(x[[k]][i],'names'),"}", sep="") )
			}
		}
	}; rm(i)
tmp<-append(tmp, paste(" (",levels(factor(unlist(x[[k]])))[l],")", sep="") )
#	print(tmp)
	tent[[q]]<-noquote(paste(tmp,collapse=" "))
	q<-q+1
tmp<-vector()
}; rm(l)
}

## Txch
txch<-list()
k<-4
if(length(unlist(x[[k]]))>0) {
q<-1
temp<-men(levels(factor(unlist(x[[k]]))))
tmp<-vector()
for(l in 1:length(temp)) {
	for(i in 1:length(x[[k]])) {
	if(isTRUE(length(x[[k]][[i]])!=0)==TRUE) {
		if(isTRUE(temp[l]%in%x[[k]][[i]])==TRUE) {
			if(latex) {
				tmp<-append(tmp, paste("\\stackrel{\\rightarrow}{\\sf ",attr(x[[k]][i],'names'),"}", sep="") )
			} else {
				tmp<-append(tmp, paste("->","{",attr(x[[k]][i],'names'),"}", sep="") )
			}
		}
		if(isTRUE(swp(temp[l])%in%x[[k]][[i]])==TRUE) {
			if(latex) {
				tmp<-append(tmp, paste("\\stackrel{\\leftarrow}{\\sf ",attr(x[[k]][i],'names'),"}", sep="") )
			} else {
				tmp<-append(tmp, paste("<-","{",attr(x[[k]][i],'names'),"}", sep="") )
			}
		}
	}
	}; rm(i)
tmp<-append(tmp, paste(" (",temp[l],")", sep="") )
#tmp<-append(tmp, paste(" (",levels(factor(unlist(x[[k]])))[l],")", sep="") )
#	print(tmp)
	txch[[q]]<-noquote(paste(tmp,collapse=" "))
	q<-q+1
tmp<-vector()
};rm(l)
}

## Mixd
mixd<-list()
k<-5
if(length(unlist(x[[k]]))>0) {
q<-1
temp<-men(levels(factor(unlist(x[[k]]))))
tmp<-vector()
for(l in 1:length(temp)) {
	for(i in 1:length(x[[k]])) {
	if(isTRUE(length(x[[k]][[i]])!=0)==TRUE) {
		if(isTRUE(temp[l]%in%x[[k]][[i]])==TRUE && isTRUE(swp(temp[l])%in%x[[k]][[i]])==TRUE) {
			if(latex) {
				tmp<-append(tmp, paste("\\stackrel{\\leftrightarrow}{\\sf ",attr(x[[k]][i],'names'),"}", sep="") )
			} else {
				tmp<-append(tmp, paste("<->","{",attr(x[[k]][i],'names'),"}", sep="") )
			}
		} else {
		if(isTRUE(temp[l]%in%x[[k]][[i]])==TRUE) {
			if(latex) {
				tmp<-append(tmp, paste("\\stackrel{\\rightarrow}{\\sf ",attr(x[[k]][i],'names'),"}", sep="") )
			} else {
				tmp<-append(tmp, paste("->","{",attr(x[[k]][i],'names'),"}", sep="") )
			}
		}
		if(isTRUE(swp(temp[l])%in%x[[k]][[i]])==TRUE) {
			if(latex) {
				tmp<-append(tmp, paste("\\stackrel{\\leftarrow}{\\sf ",attr(x[[k]][i],'names'),"}", sep="") )
			} else {
				tmp<-append(tmp, paste("<-","{",attr(x[[k]][i],'names'),"}", sep="") )
			}
		}
		}
	}
	}; rm(i)
tmp<-append(tmp, paste(" (",temp[l],")", sep="") )
#	print(tmp)
	mixd[[q]]<-noquote(paste(tmp,collapse=" "))
	q<-q+1
tmp<-vector()
};rm(l)
}

## OJO, NO TEST pero casi fija que es así por func 'men' cuando hay objeto 'temp'
## Full
full<-list()
k<-6
if(length(unlist(x[[k]]))>0) {
q<-1
temp<-men(levels(factor(unlist(x[[k]]))))
tmp<-vector()
for(l in 1:length(temp)) {
	for(i in 1:length(x[[k]])) {
	if(isTRUE(length(x[[k]][[i]])!=0)==TRUE) {
		if(isTRUE(temp[l]%in%x[[k]][[i]])==TRUE) {
			if(latex) {
				tmp<-append(tmp, paste("\\stackrel{","\\leftrightarrow}{\\sf ", attr(x[[k]][i],'names'),"}", sep="") )
			} else {
				tmp<-append(tmp, paste("<->","{", attr(x[[k]][i],'names'),"}", sep="") )
			}
		}
	}
	}; rm(i)
tmp<-append(tmp, paste(" (",temp[l],")", sep="") )
#tmp<-append(tmp, paste(" (",levels(factor(unlist(x[[k]])))[l],")", sep="") )
#	print(tmp)
	full[[q]]<-noquote(paste(tmp,collapse=" "))
	q<-q+1
tmp<-vector()
};rm(l)
}
	bndl<-list(Asym=asym, Recp=recp, Tent=tent, Txch=txch, Mixd=mixd, Full=full)
	Bundles<-unlist(bndl)

if(latex) {
##################
cat(paste("\\documentclass{article}", collapse="\n"), file=file, sep="\n", append=TRUE)
cat(paste("\\usepackage[landscape,a4paper]{geometry}", collapse="\n"), file=file, sep="\n", append=TRUE)
#cat(paste("\\geometry{left=1cm}", collapse="\n"), file=file, sep="\n", append=TRUE)
cat(paste("\\begin{document}", collapse="\n"), file=file, sep="\n", append=TRUE)
cat(paste("\\pagestyle{empty}", collapse="\n"), file=file, sep="\n", append=TRUE)
cat("", file=file, sep="\n", append=TRUE)
#
cat(paste("\\setlength{\\tabcolsep}{10pt}", collapse="\n"), file=file, sep="\n", append=TRUE)
#cat(paste("\\begin{table}[t]", collapse="\n"), file=file, sep="\n", append=TRUE)
cat(paste("\\begin{tabular}[t]{llllll}", collapse="\n"), file=file, sep="\n", append=TRUE)
cat("", file=file, sep="\n", append=TRUE)
#
cat(paste("\\begin{tabular}[t]{l}", collapse="\n"), file=file, sep="\n", append=TRUE)
cat(paste("\\normalsize{\\bf Asymmetric}:",length(bndl[[1]]),sep=" "), file=file, append=TRUE)
cat("\\\\", file=file, sep="\n", append=TRUE)
cat(paste("$",bndl[[1]],"$",sep="",collapse="  \\\\\n"),file=file, append=TRUE)
cat("  \\\\", file=file, sep="\n", append=TRUE)
cat(paste("\\end{tabular}", collapse="\n"), file=file, sep="\n", append=TRUE)
#
cat("&", file=file, sep="\n", append=TRUE)
#
cat(paste("\\begin{tabular}[t]{l}", collapse="\n"), file=file, sep="\n", append=TRUE)
cat(paste("\\normalsize{\\bf Reciprocal}:",length(bndl[[2]]),sep=" "), file=file, append=TRUE)
cat("\\\\", file=file, sep="\n", append=TRUE)
#cat("", file=file, sep="\n", append=TRUE)
cat(paste("$",bndl[[2]],"$",sep="",collapse="  \\\\\n"),file=file, append=TRUE)
cat("  \\\\", file=file, sep="\n", append=TRUE)
cat(paste("\\end{tabular}", collapse="\n"), file=file, sep="\n", append=TRUE)
#
cat("&", file=file, sep="\n", append=TRUE)
#
cat(paste("\\begin{tabular}[t]{l}", collapse="\n"), file=file, sep="\n", append=TRUE)
cat(paste("\\normalsize{\\bf Tie Entrainment}:",length(bndl[[3]]),sep=" "), file=file, append=TRUE)
cat("\\\\", file=file, sep="\n", append=TRUE)
#cat("", file=file, sep="\n", append=TRUE)
cat(paste("$",bndl[[3]],"$",sep="",collapse="  \\\\\n"),file=file, append=TRUE)
cat("  \\\\", file=file, sep="\n", append=TRUE)
cat(paste("\\end{tabular}", collapse="\n"), file=file, sep="\n", append=TRUE)
#
cat("&", file=file, sep="\n", append=TRUE)
#
cat(paste("\\begin{tabular}[t]{l}", collapse="\n"), file=file, sep="\n", append=TRUE)
cat(paste("\\normalsize{\\bf Tie Exchange}:",length(bndl[[4]]),sep=" "), file=file, append=TRUE)
cat("\\\\", file=file, sep="\n", append=TRUE)
#cat("", file=file, sep="\n", append=TRUE)
cat(paste("$",bndl[[4]],"$",sep="",collapse="  \\\\\n"),file=file, append=TRUE)
cat("  \\\\", file=file, sep="\n", append=TRUE)
cat(paste("\\end{tabular}", collapse="\n"), file=file, sep="\n", append=TRUE)
#
cat("&", file=file, sep="\n", append=TRUE)
#
cat(paste("\\begin{tabular}[t]{l}", collapse="\n"), file=file, sep="\n", append=TRUE)
cat(paste("\\normalsize{\\bf Mixed}:",length(bndl[[5]]),sep=" "), file=file, append=TRUE)
cat("\\\\", file=file, sep="\n", append=TRUE)
#cat("", file=file, sep="\n", append=TRUE)
cat(paste("$",bndl[[5]],"$",sep="",collapse="  \\\\\n"),file=file, append=TRUE)
cat("  \\\\", file=file, sep="\n", append=TRUE)
cat(paste("\\end{tabular}", collapse="\n"), file=file, sep="\n", append=TRUE)
#
cat("&", file=file, sep="\n", append=TRUE)
#
cat(paste("\\begin{tabular}[t]{l}", collapse="\n"), file=file, sep="\n", append=TRUE)
cat(paste("\\normalsize{\\bf Full}:",length(bndl[[6]]),sep=" "), file=file, append=TRUE)
cat("\\\\", file=file, sep="\n", append=TRUE)
#cat("", file=file, sep="\n", append=TRUE)
cat(paste("$",bndl[[6]],"$",sep="",collapse="  \\\\\n"),file=file, append=TRUE)
cat("  \\\\", file=file, sep="\n", append=TRUE)
cat(paste("\\end{tabular}", collapse="\n"), file=file, sep="\n", append=TRUE)
##
cat("", file=file, sep="\n", append=TRUE)
cat(paste("\\end{tabular}", collapse="\n"), file=file, sep="\n", append=TRUE)
#cat(paste("\\end{table}[t]", collapse="\n"), file=file, sep="\n", append=TRUE)
#
cat("", file=file, sep="\n", append=TRUE)
cat(paste("\\end{document}", collapse="\n"), file=file, sep="\n", append=TRUE)
##
##################
#
} else {
	return(as.data.frame(Bundles))

}

}
