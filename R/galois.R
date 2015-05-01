galois <-
function(x, labeling = c("full", "reduced")) {

ifelse(isTRUE(is.data.frame(x))==TRUE, NA, x <- as.data.frame(x))

## 1) ATTR EXTENTS TO A LIST
conj<-list()
length(conj)<-ncol(x)
for(i in 1:ncol(x)) {
conj[[i]] <- jnt(rownames(x)[which(x[,i]!=0)])
};rm(i)
attr(conj,"names") <- colnames(x)

## 1a) OBJ INTENTS TO A LIST
conj1<-list()
length(conj1)<-nrow(x)
for(i in 1:nrow(x)) {
conj1[[i]] <- jnt(colnames(x)[which(x[i,]!=0)])
};rm(i)
attr(conj1,"names") <- rownames(x)


### 2) PAIRWISE INTERSECTIONS
conj2 <- list()
conj2n <- vector()
if(isTRUE(length(conj)>1)==TRUE) {
for(k in 2:length(conj)) {
for(i in k:length(conj)) {
if(isTRUE(length(intersect(dhc(conj[[k-1]]),dhc(conj[[i]])))>1)==TRUE) {
conj2[[length(conj2)+1]] <- jnt(intersect(dhc(conj[[k-1]]),dhc(conj[[i]])))
} else {
conj2[[length(conj2)+1]] <- intersect(dhc(conj[[k-1]]),dhc(conj[[i]]))
}
conj2n[length(conj2n)+1] <- paste(attr(conj,'names')[k-1], attr(conj,'names')[i], sep=", ")
};rm(i)
};rm(k)
attr(conj2, "names") <- conj2n
} else {
conj2 <- conj
}

### 3) COMPUTE INTENTS
if(isTRUE(length(which(conj2%in%conj==FALSE))!=0)==TRUE) {
if(isTRUE(jnt(rownames(x))%in%c(conj,conj2))==TRUE ) {
conj3 <- list(conj,unique(conj2[which(conj2%in%conj==FALSE)]))
} else {
conj3 <- list(conj,unique(conj2[which(conj2%in%conj==FALSE)]),jnt(rownames(x)))
}
} else { 
conj3 <- list(conj,jnt(rownames(x)))
}


## labelling 1
for(i in 1:length(conj)) {
if(isTRUE(length(which(conj2%in%conj[[i]]==TRUE))!=0)==TRUE) {
	attr(conj3[[1]],"names")[i] <- jnt(attr(conj2,"names")[which(conj2%in%conj[[i]])])
} else { NA }
};rm(i)

## labelling 2
for(i in 1:length(conj3[[2]])) {
if(isTRUE(length(attr(conj2,"names")[which(conj3[[2]][[i]]==conj2)])!=0)==TRUE) {
	if(isTRUE(length(dhc(conj3[[2]][[i]]))==1)==TRUE) {
	attr(conj3[[2]],"names")[i] <- conj1[[which(conj3[[2]][[i]]==attr(conj1,'names'))]]
	} else if(isTRUE(length(dhc(conj3[[2]][[i]]))==1)==FALSE) {
	attr(conj3[[2]],"names")[i] <- jnt(attr(conj2,"names")[which(conj3[[2]][[i]]==conj2)])
	}
} else {
attr(conj3[[2]],"names")[i] <- jnt(attr(conj2,"names")[which(conj2%in%levels(factor(unlist(conj2))))])
}
}; rm(i)

# list the intents
if(isTRUE(jnt(rownames(x))%in%c(conj,conj2))==FALSE ) {
ifelse(isTRUE(length(conj3)>2)==TRUE, con <- c(conj3[[1]],conj3[[2]],conj3[[3]]), con <- c(conj3[[1]],conj3[[2]])) 
} else {
con <- c(conj3[[1]],conj3[[2]]) 
}

# DUPLICATES
der <- unique(con)
attr(der,"names") <- unique(attr(con,"names"))

# PO
po <- matrix(0, nrow=length(der), ncol=length(der))
for (j in 1:length(der)) {
    for (i in 1:length(der)) {
	ifelse(isTRUE(all(dhc(der[[i]]) %in% dhc(der[[j]])))==TRUE, po[i,j]<-1, NA)
    }
}; rm(i, j)


# LABEL REDUCTION !!!
##if(match.arg(labeling) == "reduced") {
#
ints <- attr(der,"names")
exts <- der
##

if(isTRUE(length(der)>2)==TRUE) {
##
for(k in 2:length(der)) {
for(i in k:length(der)) { #
#
if(isTRUE((k-1)==i)==FALSE) {
# MUTUAL exclusion?
if(isTRUE(any(isTRUE(all(po[,i]-po[,(k-1)]!=-1))==TRUE | isTRUE(all(po[,(k-1)]-po[,i]!=-1))==TRUE))==TRUE) {
# si hay inclusion de 'k-1' en 'i'...
if(isTRUE(all(po[,i]-po[,(k-1)]!=-1))==TRUE) {
	# intents
	if(isTRUE(ints[(k-1)]=="")==FALSE) {
	ints[(k-1)] <- jnt(dhc(ints[(k-1)])[which(!(dhc(ints[(k-1)])%in%dhc(ints[i])))])
	} else { NA }
	# extents
	if(isTRUE(length(exts[[i]])==0)==FALSE) {
	exts[[i]] <- jnt(dhc(exts[[i]])[which(!(dhc(exts[[i]])%in%dhc(exts[[(k-1)]])))])
	} else { NA }
# entonces hay inclusion de 'i' en 'k-1'...
} else if(isTRUE(all(po[,i]-po[,(k-1)]!=-1))==FALSE) { 
	# intents
	if(isTRUE(all.equal(dhc(ints[i]),dhc(ints[(k-1)])))==TRUE) {
	ints[i] <- ""
	} else if(isTRUE(all.equal(dhc(ints[i]),dhc(ints[(k-1)])))==FALSE) {
	ifelse(isTRUE(length(jnt(dhc(ints[i])[which(!(dhc(ints[i])%in%dhc(ints[(k-1)])))]))==0)==TRUE, NA, ints[i] <- jnt(dhc(ints[i])[which(!(dhc(ints[i])%in%dhc(ints[(k-1)])))]))
	}
	# extents
	if(isTRUE(all.equal(dhc(exts[[(k-1)]]),dhc(exts[[i]])))==TRUE) {
	exts[[(k-1)]] <- ""
	} else if(isTRUE(all.equal(dhc(exts[[(k-1)]]),dhc(exts[[i]])))==FALSE) {
	exts[[(k-1)]] <- jnt(dhc(exts[[(k-1)]])[which(!(dhc(exts[[(k-1)]])%in%dhc(exts[[i]])))])
	}
}
#
} else { NA } # then mutual exclusion
} else { NA } # then same object
#
};rm(i)
};rm(k)
##

##
} else if(isTRUE(length(der)==2)==TRUE)  { ## one attribute

# MUTUAL exclusion?
if(isTRUE(any(isTRUE(all(po[,2]-po[,1]!=-1))==TRUE | isTRUE(all(po[,1]-po[,2]!=-1))==TRUE))==TRUE) {
# si hay inclusion de '1' en '2'...
if(isTRUE(all(po[,2]-po[,1]!=-1))==TRUE) {
	# intents
	if(isTRUE(is.na(ints[2]))==FALSE) {
	ints[2] <- jnt(dhc(ints[1])[which(!(dhc(ints[1])%in%dhc(ints[2])))])
	} else { NA }
	# extents
	if(isTRUE(length(exts[[2]])==0)==FALSE) {
	exts[[2]] <- jnt(dhc(exts[[2]])[which(!(dhc(exts[[2]])%in%dhc(exts[[1]])))])
	} else { NA }
# entonces hay inclusion de '2' en '1'...
} else if(isTRUE(all(po[,2]-po[,1]!=-1))==FALSE) { 
	# intents
	if(isTRUE(all.equal(dhc(ints[2]),dhc(ints[1])))==TRUE) {
	ints[2] <- ""
	} else if(isTRUE(all.equal(dhc(ints[2]),dhc(ints[1])))==FALSE) {
	ifelse(isTRUE(length(jnt(dhc(ints[2])[which(!(dhc(ints[2])%in%dhc(ints[1])))]))==0)==TRUE, NA, ints[2] <- jnt(dhc(ints[2])[which(!(dhc(ints[2])%in%dhc(ints[1])))]))
	}
	# extents
	if(isTRUE(all.equal(dhc(exts[[1]]),dhc(exts[[2]])))==TRUE) {
	exts[[1]] <- ""
	} else if(isTRUE(all.equal(dhc(exts[[1]]),dhc(exts[[2]])))==FALSE) {
	exts[[1]] <- jnt(dhc(exts[[1]])[which(!(dhc(exts[[1]])%in%dhc(exts[[2]])))])
	}
}
#
} else { NA } # then mutual exclusion

}  else { NA } #ZERO ATTRIBUTES?
##


## CHECK DUPLICATES IN MUTUALLY EXCLUSIVE CONCEPTS...
dupl <- levels(factor(unlist(dhc(exts))[which(duplicated(unlist(dhc(exts)))==TRUE)]))
#switch(match.arg(labeling), full = dupl<-levels(factor(unlist(dhc(der))[which(duplicated(unlist(dhc(der)))==TRUE)])), reduced = dupl<-levels(factor(unlist(dhc(exts))[which(duplicated(unlist(dhc(exts)))==TRUE)])) )

dder <- der

if(isTRUE(length(dupl)>0)==TRUE) {
#
for(i in 1:length(dupl)) dder[[(length(dder)+1)]] <- dupl[i]

for(i in 1:length(dupl)) {
for(j in 1:nrow(x)) { #which((rownames(x)%in%dupl)))
  if(isTRUE(any(isTRUE(all(x[j,]-x[which(rownames(x)==dupl[i]),]!=-1))==TRUE & isTRUE(all(x[which(rownames(x)==dupl[i]),]-x[j,]!=-1))==FALSE))==TRUE) {
	dder[[(length(der)+i)]] <-  jnt(append(dder[[(length(der)+i)]],rownames(x)[j]))
  } else {
	NA }
};rm(j)
};rm(i)

dpo <- matrix(0, nrow=length(dder), ncol=length(dder))
for (j in 1:length(dder)) {
    for (i in 1:length(dder)) {
	ifelse(isTRUE(all(dhc(dder[[i]]) %in% dhc(dder[[j]])))==TRUE, dpo[i,j]<-1, NA)
    }
}; rm(i, j)

ints <- attr(dder,"names")
exts <- dder

##
for(k in 2:length(dder)) {
for(i in k:length(dder)) { #
if(isTRUE((k-1)==i)==FALSE) {
# MUTUAL exclusion?
if(isTRUE(any(isTRUE(all(dpo[,i]-dpo[,(k-1)]!=-1))==TRUE | isTRUE(all(dpo[,(k-1)]-dpo[,i]!=-1))==TRUE))==TRUE) {
# si hay inclusion de 'k-1' en 'i'...
if(isTRUE(all(dpo[,i]-dpo[,(k-1)]!=-1))==TRUE) {
	# intents
	if(isTRUE(ints[(k-1)]=="")==FALSE) {
	ints[(k-1)] <- jnt(dhc(ints[(k-1)])[which(!(dhc(ints[(k-1)])%in%dhc(ints[i])))])
	} else { NA }
	# extents
	if(isTRUE(length(exts[[i]])==0)==FALSE) {
	exts[[i]] <- jnt(dhc(exts[[i]])[which(!(dhc(exts[[i]])%in%dhc(exts[[(k-1)]])))])
	} else { NA }
# entonces hay inclusion de 'i' en 'k-1'...
} else if(isTRUE(all(dpo[,i]-dpo[,(k-1)]!=-1))==FALSE) { 
	# intents
	if(isTRUE(all.equal(dhc(ints[i]),dhc(ints[(k-1)])))==TRUE) {
	ints[i] <- ""
	} else if(isTRUE(all.equal(dhc(ints[i]),dhc(ints[(k-1)])))==FALSE) {
	ifelse(isTRUE(length(jnt(dhc(ints[i])[which(!(dhc(ints[i])%in%dhc(ints[(k-1)])))]))==0)==TRUE, NA, ints[i] <- jnt(dhc(ints[i])[which(!(dhc(ints[i])%in%dhc(ints[(k-1)])))]))
	}
	# extents
	if(isTRUE(all.equal(dhc(exts[[(k-1)]]),dhc(exts[[i]])))==TRUE) {
	exts[[(k-1)]] <- ""
	} else if(isTRUE(all.equal(dhc(exts[[(k-1)]]),dhc(exts[[i]])))==FALSE) {
	exts[[(k-1)]] <- jnt(dhc(exts[[(k-1)]])[which(!(dhc(exts[[(k-1)]])%in%dhc(exts[[i]])))])
	}
}
} else { NA }# then mutual exclusion
} else { NA } # then same object
};rm(i)
};rm(k)
##

} else if(isTRUE(length(dupl)>0)==FALSE) {
  NA  
}

#
##}

attr(dder,"names")[which(is.na(attr(dder,"names"))==TRUE)] <- ""

class(dder) <- c("Galois", "full")


derr <- exts
attr(derr,"names") <- ints
#
attr(derr,"names")[which(is.na(attr(derr,"names"))==TRUE)] <- ""

#
lst <- (red = redl(dder, derr))
#
class(lst) <- c("Galois", "reduced")



## RETURN
switch(match.arg(labeling),
full = dder,
reduced = lst )

}
