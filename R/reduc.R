reduc <-
function(x, clu, labels = NULL) 
{
    lngt <- nlevels(factor(clu))
    cls <- list()
    for (i in 1:lngt) {
        cls[[i]] <- which(clu == i)
    }
    rm(i)

#if(isTRUE(match.arg(type) == "M") == TRUE) {
    if(isTRUE(is.na(dim(x)[3])==TRUE)) {
    if (isTRUE(is.null(dimnames(x)[[1]]) == TRUE) == TRUE) 
        dimnames(x)[[1]] <- dimnames(x)[[2]] <- 1:nrow(x)

    bm <- array(dim = c(lngt, lngt))
    for (i in 1:lngt) {
        for (j in 1:lngt) {
                bm[i, j] <- sum(x[cls[[i]], cls[[j]]])
            }
    }
    rm(i, j)
    bm <- dichot(bm)
    ifelse(is.null(labels) == FALSE, rownames(bm) <- colnames(bm) <- labels, 
        NA)
    return(bm)
} else if(isTRUE(is.na(dim(x)[3])==FALSE)) {
#
px<-x
#
bm<-array(dim=c(lngt,lngt,dim(x)[3]))
for (k in 1:dim(x)[3]) {
for (i in 1:lngt) {
for (j in 1:lngt) {

   bm[i,j,k]<-sum(px[cls[[i]],cls[[j]],k]) 

}
}; rm(i,j)
}; rm(k)
#
bm<-dichot(bm)

if(is.null(labels)==FALSE) dimnames(bm)[[1]] <- dimnames(bm)[[2]] <- labels
if(is.null(dimnames(x)[[3]])==FALSE) dimnames(bm)[[3]] <- dimnames(x)[[3]]

    return(bm)

  }
#
#} else if (isTRUE(match.arg(type) == "S") == TRUE) {
##
#    if(isTRUE(attr(x, "class")[1]=="Rel.S")==FALSE) {
#        stop("\"x\" should be an object of a \"Rel.S\" class.")
#    }
#tmp<-x
#lb<-x$st
#
#ifelse(isTRUE(attr(tmp,"class")[2]=="symbolic")==TRUE, x<-convert(tmp), x<-tmp$S )
#
#px<-perm(x,clu)
#tab<-tabulate(clu)
#
#bm <- array(dim = c(lngt, lngt))
#y<-h<-j<-0
#for (i in 1:lngt) {
#k<-((y+1):(y+tabulate(clu)[i]))
#for (j in 1:lngt) {
##bm[i,j] <- min(px[k,(h+1):(h+tabulate(clu)[j])])
### ESTO ASEGURA CONGRUENCIA!!!
#for (q in 1:length(cls)) {
#if(all(unique(as.numeric(levels(factor(as.matrix(x[which(clu == i), which(clu == j)]))))) %in% cls[[q]]) == TRUE) {
### ESTO NO DETERMINA CONGRUENCIA, SOLO ASIGNA EL MENOR!!!
#	if(isTRUE(attr(tmp, "class")[2]=="symbolic")==TRUE) { 
#		bm[i, j] <- lb[min(as.numeric(as.matrix(px[k,(h+1):(h+tabulate(clu)[j])])))]
#	} else if(isTRUE(attr(tmp, "class")[2]=="numerical")==TRUE) {
#		bm[i, j] <- min(as.numeric(as.matrix(px[k,(h+1):(h+tabulate(clu)[j])])))
#	}
#}
#}
##
#if(isTRUE(j>0)==TRUE) h<-sum(tab[1:j])
#}; rm(j)
#h<-0 # no cambiar!
#y<-sum(tab[1:i])
#}; rm(i)
##bm
#
#
#if(is.null(labels)==FALSE) { 
#   lbs <- labels
##
#} else if(is.null(labels)==TRUE) { 
##
#lbs<-vector()
#for(i in 1:length(tabulate(clu))) {
#    ifelse(isTRUE(attr(tmp,"class")[2]=="symbolic")==TRUE, 
#       lbs[length(lbs)+1] <- lb[which(clu==i)[1]],
#       lbs[length(lbs)+1] <- as.numeric(dimnames(x)[[1]])[which(clu==i)[1]]
#    )
#}; rm(i)
##lbs
#}
#
### equations
##imm <- as.matrix(bm)
##lb <- lbs
##
#for(i in 1:dim(bm)[1]) {
#for(j in 1:dim(bm)[1]) {
################################   NUMERICAL ONLY...
#if(isTRUE(bm[i,j]%in%lbs)==FALSE) {
# for(l in 1:length(lbs)) {
#    if(isTRUE(attr(tmp, "class")[2]=="symbolic")==TRUE) {
#        if(isTRUE(bm[i,j]%in%lbs[l])==TRUE) bm[i,j] <- lbs[l]   #### NO FUNCIONA SYMBOLIC!!!!!!!
#    } else if(isTRUE(attr(tmp, "class")[2]=="numerical")==TRUE) {
#        if(isTRUE(clu[bm[i,j]]%in%as.numeric(lbs)[l])==TRUE) bm[i,j] <- l  
#    }
# }; rm(l)
#}
##
#};rm(j)
#};rm(i)
##
##bm <- as.data.frame(imm)
##
#
#   dimnames(bm)[[1]] <- dimnames(bm)[[2]] <- as.list(lbs)
#
#    return(as.data.frame(bm))
#
#}

}
