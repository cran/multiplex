po.levels <-
function(x, perm = FALSE) {

if (require("Rgraphviz", quietly = TRUE, warn.conflicts = FALSE)) {

pictex()

po <- x & (1 - t(x))
diag(po) <- 0
for (i in seq_len(ncol(po))) {
    tmp <- outer(po[, i], po[i, ], pmin.int)
    po <- pmin(po, (1 - tmp))
}

X <- plot(as(po, "graphNEL"))

alt <- vector()
nam <- vector()
for(i in 1: length(X@AgNode)) {
	alt[length(alt)+1] <- X@AgNode[[i]]@center@y
	nam[length(nam)+1] <- X@AgNode[[i]]@name
}; rm(i)


cls <-(rbind(nam,rep(0,length(nam))))
#
for(i in 1:nlevels(factor(alt))) cls[2,][which(alt==levels(factor(alt))[i])] <- i

ord <-vector()
for(i in 1: length(X@AgEdge)) 	ord <- append(ord, c(X@AgEdge[[i]]@head, X@AgEdge[[i]]@tail) )

cls[2,][which(!(cls[1,]%in%(unique(ord))))] <- as.numeric(max(levels(factor(cls[2,]))))+1

#
attr(cls, "dimnames") <- NULL
#rownames(cls) <- c('node','class')
colnames(cls) <- cls[2,]
cls <- as.data.frame(cls)

dev.off()
unlink("Rplots.tex")

if(perm) {
clu <- as.numeric(as.vector(unlist(cls[2,])))
	lst <- list(cls=cls[1,], clu=clu, perm=perm(x, clu=clu))
lst
} else {
	return(cls[1,])
}

} else {	stop("Package 'Rgraphviz' needs to be properly installed.")  }

}
