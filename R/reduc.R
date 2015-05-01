reduc <-
function (x, clu, labels = NULL) 
{
    lngt <- nlevels(factor(clu))
    cls <- list()
    for (i in 1:lngt) {
        cls[[i]] <- which(clu == i)
    }
    rm(i)
    if (isTRUE(is.na(dim(x)[3]) == TRUE)) {
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
    }
    else if (isTRUE(is.na(dim(x)[3]) == FALSE)) {
        px <- x
        bm <- array(dim = c(lngt, lngt, dim(x)[3]))
        for (k in 1:dim(x)[3]) {
            for (i in 1:lngt) {
                for (j in 1:lngt) {
                  bm[i, j, k] <- sum(px[cls[[i]], cls[[j]], k])
                }
            }
            rm(i, j)
        }
        rm(k)
        bm <- dichot(bm)
        if (is.null(labels) == FALSE) 
            dimnames(bm)[[1]] <- dimnames(bm)[[2]] <- labels
        if (is.null(dimnames(x)[[3]]) == FALSE) 
            dimnames(bm)[[3]] <- dimnames(x)[[3]]
        return(bm)
    }
}
