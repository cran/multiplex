perm <-
function (x, clu) 
{
    or <- list()
    for (i in 1:nlevels(factor(clu))) {
        or[[i]] <- which(clu == i)
    }
    rm(i)
    nor <- order(unlist(or))
    if (isTRUE(is.na(dim(x)[3]) == TRUE)) {
        prm <- vector()
        for (i in 1:length(nor)) {
            prm[nor[i]] <- i
        }
        return(x[prm, prm])
    }
    else {
        px <- x
        for (k in 1:dim(x)[3]) {
            prm <- vector()
            for (i in 1:length(nor)) {
                prm[nor[i]] <- i
            }
            px[, , k] <- x[prm, prm, k]
        }
        rm(k)
        if (isTRUE(is.null(dimnames(x)[[1]]) == FALSE)) {
            lbs <- vector()
            length(lbs) <- length(clu)
            for (i in 1:length(nor)) lbs[i] <- dimnames(x)[[1]][which(nor == 
                i)]
            dimnames(px)[[1]] <- dimnames(px)[[2]] <- lbs
        }
        ifelse(isTRUE(is.null(dimnames(x)[[3]]) == FALSE), dimnames(px)[[3]] <- dimnames(x)[[3]], 
            NA)
        return(px)
    }
}
