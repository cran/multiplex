signed <-
function (P, N = NULL, labels = NULL) 
{
    if (isTRUE(is.null(N) == TRUE) == FALSE) {
        if (isTRUE(is.na(dim(N)[3]) == TRUE) == FALSE) {
            N <- N[, , 1]
            warning("Three dimensional array in 'N' not supported. Take the 1st dim. only.")
        }
        else {
            if (isTRUE("-1" %in% levels(factor(as.matrix(N))) == 
                TRUE) == TRUE) 
                stop("Signed array must be placed in 'P'")
        }
    }
    if (isTRUE(is.na(dim(P)[3]) == TRUE) == TRUE) {
        if (isTRUE(is.null(N) == TRUE) == FALSE) {
            ifelse(isTRUE(length(which(P + N > 1)) > 0) == TRUE, 
                ambiv <- TRUE, ambiv <- FALSE)
        }
        else if (isTRUE(is.null(N) == TRUE) == TRUE) {
            ifelse(isTRUE(("-1" %in% P) == TRUE || ("1" %in% 
                P) == TRUE) == TRUE, ambiv <- FALSE, ambiv <- TRUE)
            ifelse(isTRUE((sum(P) == 0) == TRUE) == TRUE, ambiv <- FALSE, 
                NA)
            N <- P
            N[which(N == 1)] <- 0
            N[which(N == 0)] <- 0
            N[which(N == -1)] <- 1
            P[which(P == 1)] <- 1
            P[which(P == 0)] <- 0
            P[which(P == -1)] <- 0
        }
    }
    else if (isTRUE(is.na(dim(P)[3]) == TRUE) == FALSE) {
        if (isTRUE(is.null(N) == TRUE) == TRUE) {
            ifelse(isTRUE(length(which(P[, , 1] + P[, , 2] > 
                1)) > 0) == TRUE, ambiv <- TRUE, ambiv <- FALSE)
            N <- P[, , 2]
            P <- P[, , 1]
        }
        else if (isTRUE(is.null(N) == TRUE) == FALSE) {
            P <- P[, , 1]
            ifelse(isTRUE(length(which(P + N > 1)) > 0) == TRUE, 
                ambiv <- TRUE, ambiv <- FALSE)
        }
    }
    sn <- array(dim = c(dim(P)[1], dim(N)[2], 2))
    if (isTRUE(is.null(labels) == TRUE) == TRUE) {
        if (is.null(dimnames(P)) == FALSE) {
            labels <- dimnames(P)[[1]]
        }
        else if (is.null(dimnames(N)) == FALSE) {
            labels <- dimnames(N)[[1]]
        }
        else {
            labels <- 1:dim(P)[1]
        }
    }
    sn[, , 1] <- P
    sn[, , 2] <- N
    rownames(sn) <- colnames(sn) <- labels
    dimnames(sn)[[3]] <- c("P", "N")
    if (ambiv) {
        bnd <- bundles(sn, collapse = FALSE)
        pos <- c(bnd$asym[[1]], bnd$recp[[1]], bnd$txch[[1]], 
            (bnd$mixed[[1]][which(!(bnd$mixed[[1]] %in% bnd$mixed[[2]]))]))
        neg <- c(bnd$asym[[2]], bnd$recp[[2]], bnd$txch[[2]], 
            (bnd$mixed[[1]][which(!(bnd$mixed[[2]] %in% bnd$mixed[[1]]))]))
        amb <- as.vector(c(unlist(bnd$tent), (bnd$mixed[[1]][which(bnd$mixed[[1]] == 
            bnd$mixed[[2]])]), unlist(bnd$full)))
        mat <- matrix("o", ncol = ncol(P), nrow = nrow(P))
        rownames(mat) <- colnames(mat) <- labels
        if (length(pos) != 0) 
            for (i in 1:length(pos)) mat[which(strsplit(pos[i], 
                ", ")[[1]][1] == labels), which(strsplit(pos[i], 
                ", ")[[1]][2] == labels)] <- "p"
        if (length(neg) != 0) 
            for (i in 1:length(neg)) mat[which(strsplit(neg[i], 
                ", ")[[1]][1] == labels), which(strsplit(neg[i], 
                ", ")[[1]][2] == labels)] <- "n"
        if (length(amb) != 0) 
            for (i in 1:length(amb)) mat[which(strsplit(amb[i], 
                ", ")[[1]][1] == labels), which(strsplit(amb[i], 
                ", ")[[1]][2] == labels)] <- "a"
        mat[which(diag(P) == 1)] <- "p"
        mat[which(diag(N) == 1)] <- "n"
    }
    else {
        mat <- matrix(0, ncol = ncol(P), nrow = nrow(P))
        rownames(mat) <- colnames(mat) <- labels
        if (isTRUE(is.null(N) == TRUE) == TRUE) {
            mat <- P
        }
        else if (isTRUE(is.null(N) == TRUE) == FALSE) {
            mat[which(P == 1)] <- 1
            mat[which(N == 1)] <- -1
        }
    }
    val <- noquote(levels(factor(mat)))
    val <- levels(reorder(val, length(val):1))
    lst <- list(val = val, s = noquote(mat))
    class(lst) <- "Signed"
    return(lst)
}
