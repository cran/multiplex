read.gml <-
function (file, as = c("srt", "array"), directed = TRUE) 
{
    arx <- scan(file, what = "character", nlines = -1, quiet = TRUE)
    nod <- arx[which(("node" == arx) == TRUE)[1]:length(arx)]
    n <- length(which(("node" == arx) == TRUE))
    lb <- vector()
    id <- vector()
    for (i in 1:n) {
        id[length(id) + 1L] <- nod[which(("id" == nod) == TRUE)[i] + 
            1L]
        if (isTRUE("label" %in% nod) == TRUE) {
            lb[length(lb) + 1L] <- nod[which(("id" == nod) == 
                TRUE)[i] + 3L]
        }
    }
    rm(i)
    if (isTRUE("label" %in% nod) == FALSE) {
        lb <- id
    }
    edg <- arx[which(("edge" == arx) == TRUE)[1]:length(arx)]
    ed <- length(which(("edge" == edg) == TRUE))
    sr <- vector()
    tg <- vector()
    for (i in 1:ed) {
        sr[length(sr) + 1L] <- edg[which(("source" == edg) == 
            TRUE)[i] + 1L]
        tg[length(tg) + 1L] <- edg[which(("target" == edg) == 
            TRUE)[i] + 1L]
    }
    rm(i)
    if (isTRUE("0" %in% id) == TRUE & isTRUE(all(as.numeric(id) + 
        1L == seq_along(id))) == TRUE) {
        sr <- as.numeric(sr) + 1L
        tg <- as.numeric(tg) + 1L
    }
    pr <- vector()
    for (i in 1:length(sr)) pr[length(pr) + 1L] <- paste(sr[i], 
        tg[i], sep = ", ")
    z <- max(tabulate(factor(pr)))
    ledg <- length(grep("graphics", edg, fixed = TRUE, value = TRUE))
    nls <- nlevels(factor(edg[grep("style", edg, fixed = TRUE) + 
        1L]))
    nlf <- nlevels(factor(edg[grep("fill", edg, fixed = TRUE) + 
        1L]))
    st <- vector()
    if (isTRUE(ledg == 0L) == FALSE) {
        for (i in 2:ledg) {
            tmp <- edg[(grep("graphics", edg, fixed = TRUE)[(i - 
                1)] + 2L):(grep("graphics", edg, fixed = TRUE)[i] - 
                7L)]
            if (isTRUE(nls > 1L) == TRUE) {
                ifelse(isTRUE(length(tmp[grep("style", tmp, fixed = TRUE) + 
                  1L]) == 0L) == FALSE, st[length(st) + 1L] <- tmp[grep("style", 
                  tmp, fixed = TRUE) + 1L], st[length(st) + 1L] <- "default")
            }
            else if (isTRUE(nlf > 1L) == TRUE) {
                ifelse(isTRUE(length(tmp[grep("fill", tmp, fixed = TRUE) + 
                  1L]) == 0L) == FALSE, st[length(st) + 1L] <- tmp[grep("fill", 
                  tmp, fixed = TRUE) + 1L], st[length(st) + 1L] <- "#000000")
            }
        }
        rm(i)
        tmp <- edg[utils::tail(grep("graphics", edg, fixed = TRUE), 
            1):length(edg)]
        if (isTRUE(nls > 1L) == TRUE) {
            ifelse(isTRUE(length(tmp[grep("style", tmp, fixed = TRUE) + 
                1L]) == 0L) == FALSE, st[length(st) + 1L] <- tmp[grep("style", 
                tmp, fixed = TRUE) + 1L], st[length(st) + 1L] <- "default")
        }
        else if (isTRUE(nlf > 1L) == TRUE) {
            ifelse(isTRUE(length(tmp[grep("fill", tmp, fixed = TRUE) + 
                1L]) == 0L) == FALSE, st[length(st) + 1L] <- tmp[grep("fill", 
                tmp, fixed = TRUE) + 1L], st[length(st) + 1L] <- "#000000")
        }
        ndf <- cbind(sr, tg, st)
    }
    else {
        ndf <- cbind(sr, tg)
    }
    if (isTRUE(length(st) == 0L) == TRUE) {
        st <- rep("1", nrow(ndf))
        ifelse(isTRUE(z == 2L) == TRUE, st[which(duplicated(ndf))] <- "2", 
            NA)
        ndf <- cbind(ndf, st)
    }
    nlst <- nlevels(factor(st))
    if (match.arg(as) == "array") {
        if (isTRUE(z > 1L) == TRUE) {
            mat <- array(0L, dim = c(n, n, z))
            if (isTRUE(nlst > 1L) == TRUE) {
                for (k in 1:nlst) {
                  mdf <- subset(ndf, ndf[, 3] == levels(factor(st))[k])
                  pr <- vector()
                  for (i in 1:nrow(mdf)) {
                    if (isTRUE("0" %in% id) == FALSE) {
                      pr[length(pr) + 1L] <- paste(match(mdf[i, 
                        1], id), match(mdf[i, 2], id), sep = ", ")
                    }
                    else if (isTRUE("0" %in% id) == TRUE && isTRUE(all(as.numeric(id) + 
                      1L == seq_along(id))) == TRUE) {
                      pr[length(pr) + 1L] <- paste(match(as.numeric(mdf[i, 
                        1]), as.numeric(id) + 1L), match(as.numeric(mdf[i, 
                        2]), as.numeric(id) + 1L), sep = ", ")
                    }
                    else if (isTRUE("0" %in% id) == TRUE) {
                      pr[length(pr) + 1L] <- paste(match(as.numeric(mdf[i, 
                        1]), as.numeric(id)), match(as.numeric(mdf[i, 
                        2]), as.numeric(id)), sep = ", ")
                    }
                  }
                  rm(i)
                  mat[, , k] <- transf(pr, "listmat", ord = n, 
                    labels = c(1:n))
                }
                rm(k)
            }
        }
        if (isTRUE(ncol(ndf) == 2L) == TRUE | isTRUE(nlst == 
            1L) == TRUE) {
            mat <- array(0L, dim = c(n, n, 1L))
            mat[, , 1] <- dichot(transf(pr, "listmat", ord = n, 
                labels = c(1:n)))
        }
        if (isTRUE(directed == FALSE) == TRUE) {
            for (i in 1:z) {
                mat[, , i] <- (mat[, , i] + t(mat[, , i]))
            }
            rm(i)
        }
        dimnames(mat)[[1]] <- dimnames(mat)[[2]] <- lb
        return(mat)
    }
    else if (match.arg(as) == "srt") {
        if (isTRUE("0" %in% id) == FALSE) {
            srt <- as.data.frame(cbind(lb[match(ndf[, 1], id)], 
                lb[match(ndf[, 2], id)]))
        }
        else if (isTRUE("0" %in% id) == TRUE && isTRUE(all(as.numeric(id) + 
            1L == seq_along(id))) == TRUE) {
            srt <- as.data.frame(cbind(lb[match(as.numeric(ndf[, 
                1]), as.numeric(id) + 1L)], lb[match(as.numeric(ndf[, 
                2]), as.numeric(id) + 1L)]))
        }
        else if (isTRUE("0" %in% id) == TRUE) {
            srt <- as.data.frame(cbind(lb[match(as.numeric(ndf[, 
                1]), as.numeric(id))], lb[match(as.numeric(ndf[, 
                2]), as.numeric(id))]))
        }
        if (isTRUE(z > 1L) == TRUE | isTRUE(nlst > 1L) == TRUE) {
            DF <- as.data.frame(cbind(srt, data.frame(matrix(0L, 
                ncol = z, nrow = nrow(srt)))))
            for (k in 1:nlevels(factor(st))) {
                DF[which(st == levels(factor(st))[k]), (k + 2L)] <- 1L
            }
            rm(k)
            ties <- vector()
            for (i in 1:z) {
                ties <- append(ties, paste("T", i, sep = ""))
            }
            rm(i)
            colnames(DF) <- c("S", "R", ties)
        }
        else if (isTRUE(z == 1L) == TRUE) {
            DF <- as.data.frame(cbind(srt, rep(1, nrow(srt))), 
                row.names = NULL)
            colnames(DF) <- c("S", "R", "T")
        }
        if (isTRUE(directed == FALSE) == TRUE) {
            DF2 <- data.frame(DF[, 2], DF[, 1], DF[, 3:ncol(DF)])
            ifelse(isTRUE(z == 1L) == TRUE, colnames(DF2) <- c("S", 
                "R", "T"), colnames(DF2)[1:2] <- c("S", "R"))
            DF <- unique(as.data.frame(rbind(DF, DF2)))
        }
        else {
            NA
        }
        return(DF)
    }
    else {
        stop("Specified argument not supported.")
    }
}
