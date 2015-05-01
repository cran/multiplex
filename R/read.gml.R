read.gml <-
function (file, as = c("srt", "array"), directed = TRUE) 
{
    arx <- scan(file, what = "character", nlines = -1, quiet = TRUE)
    nod <- arx[which(("node" == arx) == TRUE)[1]:length(arx)]
    n <- length(which(("node" == arx) == TRUE))
    lb <- vector()
    id <- vector()
    for (i in 1:n) {
        id[length(id) + 1] <- nod[which(("id" == nod) == TRUE)[i] + 
            1]
        if (isTRUE("label" %in% nod) == TRUE) {
            lb[length(lb) + 1] <- nod[which(("id" == nod) == 
                TRUE)[i] + 3]
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
        sr[length(sr) + 1] <- edg[which(("source" == edg) == 
            TRUE)[i] + 1]
        tg[length(tg) + 1] <- edg[which(("target" == edg) == 
            TRUE)[i] + 1]
    }
    rm(i)
    if (isTRUE("0" %in% id) == TRUE & isTRUE(all(as.numeric(id) + 
        1 == seq_along(id))) == TRUE) {
        sr <- as.numeric(sr) + 1
        tg <- as.numeric(tg) + 1
    }
    pr <- vector()
    for (i in 1:length(sr)) pr[length(pr) + 1] <- paste(sr[i], 
        tg[i], sep = ", ")
    z <- max(tabulate(factor(pr)))
    ledg <- length(grep("graphics", edg, fixed = TRUE, value = TRUE))
    nls <- nlevels(factor(edg[grep("style", edg, fixed = TRUE) + 
        1]))
    nlf <- nlevels(factor(edg[grep("fill", edg, fixed = TRUE) + 
        1]))
    st <- vector()
    if (isTRUE(ledg == 0) == FALSE) {
        for (i in 2:ledg) {
            tmp <- edg[(grep("graphics", edg, fixed = TRUE)[(i - 
                1)] + 2):(grep("graphics", edg, fixed = TRUE)[i] - 
                7)]
            if (isTRUE(nls > 1) == TRUE) {
                ifelse(isTRUE(length(tmp[grep("style", tmp, fixed = TRUE) + 
                  1]) == 0) == FALSE, st[length(st) + 1] <- tmp[grep("style", 
                  tmp, fixed = TRUE) + 1], st[length(st) + 1] <- "default")
            }
            else if (isTRUE(nlf > 1) == TRUE) {
                ifelse(isTRUE(length(tmp[grep("fill", tmp, fixed = TRUE) + 
                  1]) == 0) == FALSE, st[length(st) + 1] <- tmp[grep("fill", 
                  tmp, fixed = TRUE) + 1], st[length(st) + 1] <- "#000000")
            }
        }
        rm(i)
        tmp <- edg[tail(grep("graphics", edg, fixed = TRUE), 
            1):length(edg)]
        if (isTRUE(nls > 1) == TRUE) {
            ifelse(isTRUE(length(tmp[grep("style", tmp, fixed = TRUE) + 
                1]) == 0) == FALSE, st[length(st) + 1] <- tmp[grep("style", 
                tmp, fixed = TRUE) + 1], st[length(st) + 1] <- "default")
        }
        else if (isTRUE(nlf > 1) == TRUE) {
            ifelse(isTRUE(length(tmp[grep("fill", tmp, fixed = TRUE) + 
                1]) == 0) == FALSE, st[length(st) + 1] <- tmp[grep("fill", 
                tmp, fixed = TRUE) + 1], st[length(st) + 1] <- "#000000")
        }
        ndf <- cbind(sr, tg, st)
    }
    else {
        ndf <- cbind(sr, tg)
    }
    if (isTRUE(length(st) == 0) == TRUE) {
        st <- rep("1", nrow(ndf))
        ifelse(isTRUE(z == 2) == TRUE, st[which(duplicated(ndf))] <- "2", 
            NA)
        ndf <- cbind(ndf, st)
    }
    nlst <- nlevels(factor(st))
    if (match.arg(as) == "array") {
        if (isTRUE(z > 1) == TRUE) {
            mat <- array(0, dim = c(n, n, z))
            if (isTRUE(nlst > 1) == TRUE) {
                for (k in 1:nlst) {
                  mdf <- subset(ndf, ndf[, 3] == levels(factor(st))[k])
                  pr <- vector()
                  for (i in 1:nrow(mdf)) {
                    if (isTRUE("0" %in% id) == FALSE) {
                      pr[length(pr) + 1] <- paste(match(mdf[i, 
                        1], id), match(mdf[i, 2], id), sep = ", ")
                    }
                    else if (isTRUE("0" %in% id) == TRUE && isTRUE(all(as.numeric(id) + 
                      1 == seq_along(id))) == TRUE) {
                      pr[length(pr) + 1] <- paste(match(as.numeric(mdf[i, 
                        1]), as.numeric(id) + 1), match(as.numeric(mdf[i, 
                        2]), as.numeric(id) + 1), sep = ", ")
                    }
                    else if (isTRUE("0" %in% id) == TRUE) {
                      pr[length(pr) + 1] <- paste(match(as.numeric(mdf[i, 
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
        if (isTRUE(ncol(ndf) == 2) == TRUE | isTRUE(nlst == 1) == 
            TRUE) {
            mat <- array(0, dim = c(n, n, 1))
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
            1 == seq_along(id))) == TRUE) {
            srt <- as.data.frame(cbind(lb[match(as.numeric(ndf[, 
                1]), as.numeric(id) + 1)], lb[match(as.numeric(ndf[, 
                2]), as.numeric(id) + 1)]))
        }
        else if (isTRUE("0" %in% id) == TRUE) {
            srt <- as.data.frame(cbind(lb[match(as.numeric(ndf[, 
                1]), as.numeric(id))], lb[match(as.numeric(ndf[, 
                2]), as.numeric(id))]))
        }
        if (isTRUE(z > 1) == TRUE | isTRUE(nlst > 1) == TRUE) {
            DF <- as.data.frame(cbind(srt, data.frame(matrix(0, 
                ncol = z, nrow = nrow(srt)))))
            for (k in 1:nlevels(factor(st))) {
                DF[which(st == levels(factor(st))[k]), (k + 2)] <- 1
            }
            rm(k)
            ties <- vector()
            for (i in 1:z) {
                ties <- append(ties, paste("T", i, sep = ""))
            }
            rm(i)
            colnames(DF) <- c("S", "R", ties)
        }
        else if (isTRUE(z == 1) == TRUE) {
            DF <- as.data.frame(cbind(srt, rep(1, nrow(srt))), 
                row.names = NULL)
            colnames(DF) <- c("S", "R", "T")
        }
        if (isTRUE(directed == FALSE) == TRUE) {
            DF2 <- data.frame(DF[, 2], DF[, 1], DF[, 3:ncol(DF)])
            ifelse(isTRUE(z == 1) == TRUE, colnames(DF2) <- c("S", 
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
