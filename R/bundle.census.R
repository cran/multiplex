bundle.census <-
function (x, loops = FALSE) 
{
    if (isTRUE(is.array(x)) == FALSE) 
        stop("'x' sholud be an array.")
    if (isTRUE(dim(x)[1] == dim(x)[2]) == FALSE) 
        stop("'x' must be a square array.")
    prsep <- ", "
    lb2lb <- TRUE
    ifelse(isTRUE(is.null(dimnames(x)[1]) == TRUE | is.null(dimnames(x)[1][[1]]) == 
        TRUE) == TRUE, LBS <- 1:nrow(x), LBS <- dimnames(x)[[1]])
    lbs <- seq(LBS)
    xd <- dichot(x, c = 1L, diag.incl = TRUE)
    ifelse(isTRUE(dim(xd)[3] == 1) == TRUE, xd <- xd[, , 1], 
        NA)
    if (is.na(dim(xd)[3]) == FALSE) {
        TRD <- TRUE
        r <- dim(x)[3]
        if (isTRUE(is.null(dimnames(x)[[3]]) == TRUE) | isTRUE(any(duplicated(dimnames(x)[[3]]))) == 
            TRUE) {
            dimnames(x)[[3]] <- 1:dim(x)[3]
        }
        mlt <- list()
        for (i in 1:dim(x)[3]) {
            mlt[[i]] <- transf(xd[, , i], type = "tolist", labels = lbs, 
                prsep = prsep, lb2lb = lb2lb)
        }
        rm(i)
        m <- unlist(mlt)
    }
    else {
        TRD <- FALSE
        r <- 1
        m <- transf(xd, type = "tolist", labels = lbs, prsep = prsep, 
            lb2lb = lb2lb)
    }
    DF <- data.frame(matrix(ncol = 2L, nrow = length(m)))
    DF[, 1] <- dhc(m, prsep = prsep)[which(1:(length(m) * 2L)%%2L == 
        1L)]
    DF[, 2] <- dhc(m, prsep = prsep)[which(1:(length(m) * 2L)%%2L == 
        0L)]
    DF <- DF[which(DF[, 1] != DF[, 2]), ]
    out <- list()
    inn <- list()
    All <- list()
    for (i in seq(lbs)) {
        out[[i]] <- as.numeric(DF[which(DF[, 1] == as.numeric(lbs[i])), 
            2])
        inn[[i]] <- as.numeric(DF[which(DF[, 2] == as.numeric(lbs[i])), 
            1])
        All[[i]] <- c(out[[i]], inn[[i]])
    }
    rm(i)
    finn <- list()
    fout <- list()
    for (i in seq(lbs)) {
        finn[[i]] <- which(tabulate(inn[[i]]) == dim(x)[3])
        fout[[i]] <- which(tabulate(out[[i]]) == dim(x)[3])
    }
    rm(i)
    full <- list()
    for (i in seq(lbs)) {
        full[[i]] <- intersect(fout[[i]], finn[[i]])
    }
    rm(i)
    rm(finn, fout)
    asym <- list()
    for (i in seq(lbs)) {
        asym[[i]] <- which(tabulate(All[[i]]) == 1L)
    }
    rm(i)
    sout <- list()
    sinn <- list()
    for (i in seq(lbs)) {
        sout[[i]] <- which(tabulate(out[[i]]) == 1L)[which(!(which(tabulate(out[[i]]) == 
            1L) %in% asym[[i]]))]
        sinn[[i]] <- which(tabulate(inn[[i]]) == 1L)[which(!(which(tabulate(inn[[i]]) == 
            1L) %in% asym[[i]]))]
    }
    rm(i)
    dobl <- list()
    dout <- list()
    dinn <- list()
    for (i in seq(lbs)) {
        dobl[[i]] <- which(tabulate(All[[i]]) == 2L)
        dout[[i]] <- which(tabulate(out[[i]]) == 2L)
        dinn[[i]] <- which(tabulate(inn[[i]]) == 2L)
    }
    rm(i)
    if ((is.na(dim(x)[3]) == FALSE | isTRUE(dim(x)[3] == 1) == 
        TRUE)) {
        Eout <- list()
        length(Eout) <- length(lbs)
        Einn <- list()
        length(Einn) <- length(lbs)
        TEnt <- list()
        length(TEnt) <- length(lbs)
        for (i in 1:length(dobl)) {
            tmpout <- vector()
            tmpinn <- vector()
            for (j in 1:length(dobl[[i]])) {
                if (isTRUE(dobl[[i]][j] %in% dout[[i]]) == TRUE) 
                  tmpout[length(tmpout) + 1L] <- dobl[[i]][j]
                if (isTRUE(dobl[[i]][j] %in% dinn[[i]]) == TRUE) 
                  tmpinn[length(tmpinn) + 1L] <- dobl[[i]][j]
            }
            rm(j)
            Eout[[i]] <- tmpout
            Einn[[i]] <- tmpinn
            TEnt[[i]] <- c(tmpout, tmpinn)
        }
        rm(tmpout, tmpinn)
    }
    else {
        TEnt <- Eout <- Einn <- character(0)
    }
    trip <- list()
    trin <- list()
    trou <- list()
    for (i in seq(lbs)) {
        trip[[i]] <- which(tabulate(All[[i]]) > 2L)
        trin[[i]] <- which(tabulate(inn[[i]]) > 2L)
        trou[[i]] <- which(tabulate(out[[i]]) > 2L)
    }
    rm(i)
    tripfl <- list()
    trinfl <- list()
    troufl <- list()
    for (i in seq(lbs)) {
        tripfl[[i]] <- trip[[i]][which(!(trip[[i]] %in% full[[i]]))]
        trinfl[[i]] <- trin[[i]][which(!(trin[[i]] %in% full[[i]]))]
        troufl[[i]] <- trou[[i]][which(!(trou[[i]] %in% full[[i]]))]
    }
    rm(i)
    sngl <- list()
    for (i in seq(lbs)) {
        sngl[[i]] <- sout[[i]][which(!(sout[[i]] %in% tripfl[[i]]))]
    }
    rm(i)
    recp <- list()
    for (i in seq(lbs)) {
        tmprcp <- vector()
        for (j in 1:length(sngl[[i]])) {
            chk <- paste(sngl[[i]][j], i, sep = prsep)
            if (isTRUE(TRD == TRUE) == TRUE) {
                for (k in 1:dim(x)[3]) {
                  ifelse(isTRUE(all(c(chk, swp(chk, prsep = prsep)) %in% 
                    mlt[[k]])) == TRUE, tmprcp <- append(tmprcp, 
                    sngl[[i]][j]), NA)
                }
                rm(k)
            }
            else {
                ifelse(isTRUE(all(c(chk, swp(chk, prsep = prsep)) %in% 
                  m)) == TRUE, tmprcp <- append(tmprcp, sngl[[i]][j]), 
                  NA)
            }
        }
        rm(j)
        recp[[i]] <- tmprcp
    }
    rm(i)
    rm(tmprcp)
    if (isTRUE(TRD == TRUE) == TRUE) {
        xchg <- list()
        for (i in seq(lbs)) {
            xchg[[i]] <- sngl[[i]][which(!(sngl[[i]] %in% recp[[i]]))]
        }
        rm(i)
    }
    else {
        xchg <- character(0)
    }
    if (isTRUE(TRD == TRUE) == TRUE) {
        Eout3p <- list()
        length(Eout3p) <- length(lbs)
        Einn3p <- list()
        length(Einn3p) <- length(lbs)
        TEnt3p <- list()
        length(TEnt3p) <- length(lbs)
        for (i in 1:length(tripfl)) {
            tmpout <- vector()
            tmpinn <- vector()
            for (j in 1:length(tripfl[[i]])) {
                if (isTRUE(tripfl[[i]][j] %in% trou[[i]]) == 
                  TRUE) 
                  tmpout[length(tmpout) + 1L] <- tripfl[[i]][j]
                if (isTRUE(tripfl[[i]][j] %in% trin[[i]]) == 
                  TRUE) 
                  tmpinn[length(tmpinn) + 1L] <- tripfl[[i]][j]
            }
            rm(j)
            Eout3p[[i]] <- tmpout
            Einn3p[[i]] <- tmpinn
            TEnt3p[[i]] <- c(tmpout, tmpinn)
        }
        TEinn <- list()
        TEout <- list()
        for (i in 1:length(TEnt3p)) {
            tmpinn <- vector()
            tmpout <- vector()
            for (j in 1:length(TEnt3p[[i]])) {
                if (isTRUE(length(Eout3p[[i]]) > 0) == TRUE) {
                  if (isTRUE(!(Einn3p[[i]][j] %in% out[[i]])) == 
                    TRUE) 
                    tmpinn <- append(tmpinn, Einn3p[[i]][j])
                  if (isTRUE(!(Eout3p[[i]][j] %in% inn[[i]])) == 
                    TRUE) 
                    tmpout <- append(tmpout, Eout3p[[i]][j])
                }
            }
            rm(j)
            TEinn[[i]] <- tmpinn
            TEout[[i]] <- tmpout
        }
        rm(i)
        rm(tmpinn, tmpout)
        mixe <- list()
        for (i in seq(lbs)) {
            mixe[[i]] <- tripfl[[i]][which(!(tripfl[[i]] %in% 
                c(TEinn[[i]], TEout[[i]])))]
        }
        rm(i)
        Eoutnp <- list()
        for (i in seq(lbs)) {
            Eoutnp[[i]] <- unique(c(Eout[[i]], Eout3p[[i]]))
            Eout[[i]] <- Eoutnp[[i]][which(!(Eoutnp[[i]] %in% 
                mixe[[i]]))]
        }
        rm(i)
    }
    else {
        mixe <- TEinn <- TEout <- character(0)
    }
    Et <- vector()
    if (isTRUE(TRD == TRUE) == TRUE) {
        for (i in 1:length(Eout)) {
            for (j in 1:length(Eout[[i]])) {
                if (isTRUE(length(Eout[[i]]) != 0L) == TRUE) {
                  Et <- append(Et, paste(lbs[i], Eout[[i]][j], 
                    sep = prsep))
                }
            }
            rm(j)
        }
        rm(i)
        ENT <- list()
        for (k in 1:r) {
            tmp <- vector()
            for (i in which(Et %in% transf(xd[, , k], labels = lbs, 
                prsep = prsep, lb2lb = lb2lb))) {
                tmp <- append(tmp, Et[i])
            }
            rm(i)
            ENT[[k]] <- as.character(tmp)
        }
        rm(k)
    }
    else {
        ENT <- as.character(Et)
    }
    if (loops) {
        if (isTRUE(is.na(dim(xd)[3])) == FALSE) {
            vec <- vector()
            for (i in 1:dim(xd)[3]) {
                vec <- append(vec, sum(diag(xd[, , i])))
            }
            rm(i)
            lop <- sum(vec)
        }
        else {
            lop <- sum(diag(xd))
        }
    }
    tbnd <- (length(unlist(full))/2L + length(unlist(asym))/2L + 
        length(unlist(recp))/2L + length(unlist(xchg))/2L + length(unlist(Eout))/1L + 
        (length(unlist(tripfl))/2L) - (length(which(table(unlist(ENT)) > 
        2L))))
    ifelse(isTRUE(loops == FALSE) == TRUE, {
        bc <- cbind(abs(choose(nrow(xd), 2) - (choose(nrow(xd), 
            2)) - tbnd), (choose(nrow(xd), 2)) - tbnd, length(unlist(asym))/2L, 
            length(unlist(recp))/2L, length(unlist(Eout))/1L, 
            length(unlist(xchg))/2L, (length(unlist(tripfl))/2L) - 
                (length(which(table(unlist(ENT)) > 2L))), length(unlist(full))/2L)
        colnames(bc) <- c("BUNDLES", "NULL", "ASYMM", "RECIP", 
            "T.ENTR", "T.EXCH", "MIXED", "FULL")
        rownames(bc) <- "TOTAL"
    }, {
        bc <- cbind(abs(choose(nrow(xd), 2) - (choose(nrow(xd), 
            2)) - tbnd), (choose(nrow(xd), 2)) - tbnd, length(unlist(asym))/2L, 
            length(unlist(recp))/2L, length(unlist(Eout))/1L, 
            length(unlist(xchg))/2L, (length(unlist(tripfl))/2L) - 
                (length(which(table(unlist(ENT)) > 2L))), length(unlist(full))/2L, 
            lop)
        colnames(bc) <- c("BUNDLES", "NULL", "ASYMM", "RECIP", 
            "T.ENTR", "T.EXCH", "MIXED", "FULL", "LOOP")
        rownames(bc) <- "TOTAL"
    })
    return(bc)
}
