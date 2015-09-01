bundles <-
function (m, loops = FALSE, prsep = ", ", smpl = FALSE, lb2lb = TRUE, 
    collapse = FALSE) 
{
    ifelse(isTRUE(is.null(dimnames(m)[1]) == TRUE | is.null(dimnames(m)[1][[1]]) == 
        TRUE) == TRUE, LBS <- 1:nrow(m), LBS <- dimnames(m)[[1]])
    lbs <- seq(LBS)
    if (isTRUE(is.na(dim(m)[3])) == FALSE) {
        if (isTRUE(is.null(dimnames(m)[[3]]) == TRUE) | isTRUE(any(duplicated(dimnames(m)[[3]]))) == 
            TRUE) 
            dimnames(m)[[3]] <- 1:dim(m)[3]
    }
    if (is.na(dim(m)[3]) == FALSE) {
        x <- transf(dichot(m)[, , 1], "matlist", labels = lbs, 
            prsep = prsep, lb2lb = lb2lb)
        if (isTRUE(dim(m)[3] > 1) == TRUE) {
            for (k in 2:dim(m)[3]) x <- append(x, transf(dichot(m)[, 
                , k], "matlist", labels = lbs, prsep = prsep, 
                lb2lb = lb2lb))
            rm(k)
        }
    }
    else {
        x <- transf(dichot(m), "matlist", labels = lbs, prsep = prsep, 
            lb2lb = lb2lb)
    }
    dfl <- data.frame(matrix(ncol = 2L, nrow = 0L))
    for (i in 1:length(x)) {
        dfl[i, 1] <- strsplit(x[i], prsep)[[1]][1]
        dfl[i, 2] <- strsplit(x[i], prsep)[[1]][2]
    }
    rm(i)
    DF <- dfl
    DF <- data.frame(matrix(ncol = 2L, nrow = 0L))
    k <- 1
    for (i in 1:nrow(dfl)) {
        if (isTRUE(dfl[i, 1] != dfl[i, 2]) == TRUE) 
            DF[k, ] <- dfl[i, ]
        k <- k + 1L
    }
    rm(i)
    rm(k)
    DF <- stats::na.omit(DF)
    out <- list()
    inn <- list()
    All <- list()
    for (i in 1:length(lbs)) {
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
        finn[[i]] <- which(tabulate(inn[[i]]) == dim(m)[3])
        fout[[i]] <- which(tabulate(out[[i]]) == dim(m)[3])
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
    dobl <- list()
    dout <- list()
    for (i in seq(lbs)) {
        dobl[[i]] <- which(tabulate(All[[i]]) == 2L)
        dout[[i]] <- which(tabulate(out[[i]]) == 2L)
    }
    rm(i)
    rete <- list()
    for (i in 1:length(dobl)) {
        tmprte <- vector()
        for (j in 1:length(dobl[[i]])) {
            if (isTRUE(dobl[[i]][j] %in% which(tabulate(inn[[i]]) == 
                1L)) == TRUE && isTRUE(dobl[[i]][j] %in% which(tabulate(out[[i]]) == 
                1)) == TRUE) 
                tmprte[length(tmprte) + 1L] <- dobl[[i]][j]
        }
        rm(j)
        rete[[i]] <- tmprte
    }
    rm(i)
    rm(tmprte)
    if (isTRUE(is.na(dim(m)[3])) == FALSE) {
        tmp <- list()
        tt <- vector()
        if (isTRUE(is.null(dimnames(m)[[3]])) == FALSE) {
            for (i in 1:length(dimnames(m)[[3]])) tmp[i] <- dimnames(m)[[3]][i]
            rm(i)
            for (i in 1:length(tmp)) tt[i] <- (strsplit(tmp[[i]], 
                "")[[1]][1])
            rm(i)
        }
        else {
            for (i in 1:dim(m)[3]) tt[i] <- tmp[i] <- i
            rm(i)
        }
        for (k in 1:length(tt)) {
            allr <- paste("all", tt[k], sep = "_")
            assign(allr, transf(dichot(m)[, , k], "matlist", 
                labels = lbs, prsep = prsep, lb2lb = lb2lb))
            tmp <- transf(dichot(m)[, , k], "matlist", labels = lbs, 
                prsep = prsep, lb2lb = lb2lb)
            tDF <- data.frame(matrix(ncol = 2L, nrow = 0L))
            for (i in 1:length(tmp)) {
                tDF[i, 1] <- strsplit(tmp[i], prsep)[[1]][1]
                tDF[i, 2] <- strsplit(tmp[i], prsep)[[1]][2]
            }
            rm(i)
            rm(tmp)
            oud <- list()
            ind <- list()
            ald <- list()
            for (i in 1:length(lbs)) {
                oud[[i]] <- as.numeric(tDF[which(tDF[, 1] == 
                  as.numeric(lbs[i])), 2])
                ind[[i]] <- as.numeric(tDF[which(tDF[, 2] == 
                  as.numeric(lbs[i])), 1])
                ald[[i]] <- c(oud[[i]], ind[[i]])
            }
            rm(i)
            assign(allr, ald)
            rm(oud, ind, ald, allr)
            rm(tDF)
        }
        rm(k)
    }
    else if (isTRUE(is.na(dim(m)[3])) == FALSE | isTRUE(dim(m)[3] == 
        1L) == TRUE) {
        tt <- "R"
        tmp <- x
    }
    if (isTRUE(is.na(dim(m)[3])) == FALSE) {
        for (k in 1:length(tt)) {
            tmpxchg <- list()
            xchr <- paste("xch", tt[k], sep = "_")
            for (i in 1:length(rete)) {
                allr <- paste("all", tt[k], sep = "_")
                tmpxchr <- vector()
                for (j in 1:length(rete[[i]])) {
                  ifelse(isTRUE(rete[[i]][j] %in% which(tabulate(eval(as.name(allr))[[i]]) == 
                    1)) == TRUE, tmpxchr[length(tmpxchr) + 1L] <- rete[[i]][j], 
                    NA)
                }
                rm(j)
                tmpxchg[[i]] <- tmpxchr
            }
            rm(i)
            assign(xchr, tmpxchg)
        }
        rm(k)
        rm(tmpxchr)
        rm(tmpxchg)
        xchg <- list()
        length(xchg) <- length(lbs)
        for (k in 1:length(tt)) {
            xchr <- paste("xch", tt[k], sep = "_")
            for (i in 1:length(rete)) {
                vecr <- paste("vec", i, sep = "")
                tmpxch <- vector()
                tmpxch <- eval(as.name(xchr))[[i]]
                if (sum(tmpxch) > 0) {
                  assign(vecr, tmpxch)
                  xchg[[i]] <- eval(as.name(vecr))
                  rm(vecr)
                }
                else {
                  rm(vecr)
                }
            }
            rm(i)
        }
        rm(k)
        rm(tmpxch)
        rm(xchr)
    }
    else {
        xchg <- logical(0)
    }
    recp <- list()
    if (isTRUE(is.na(dim(m)[3])) == FALSE) {
        for (i in 1:length(rete)) {
            recp[[i]] <- rete[[i]][which(!(rete[[i]] %in% xchg[[i]]))]
        }
        rm(i)
    }
    else {
        recp <- rete
    }
    if (isTRUE(is.na(dim(m)[3])) == FALSE) {
        Eout <- list()
        length(Eout) <- length(lbs)
        for (i in 1:length(dobl)) {
            tmpout <- vector()
            for (j in 1:length(dobl[[i]])) {
                if (isTRUE(dobl[[i]][j] %in% dout[[i]]) == TRUE) 
                  tmpout[length(tmpout) + 1L] <- dobl[[i]][j]
            }
            rm(j)
            Eout[[i]] <- tmpout
        }
        rm(tmpout)
        trpr <- list()
        tinn <- list()
        tout <- list()
        for (i in seq(lbs)) {
            trpr[[i]] <- which(tabulate(All[[i]]) > 2L)
            tinn[[i]] <- which(tabulate(inn[[i]]) > 2L)
            tout[[i]] <- which(tabulate(out[[i]]) > 2L)
        }
        rm(i)
        teinn <- list()
        teout <- list()
        length(teinn) <- length(lbs)
        length(teout) <- length(lbs)
        for (i in 1:length(trpr)) {
            tmpinn <- vector()
            tmpout <- vector()
            for (j in 1:length(trpr[[i]])) {
                if (isTRUE(trpr[[i]][j] %in% tinn[[i]]) == TRUE) 
                  tmpinn[length(tmpinn) + 1] <- trpr[[i]][j]
                if (isTRUE(trpr[[i]][j] %in% tout[[i]]) == TRUE) 
                  tmpout[length(tmpout) + 1] <- trpr[[i]][j]
            }
            rm(j)
            teinn[[i]] <- tmpinn
            teout[[i]] <- tmpout
        }
        rm(tmpinn, tmpout)
        TEinn <- list()
        TEout <- list()
        for (i in 1:length(trpr)) {
            tmpinn <- vector()
            tmpout <- vector()
            for (j in 1:length(trpr[[i]])) {
                if (isTRUE(!(teinn[[i]][j] %in% out[[i]])) == 
                  TRUE) 
                  tmpinn[length(tmpinn) + 1L] <- teinn[[i]][j]
                if (isTRUE(!(teout[[i]][j] %in% inn[[i]])) == 
                  TRUE) 
                  tmpout[length(tmpout) + 1L] <- teout[[i]][j]
            }
            rm(j)
            TEinn[[i]] <- tmpinn
            TEout[[i]] <- tmpout
        }
        rm(i)
        rm(tmpinn, tmpout)
    }
    else {
        TEinn <- TEout <- Eout <- logical(0)
    }
    mix <- list()
    if (isTRUE(is.na(dim(m)[3])) == FALSE) {
        for (i in 1:length(trpr)) {
            mix[[i]] <- trpr[[i]][which(!(trpr[[i]] %in% TEinn[[i]] | 
                trpr[[i]] %in% TEout[[i]]))]
        }
        rm(i)
        mixe <- list()
        for (i in 1:length(mix)) {
            mixe[[i]] <- mix[[i]][which(!(mix[[i]] %in% full[[i]]))]
        }
        rm(i)
    }
    else {
        mixe <- logical(0)
    }
    As <- vector()
    for (i in 1:length(asym)) {
        for (j in 1:length(asym[[i]])) {
            if (isTRUE(length(asym[[i]]) != 0L) == TRUE) {
                if (isTRUE(is.na(dim(m)[3])) == FALSE) {
                  As <- append(As, paste(lbs[i], asym[[i]][j], 
                    sep = prsep))
                }
                else {
                  ifelse(isTRUE(asym[[i]][j] %in% out[[i]]) == 
                    TRUE, As <- append(As, paste(lbs[i], asym[[i]][j], 
                    sep = prsep)), NA)
                }
            }
        }
        rm(j)
    }
    rm(i)
    if (isTRUE(is.na(dim(m)[3])) == FALSE) {
        AS <- list()
        for (k in 1:length(tt)) {
            tmp <- vector()
            for (i in which(As %in% transf(m[, , k], labels = lbs, 
                prsep = prsep, lb2lb = lb2lb))) {
                tmp <- append(tmp, As[i])
            }
            rm(i)
            AS[[k]] <- tmp
        }
        rm(k)
    }
    else {
        AS <- As
    }
    Rp <- vector()
    for (i in 1:length(recp)) {
        for (j in 1:length(recp[[i]])) {
            if (isTRUE(length(recp[[i]]) != 0L) == TRUE) {
                Rp <- append(Rp, paste(lbs[i], recp[[i]][j], 
                  sep = prsep))
            }
        }
        rm(j)
    }
    rm(i)
    if (isTRUE(is.na(dim(m)[3])) == FALSE) {
        RP <- list()
        for (k in 1:length(tt)) {
            tmp <- vector()
            for (i in which(Rp %in% transf(m[, , k], labels = lbs, 
                prsep = prsep, lb2lb = lb2lb))) {
                tmp <- append(tmp, Rp[i])
            }
            rm(i)
            RP[[k]] <- tmp
        }
        rm(k)
    }
    else {
        RP <- Rp
    }
    Xc <- vector()
    if (isTRUE(is.na(dim(m)[3])) == FALSE) {
        for (i in 1:length(xchg)) {
            for (j in 1:length(xchg[[i]])) {
                if (isTRUE(length(xchg[[i]]) != 0L) == TRUE) {
                  if (isTRUE(lbs[i] < xchg[[i]][j]) == TRUE) 
                    Xc <- append(Xc, paste(lbs[i], xchg[[i]][j], 
                      sep = prsep))
                }
            }
            rm(j)
        }
        rm(i)
        XC <- list()
        XCt <- list()
        for (k in 1:length(tt)) {
            tmp <- vector()
            tmpt <- vector()
            for (i in which(Xc %in% transf(m[, , k], labels = lbs, 
                prsep = prsep, lb2lb = lb2lb))) {
                tmp <- append(tmp, Xc[i])
            }
            rm(i)
            for (i in which(Xc %in% transf(t(m[, , k]), labels = lbs, 
                prsep = prsep, lb2lb = lb2lb))) {
                tmpt <- append(tmpt, paste(strsplit(Xc[i], prsep)[[1]][2], 
                  strsplit(Xc[i], prsep)[[1]][1], sep = prsep))
            }
            rm(i)
            XC[[k]] <- tmp
            XCt[[k]] <- tmpt
        }
        rm(k)
        XCH <- list()
        for (i in 1:length(XC)) {
            XCH[[i]] <- c(XC[[i]], XCt[[i]])
        }
        rm(i)
    }
    else {
        XCH <- Xc
    }
    Et <- vector()
    if (isTRUE(is.na(dim(m)[3])) == FALSE) {
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
        for (i in 1:length(TEout)) {
            for (j in 1:length(TEout[[i]])) {
                if (isTRUE(is.na(stats::na.omit(TEout[[i]])) == 
                  FALSE) == TRUE) {
                  Et <- append(Et, paste(lbs[i], TEout[[i]][j], 
                    sep = prsep))
                }
            }
            rm(j)
        }
        rm(i)
        ENT <- list()
        for (k in 1:length(tt)) {
            tmp <- vector()
            for (i in which(Et %in% transf(m[, , k], labels = lbs, 
                prsep = prsep, lb2lb = lb2lb))) {
                tmp <- append(tmp, Et[i])
            }
            rm(i)
            ENT[[k]] <- tmp
        }
        rm(k)
    }
    else {
        ENT <- Et
    }
    Mx <- vector()
    if (isTRUE(is.na(dim(m)[3])) == FALSE) {
        for (i in 1:length(mixe)) {
            for (j in 1:length(mixe[[i]])) {
                if (isTRUE(length(mixe[[i]]) != 0L) == TRUE) {
                  if (isTRUE(lbs[i] < mixe[[i]][j]) == TRUE) 
                    Mx <- append(Mx, paste(lbs[i], mixe[[i]][j], 
                      sep = prsep))
                }
            }
            rm(j)
        }
        rm(i)
        MX <- list()
        MXt <- list()
        for (k in 1:length(tt)) {
            tmp <- vector()
            tmpt <- vector()
            for (i in which(Mx %in% transf(m[, , k], labels = lbs, 
                prsep = prsep, lb2lb = lb2lb))) {
                tmp <- append(tmp, Mx[i])
            }
            rm(i)
            for (i in which(Mx %in% transf(t(m[, , k]), labels = lbs, 
                prsep = prsep, lb2lb = lb2lb))) {
                tmpt <- append(tmpt, paste(strsplit(Mx[i], prsep)[[1]][2], 
                  strsplit(Mx[i], prsep)[[1]][1], sep = prsep))
            }
            rm(i)
            MX[[k]] <- tmp
            MXt[[k]] <- tmpt
        }
        rm(k)
        MIX <- list()
        for (i in 1:length(MX)) {
            MIX[[i]] <- c(MX[[i]], MXt[[i]])
        }
        rm(i)
    }
    else {
        MIX <- Mx
    }
    Fl <- vector()
    if (isTRUE(is.na(dim(m)[3])) == FALSE) {
        for (i in 1:length(full)) {
            for (j in 1:length(full[[i]])) {
                if (isTRUE(length(full[[i]]) != 0L) == TRUE) {
                  if (isTRUE(lbs[i] < full[[i]][j]) == TRUE) 
                    Fl <- append(Fl, paste(lbs[i], full[[i]][j], 
                      sep = prsep))
                }
            }
            rm(j)
        }
        rm(i)
        FL <- list()
        FLt <- list()
        for (k in 1:length(tt)) {
            tmp <- vector()
            tmpt <- vector()
            for (i in which(Fl %in% transf(m[, , k], labels = lbs, 
                prsep = prsep, lb2lb = lb2lb))) {
                tmp <- append(tmp, Fl[i])
            }
            rm(i)
            for (i in which(Fl %in% transf(t(m[, , k]), labels = lbs, 
                prsep = prsep, lb2lb = lb2lb))) {
                tmpt <- append(tmpt, paste(strsplit(Fl[i], prsep)[[1]][2], 
                  strsplit(Fl[i], prsep)[[1]][1], sep = prsep))
            }
            rm(i)
            FL[[k]] <- tmp
            FLt[[k]] <- tmpt
        }
        rm(k)
        FUL <- list()
        for (i in 1:length(FL)) {
            FUL[[i]] <- c(FL[[i]], FLt[[i]])
        }
        rm(i)
    }
    else {
        FUL <- Fl
    }
    if (lb2lb) {
        if (isTRUE(is.null(dimnames(m)[[1]]) == TRUE) == FALSE) {
            if (length(AS) > 0L) {
                for (k in 1:length(AS)) {
                  for (i in 1:length(AS[[k]])) {
                    if (length(AS[[k]]) > 0L) {
                      AS[[k]][i] <- paste(dimnames(m)[[1]][as.numeric(strsplit(AS[[k]][i], 
                        prsep)[[1]][1])], dimnames(m)[[1]][as.numeric(strsplit(AS[[k]][i], 
                        prsep)[[1]][2])], sep = prsep)
                    }
                  }
                  rm(i)
                }
                rm(k)
            }
            if (length(RP) > 0L) {
                for (k in 1:length(RP)) {
                  for (i in 1:length(RP[[k]])) {
                    if (length(RP[[k]]) > 0L) {
                      RP[[k]][i] <- paste(dimnames(m)[[1]][as.numeric(strsplit(RP[[k]][i], 
                        prsep)[[1]][1])], dimnames(m)[[1]][as.numeric(strsplit(RP[[k]][i], 
                        prsep)[[1]][2])], sep = prsep)
                    }
                  }
                  rm(i)
                }
                rm(k)
            }
            if (isTRUE(is.na(dim(m)[3])) == FALSE) {
                if (length(XCH) > 0L) {
                  for (k in 1:length(XCH)) {
                    for (i in 1:length(XCH[[k]])) {
                      if (length(XCH[[k]]) > 0L) {
                        XCH[[k]][i] <- paste(dimnames(m)[[1]][as.numeric(strsplit(XCH[[k]][i], 
                          prsep)[[1]][1])], dimnames(m)[[1]][as.numeric(strsplit(XCH[[k]][i], 
                          prsep)[[1]][2])], sep = prsep)
                      }
                    }
                    rm(i)
                  }
                  rm(k)
                }
                if (length(ENT) > 0L) {
                  for (k in 1:length(ENT)) {
                    for (i in 1:length(ENT[[k]])) {
                      if (length(ENT[[k]]) > 0L) {
                        ENT[[k]][i] <- paste(dimnames(m)[[1]][as.numeric(strsplit(ENT[[k]][i], 
                          prsep)[[1]][1])], dimnames(m)[[1]][as.numeric(strsplit(ENT[[k]][i], 
                          prsep)[[1]][2])], sep = prsep)
                      }
                    }
                    rm(i)
                  }
                  rm(k)
                }
                if (length(MIX) > 0L) {
                  for (k in 1:length(MIX)) {
                    for (i in 1:length(MIX[[k]])) {
                      if (length(MIX[[k]]) > 0L) {
                        MIX[[k]][i] <- paste(dimnames(m)[[1]][as.numeric(strsplit(MIX[[k]][i], 
                          prsep)[[1]][1])], dimnames(m)[[1]][as.numeric(strsplit(MIX[[k]][i], 
                          prsep)[[1]][2])], sep = prsep)
                      }
                    }
                    rm(i)
                  }
                  rm(k)
                }
                if (length(FUL) > 0L) {
                  for (k in 1:length(FUL)) {
                    for (i in 1:length(FUL[[k]])) {
                      if (length(FUL[[k]]) > 0L) {
                        FUL[[k]][i] <- paste(dimnames(m)[[1]][as.numeric(strsplit(FUL[[k]][i], 
                          prsep)[[1]][1])], dimnames(m)[[1]][as.numeric(strsplit(FUL[[k]][i], 
                          prsep)[[1]][2])], sep = prsep)
                      }
                    }
                    rm(i)
                  }
                  rm(k)
                }
            }
        }
    }
    if (loops) {
        if (isTRUE(is.na(dim(m)[3])) == FALSE) {
            LOP <- list()
            length(LOP) <- dim(m)[3]
            for (i in 1:dim(m)[3]) {
                lp <- which(diag(m[, , i]) != 0L)
                if (isTRUE(length(lp) > 0L) == TRUE) {
                  for (j in 1:length(lp)) {
                    if (lb2lb) {
                      if (isTRUE(is.null(dimnames(m)[[1]])) == 
                        FALSE) {
                        LOP[[i]] <- append(LOP[[i]], paste(dimnames(m)[[1]][lp][j], 
                          dimnames(m)[[1]][lp][j], sep = prsep))
                      }
                      else if (isTRUE(is.null(dimnames(m)[[1]])) == 
                        TRUE) {
                        LOP[[i]] <- append(LOP[[i]], paste(lp[j], 
                          lp[j], sep = prsep))
                      }
                    }
                    else {
                      LOP[[i]] <- append(LOP[[i]], paste(lp[j], 
                        lp[j], sep = prsep))
                    }
                  }
                  rm(j)
                }
                else {
                  LOP[[i]] <- logical(0)
                }
            }
            rm(i)
            attr(LOP, "names") <- dimnames(m)[[3]]
        }
        else {
            LOP <- vector()
            lp <- which(diag(m) != 0L)
            for (j in 1:length(lp)) {
                LOP <- append(LOP, paste(dimnames(m)[[1]][lp][j], 
                  dimnames(m)[[1]][lp][j], sep = prsep))
            }
            rm(j)
        }
        ifelse(isTRUE(smpl == FALSE) == TRUE, attr(LOP, "names") <- dimnames(m)[[3]], 
            attr(LOP, "names") <- tt)
    }
    if (isTRUE(is.na(dim(m)[3])) == FALSE) {
        ifelse(isTRUE(smpl == FALSE) == TRUE, attr(FUL, "names") <- attr(MIX, 
            "names") <- attr(ENT, "names") <- attr(XCH, "names") <- attr(RP, 
            "names") <- attr(AS, "names") <- dimnames(m)[[3]], 
            attr(FUL, "names") <- attr(MIX, "names") <- attr(ENT, 
                "names") <- attr(XCH, "names") <- attr(RP, "names") <- attr(AS, 
                "names") <- tt)
    }
    if (collapse) {
        ifelse(isTRUE(loops == FALSE) == TRUE, lst <- list(asym = as.vector(unlist(AS)), 
            recp = as.vector(unlist(RP)), tent = as.vector(unlist(ENT)), 
            txch = as.vector(unlist(XCH)), mixed = as.vector(unlist(MIX)), 
            full = as.vector(unlist(FUL))), lst <- list(asym = as.vector(unlist(AS)), 
            recp = as.vector(unlist(RP)), tent = as.vector(unlist(ENT)), 
            txch = as.vector(unlist(XCH)), mixed = as.vector(unlist(MIX)), 
            full = as.vector(unlist(FUL)), loop = as.vector(unlist(LOP))))
    }
    else {
        ifelse(isTRUE(loops == FALSE) == TRUE, lst <- list(asym = AS, 
            recp = RP, tent = ENT, txch = XCH, mixed = MIX, full = FUL), 
            lst <- list(asym = AS, recp = RP, tent = ENT, txch = XCH, 
                mixed = MIX, full = FUL, loop = LOP))
    }
    class(lst) <- "Rel.Bundles"
    return(lst)
}
