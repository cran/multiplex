strings <-
function (x, smpl = FALSE, equat = FALSE, k = 2) 
{
    if (is.array(x) == FALSE) 
        stop("Data must be a stacked array of square matrices.")
    if (k > 3) 
        warning("Only equations until k = 3 will be considered.")
    if (is.na(dim(x)[3]) == TRUE) 
        stop("Labels in matrix(ces) needed")
    if (isTRUE(any(duplicated(dimnames(x)[[3]]))) == TRUE) 
        stop("Duplicated labels found in the input data")
    if (is.null(dimnames(x)[[3]]) == TRUE) 
        dimnames(x)[[3]] <- 1:dim(x)[3]
    if (smpl == TRUE | equat == TRUE) {
        ifelse(is.null(dimnames(x)[[3]]) == FALSE, lbs <- dimnames(x)[[3]], 
            lbs <- 1:dim(x)[3])
        if (is.null(dimnames(x)[[3]]) == FALSE) {
            nlb <- list()
            for (i in 1:length(lbs)) {
                nlb[i] <- lbs[i]
            }
            rm(i)
            for (i in 1:length(nlb)) {
                lbs[i] <- (strsplit(nlb[[i]], "")[[1]][1])
            }
            dimnames(x)[[3]] <- lbs
        }
    }
    gener <- dimnames(x)[[3]]
    if (is.na(dim(x)[3]) == TRUE) {
        s0 <- data.frame(matrix(ncol = 1, nrow = 1))
        if (isTRUE(all.equal(replace(x %*% x, x %*% x >= 1, 1), 
            x) == TRUE)) 
            s0[1, 1] <- 1
        Bx <- array(dim = c(dim(x)[1], dim(x)[2], 2))
        Bx[, , 1] <- as.matrix(x)
        Bx[, , 2] <- replace(x %*% x, x %*% x >= 1, 1)
    }
    if (is.na(dim(x)[3]) == FALSE) {
        tmpo <- data.frame(matrix(ncol = (dim(x)[1] * dim(x)[2]), 
            nrow = 0))
        for (i in 1:dim(x)[3]) {
            ifelse(isTRUE(dim(x)[3] > 1) == TRUE, tmpo[i, ] <- as.vector(x[, 
                , i]), tmpo <- as.vector(x))
        }
        rm(i)
        if (isTRUE(is.character(dimnames(x)[[3]]) == TRUE) == 
            TRUE) 
            dimnames(x)[[3]][which(duplicated(dimnames(x)[[3]]))] <- 1:length(which(duplicated(dimnames(x)[[3]])))
        if (isTRUE(is.null(dim(tmpo)) == FALSE) == TRUE) 
            rownames(tmpo) <- dimnames(x)[[3]]
        tmpu <- unique(tmpo)
        dpl <- duplicated(tmpo)
        if (isTRUE(nrow(tmpo) != nrow(tmpu)) == TRUE) {
            ifelse(isTRUE((nrow(tmpo) - nrow(tmpu)) > 1) == TRUE, 
                note <- paste("There are", nrow(tmpo) - nrow(tmpu), 
                  "repeated generators that have been equated", 
                  sep = " "), note <- "There is 1 repeated generator that has been equated")
        }
        else {
            note <- NULL
        }
        if (isTRUE(dim(x)[3] < 2) == TRUE) 
            x <- array(tmpo, c(dim(x)[1], dim(x)[2]))
        if (isTRUE(dim(x)[3] > 1) == TRUE) {
            tmp <- array(dim = c(dim(x)[1], dim(x)[2], nrow(tmpu)))
            for (i in 1:nrow(tmpu)) {
                tmp[, , i][1:(dim(x)[1] * dim(x)[2])] <- as.numeric(tmpu[i, 
                  ])
            }
            rm(i)
            if (is.null(dimnames(tmp)[[1]]) == FALSE) 
                dimnames(tmp)[[3]] <- rownames(tmpu)
            if (is.null(dimnames(x)[[1]]) == FALSE) 
                dimnames(tmp)[[1]] <- dimnames(tmp)[[2]] <- dimnames(x)[[1]]
            x <- tmp
            dimnames(x)[[3]] <- as.list(rownames(tmpu))
        }
        rm(tmp)
        s0 <- data.frame(matrix(ncol = dim(x)[3], nrow = dim(x)[3]))
        for (q in 1:dim(x)[3]) {
            for (j in 1:dim(x)[3]) {
                tmp <- x[, , j] %*% x[, , q]
                tmp <- replace(tmp, tmp >= 1, 1)
                for (i in dim(x)[3]:1) {
                  if (isTRUE(all.equal(tmp, x[, , i]) == TRUE)) 
                    s0[j, q] <- i
                }
            }
        }
        rm(i, j, q)
        dimnames(s0)[[1]] <- 1:dim(x)[3]
        dimnames(s0)[[2]] <- 1:dim(x)[3]
        if (sum(as.numeric(is.na(s0))) == 0) 
            Bx <- x
        if (sum(as.numeric(is.na(s0))) > 0) {
            Bx <- array(dim = c(dim(x)[1], dim(x)[2], 0))
            for (i in 1:nrow(s0)) {
                for (j in 1:length(which(is.na(s0[i, ])))) {
                  if (length(which(is.na(s0[i, ]))) > 0) 
                    Bx <- zbnd(Bx, (replace(x[, , i] %*% x[, 
                      , which(is.na(s0[i, ]))[j]], x[, , i] %*% 
                      x[, , which(is.na(s0[i, ]))[j]] >= 1, 1)))
                }
            }
            rm(i, j)
            tmp <- data.frame(matrix(ncol = (dim(x)[1] * dim(x)[2]), 
                nrow = 0))
            for (i in 1:dim(Bx)[3]) {
                tmp[i, ] <- as.vector(Bx[, , i])
            }
            rm(i)
            xBx <- array(dim = c(dim(x)[1], dim(x)[2], nrow(unique(tmp))))
            for (i in 1:nrow(unique(tmp))) {
                xBx[, , i][1:(dim(Bx)[1] * dim(Bx)[2])] <- as.numeric(unique(tmp)[i, 
                  ])
            }
            rm(i)
            if (is.null(dimnames(xBx)) == FALSE) 
                dimnames(xBx)[[3]] <- (dim(x)[3] + 1):(dim(xBx)[3] + 
                  dim(x)[3])
            Bx <- zbnd(x, xBx)
            rm(xBx, tmp)
        }
    }
    while (sum(as.numeric(is.na(s0))) > 0) {
        BBx <- Bx
        for (i in 1:nrow(s0)) {
            for (j in 1:length(which(is.na(s0[i, ])))) {
                if (length(which(is.na(s0[i, ]))) > 0) 
                  BBx <- zbnd(BBx, (replace(Bx[, , i] %*% Bx[, 
                    , which(is.na(s0[i, ]))[j]], Bx[, , i] %*% 
                    Bx[, , which(is.na(s0[i, ]))[j]] >= 1, 1)))
            }
        }
        rm(i, j)
        tmp <- data.frame(matrix(ncol = (dim(Bx)[1] * dim(Bx)[2]), 
            nrow = 0))
        for (i in 1:dim(BBx)[3]) {
            tmp[i, ] <- as.vector(BBx[, , i])
        }
        rm(i)
        Bx <- array(dim = c(dim(x)[1], dim(x)[2], nrow(unique(tmp))))
        for (i in 1:nrow(unique(tmp))) {
            Bx[, , i][1:(dim(BBx)[1] * dim(BBx)[2])] <- as.numeric(unique(tmp)[i, 
                ])
        }
        rm(i)
        rm(tmp, BBx)
        if (is.na(dim(x)[3]) == TRUE) {
            s0 <- data.frame(matrix(ncol = 1, nrow = dim(Bx)[3]))
            for (j in 1:dim(Bx)[3]) {
                tmp <- Bx[, , j] %*% Bx[, , 1]
                tmp <- replace(tmp, tmp >= 1, 1)
                for (i in dim(Bx)[3]:1) {
                  if (isTRUE(all.equal(tmp, Bx[, , i]) == TRUE)) 
                    s0[j, 1] <- i
                }
            }
            rm(i, j)
        }
        if (is.na(dim(x)[3]) == FALSE) {
            s0 <- data.frame(matrix(ncol = dim(x)[3], nrow = dim(Bx)[3]))
            for (q in 1:dim(x)[3]) {
                for (j in 1:dim(Bx)[3]) {
                  tmp <- Bx[, , j] %*% Bx[, , q]
                  tmp <- replace(tmp, tmp >= 1, 1)
                  for (i in dim(Bx)[3]:1) {
                    if (isTRUE(all.equal(tmp, Bx[, , i]) == TRUE)) 
                      s0[j, q] <- i
                  }
                }
            }
            rm(i, j, q)
        }
    }
    ifelse(isTRUE(is.na(dim(x)[3])) == TRUE, dimnames(s0)[[2]] <- 1, 
        dimnames(s0)[[2]] <- 1:dim(x)[3])
    tmpO <- data.frame(matrix(ncol = (dim(Bx)[1] * dim(Bx)[2]), 
        nrow = 0))
    for (i in 1:dim(Bx)[3]) {
        tmpO[i, ] <- as.vector(Bx[, , i])
    }
    rm(i)
    rownames(tmpO) <- dimnames(Bx)[[3]]
    tmpU <- unique(tmpO)
    tmp <- array(dim = c(dim(Bx)[1], dim(Bx)[2], nrow(tmpU)))
    for (i in 1:nrow(tmpU)) {
        tmp[, , i][1:(dim(Bx)[1] * dim(Bx)[2])] <- as.numeric(tmpU[i, 
            ])
    }
    rm(i)
    Bx <- tmp
    dimnames(Bx)[[3]] <- as.list(rownames(tmpU))
    E <- s0
    rm(s0)
    if (dim(Bx)[3] == ncol(E)) {
        W <- rbind(cbind(1:ncol(E), NA, NA))
        colnames(W) <- c("", "n", "g")
    }
    if (dim(Bx)[3] > ncol(E)) {
        tmp <- (ncol(E) + 1):dim(Bx)[3]
        z <- vector()
        for (i in 1:length(tmp)) {
            z[i] <- which(t(E) == tmp[i])[1]
        }
        rm(i)
        g <- vector()
        for (i in 1:length(tmp)) {
            ifelse(z[i]%%ncol(E) == 0, g[i] <- ncol(E), g[i] <- z[i]%%ncol(E))
        }
        rm(i)
        n <- vector()
        for (i in 1:length(tmp)) {
            ifelse(z[i]%%ncol(E) == 0, n[i] <- (z[i]%/%ncol(E)), 
                n[i] <- ((z[i]%/%ncol(E)) + 1))
        }
        rm(i)
        W <- rbind(cbind(1:ncol(E), NA, NA), cbind(((ncol(E) + 
            1):nrow(E)), n, g))
        rm(z, n, g)
    }
    rm(tmp)
    if (is.na(dim(x)[3]) == TRUE) {
        ifelse(is.null(dimnames(Bx)[[3]]) == TRUE, lbl <- 1:dim(Bx)[3], 
            lbl <- dimnames(Bx)[[3]])
    }
    if (is.na(dim(x)[3]) == FALSE) {
        if (is.null(dimnames(x)[[3]]) == TRUE) 
            lbl <- 1:dim(Bx)[3]
        if (is.null(dimnames(x)[[3]]) == FALSE) {
            if (isTRUE(dim(Bx)[3] == dim(x)[3]) == TRUE) 
                lbl <- dimnames(x)[[3]]
            if (isTRUE(dim(Bx)[3] < dim(x)[3]) == TRUE) 
                lbl <- rownames(tmpO)
            if (isTRUE(dim(Bx)[3] > dim(x)[3]) == TRUE) 
                lbl <- c(dimnames(x)[[3]], (dim(x)[3] + 1):dim(Bx)[3])
        }
        dimnames(Bx)[[3]] <- lbl
        for (i in which(W[, 2] < (ncol(E) + 1))) {
            lbl[(i)] <- paste(dimnames(Bx)[[3]][W[, 2][i]], dimnames(Bx)[[3]][W[, 
                3][i]], sep = "")
        }
        rm(i)
        for (i in which(W[, 2] < (which(W[, 2] > ncol(E))[1]))[(length(which(W[, 
            2] < (ncol(E) + 1))) + 1):length(which(W[, 2] < (which(W[, 
            2] > ncol(E))[1])))]) {
            lbl[(i)] <- paste(dimnames(Bx)[[3]][W[W[, 2][i], 
                ][2]], dimnames(Bx)[[3]][W[W[, 2][i], ][3]], 
                dimnames(Bx)[[3]][W[, 3][i]], sep = "")
        }
        rm(i)
        for (i in which(W[, 2] < (which(W[, 2] >= (which(W[, 
            2] > ncol(E)))[1])[1]))[(length(which(W[, 2] < (which(W[, 
            2] > ncol(E))[1]))) + 1):length(which(W[, 2] < (which(W[, 
            2] >= (which(W[, 2] > ncol(E)))[1])[1])))]) {
            lbl[(i)] <- paste(dimnames(Bx)[[3]][W[W[W[W[, 2][i], 
                ], ][1, ][2], ][2]], dimnames(Bx)[[3]][W[W[W[W[, 
                2][i], ], ][1, ][2], ][3]], dimnames(Bx)[[3]][W[W[, 
                2][i], ][3]], dimnames(Bx)[[3]][W[, 3][i]], sep = "")
        }
        rm(i)
        for (i in which(W[, 2] < (which(W[, 2] >= (which(W[, 
            2] >= (which(W[, 2] > ncol(E))[1])))[1])[1]))[(length(which(W[, 
            2] < (which(W[, 2] >= (which(W[, 2] > ncol(E)))[1])[1]))) + 
            1):length(which(W[, 2] < (which(W[, 2] >= (which(W[, 
            2] >= (which(W[, 2] > ncol(E))[1])))[1])[1])))]) {
            lbl[(i)] <- paste(dimnames(Bx)[[3]][W[W[W[W[W[, 2][i], 
                ], ][1, ][2], ][2], ][2]], dimnames(Bx)[[3]][W[W[W[W[W[, 
                2][i], ], ][1, ][2], ][2], ][3]], dimnames(Bx)[[3]][W[W[W[W[, 
                2][i], ], ][1, ][2], ][3]], dimnames(Bx)[[3]][W[W[, 
                2][i], ][3]], dimnames(Bx)[[3]][W[, 3][i]], sep = "")
        }
        rm(i)
        for (i in which(W[, 2] < (which(W[, 2] >= (which(W[, 
            2] >= (which(W[, 2] >= (which(W[, 2] > ncol(E))[1]))[1])))[1])[1]))[(length(which(W[, 
            2] < (which(W[, 2] >= (which(W[, 2] >= (which(W[, 
            2] > ncol(E))[1])))[1])[1]))) + 1):length(which(W[, 
            2] < (which(W[, 2] >= (which(W[, 2] >= (which(W[, 
            2] >= (which(W[, 2] > ncol(E))[1]))[1])))[1])[1])))]) {
            lbl[(i)] <- paste(dimnames(Bx)[[3]][W[W[W[W[W[W[, 
                2][i], ], ][1, ][2], ][2], ][2], ][2]], dimnames(Bx)[[3]][W[W[W[W[W[W[, 
                2][i], ], ][1, ][2], ][2], ][2], ][3]], dimnames(Bx)[[3]][W[W[W[W[W[, 
                2][i], ], ][1, ][2], ][2], ][3]], dimnames(Bx)[[3]][W[W[W[W[, 
                2][i], ], ][1, ][2], ][3]], dimnames(Bx)[[3]][W[W[, 
                2][i], ][3]], dimnames(Bx)[[3]][W[, 3][i]], sep = "")
        }
        rm(i)
        for (i in which(W[, 2] < (which(W[, 2] >= (which(W[, 
            2] >= (which(W[, 2] >= (which(W[, 2] >= (which(W[, 
            2] > ncol(E))[1]))[1]))[1])))[1])[1]))[(length(which(W[, 
            2] < (which(W[, 2] >= (which(W[, 2] >= (which(W[, 
            2] >= (which(W[, 2] > ncol(E))[1]))[1])))[1])[1]))) + 
            1):length(which(W[, 2] < (which(W[, 2] >= (which(W[, 
            2] >= (which(W[, 2] >= (which(W[, 2] >= (which(W[, 
            2] > ncol(E))[1]))[1]))[1])))[1])[1])))]) {
            lbl[(i)] <- paste(dimnames(Bx)[[3]][W[W[W[W[W[W[W[, 
                2][i], ], ][1, ][2], ][2], ][2], ][2], ][2]], 
                dimnames(Bx)[[3]][W[W[W[W[W[W[W[, 2][i], ], ][1, 
                  ][2], ][2], ][2], ][2], ][3]], dimnames(Bx)[[3]][W[W[W[W[W[W[, 
                  2][i], ], ][1, ][2], ][2], ][2], ][3]], dimnames(Bx)[[3]][W[W[W[W[W[, 
                  2][i], ], ][1, ][2], ][2], ][3]], dimnames(Bx)[[3]][W[W[W[W[, 
                  2][i], ], ][1, ][2], ][3]], dimnames(Bx)[[3]][W[W[, 
                  2][i], ][3]], dimnames(Bx)[[3]][W[, 3][i]], 
                sep = "")
        }
        rm(i)
        for (i in which(W[, 2] < (which(W[, 2] >= (which(W[, 
            2] >= (which(W[, 2] >= (which(W[, 2] >= (which(W[, 
            2] >= (which(W[, 2] > ncol(E))[1]))[1]))[1]))[1])))[1])[1]))[(length(which(W[, 
            2] < (which(W[, 2] >= (which(W[, 2] >= (which(W[, 
            2] >= (which(W[, 2] >= (which(W[, 2] > ncol(E))[1]))[1]))[1])))[1])[1]))) + 
            1):length(which(W[, 2] < (which(W[, 2] >= (which(W[, 
            2] >= (which(W[, 2] >= (which(W[, 2] >= (which(W[, 
            2] >= (which(W[, 2] > ncol(E))[1]))[1]))[1]))[1])))[1])[1])))]) {
            lbl[(i)] <- paste(dimnames(Bx)[[3]][W[W[W[W[W[W[W[W[, 
                2][i], ], ][1, ][2], ][2], ][2], ][2], ][2], 
                ][2]], dimnames(Bx)[[3]][W[W[W[W[W[W[W[W[, 2][i], 
                ], ][1, ][2], ][2], ][2], ][2], ][2], ][3]], 
                dimnames(Bx)[[3]][W[W[W[W[W[W[W[, 2][i], ], ][1, 
                  ][2], ][2], ][2], ][2], ][3]], dimnames(Bx)[[3]][W[W[W[W[W[W[, 
                  2][i], ], ][1, ][2], ][2], ][2], ][3]], dimnames(Bx)[[3]][W[W[W[W[W[, 
                  2][i], ], ][1, ][2], ][2], ][3]], dimnames(Bx)[[3]][W[W[W[W[, 
                  2][i], ], ][1, ][2], ][3]], dimnames(Bx)[[3]][W[W[, 
                  2][i], ][3]], dimnames(Bx)[[3]][W[, 3][i]], 
                sep = "")
        }
        rm(i)
        for (i in which(W[, 2] < (which(W[, 2] >= (which(W[, 
            2] >= (which(W[, 2] >= (which(W[, 2] >= (which(W[, 
            2] >= (which(W[, 2] >= (which(W[, 2] > ncol(E))[1]))[1]))[1]))[1]))[1])))[1])[1]))[(length(which(W[, 
            2] < (which(W[, 2] >= (which(W[, 2] >= (which(W[, 
            2] >= (which(W[, 2] >= (which(W[, 2] >= (which(W[, 
            2] > ncol(E))[1]))[1]))[1]))[1])))[1])[1]))) + 1):length(which(W[, 
            2] < (which(W[, 2] >= (which(W[, 2] >= (which(W[, 
            2] >= (which(W[, 2] >= (which(W[, 2] >= (which(W[, 
            2] >= (which(W[, 2] > ncol(E))[1]))[1]))[1]))[1]))[1])))[1])[1])))]) {
            lbl[(i)] <- paste(dimnames(Bx)[[3]][W[W[W[W[W[W[W[W[W[, 
                2][i], ], ][1, ][2], ][2], ][2], ][2], ][2], 
                ][2], ][2]], dimnames(Bx)[[3]][W[W[W[W[W[W[W[W[W[, 
                2][i], ], ][1, ][2], ][2], ][2], ][2], ][2], 
                ][2], ][3]], dimnames(Bx)[[3]][W[W[W[W[W[W[W[W[, 
                2][i], ], ][1, ][2], ][2], ][2], ][2], ][2], 
                ][3]], dimnames(Bx)[[3]][W[W[W[W[W[W[W[, 2][i], 
                ], ][1, ][2], ][2], ][2], ][2], ][3]], dimnames(Bx)[[3]][W[W[W[W[W[W[, 
                2][i], ], ][1, ][2], ][2], ][2], ][3]], dimnames(Bx)[[3]][W[W[W[W[W[, 
                2][i], ], ][1, ][2], ][2], ][3]], dimnames(Bx)[[3]][W[W[W[W[, 
                2][i], ], ][1, ][2], ][3]], dimnames(Bx)[[3]][W[W[, 
                2][i], ][3]], dimnames(Bx)[[3]][W[, 3][i]], sep = "")
        }
        rm(i)
        dimnames(Bx)[[3]] <- lbl
    }
    if (is.null(dimnames(x)[[1]]) == FALSE) 
        dimnames(Bx)[[1]] <- dimnames(Bx)[[2]] <- dimnames(x)[[1]]
    if (equat == TRUE) {
        gn <- dimnames(x)[[3]]
        w <- Bx
        luq <- list()
        length(luq) <- length(lbl)
        names(luq) <- lbl
        unq <- data.frame(matrix(ncol = (dim(w)[1] * dim(w)[2]), 
            nrow = 0))
        for (i in 1:dim(w)[3]) {
            ifelse(isTRUE(dim(w)[3] > 1) == TRUE, unq[i, ] <- as.vector(w[, 
                , i]), unq <- as.vector(w))
        }
        rm(i)
        rownames(unq) <- lbl
        if (isTRUE(TRUE %in% dpl) == TRUE) {
            eq <- list()
            length(eq) <- nrow(unique(tmpo))
            names(eq) <- rownames(unique(tmpo))
            rownames(tmpo) <- gener
            for (i in which(duplicated(tmpo))) {
                for (j in which(!(duplicated(tmpo)))) {
                  if (isTRUE(all(tmpo[i, ] == tmpo[j, ]) == TRUE)) {
                    eq[[which(attr(eq, "names") == rownames(tmpo[j, 
                      ]))]] <- append(eq[[which(attr(eq, "names") == 
                      rownames(tmpo[j, ]))]], rownames(tmpo)[i])
                  }
                }
                rm(j)
            }
            rm(i)
            for (i in 1:length(eq)) {
                if (isTRUE(is.null(eq[[i]])) == FALSE) {
                  luq[[which(names(eq)[i] == names(luq))]] <- append(luq[[which(names(eq)[i] == 
                    names(luq))]], eq[[i]])
                }
                else {
                  NA
                }
            }
            rm(i)
        }
        if (k > 1) {
            if (length(gn) > 1) {
                eq2 <- vector()
                for (i in 1:length(gn)) {
                  eq2 <- append(eq2, paste(gn[i], gn[i], sep = ""))
                }
                rm(i)
                db <- eq2
                for (i in 1:ncol(combn(gn, 2))) {
                  if (!(paste(combn(gn, 2)[, i][1], combn(gn, 
                    2)[, i][2], sep = "") %in% lbl)) 
                    eq2 <- append(eq2, paste(combn(gn, 2)[, i][1], 
                      combn(gn, 2)[, i][2], sep = ""))
                  if (!(paste(combn(gn, 2)[, i][2], combn(gn, 
                    2)[, i][1], sep = "") %in% lbl)) 
                    eq2 <- append(eq2, paste(combn(gn, 2)[, i][2], 
                      combn(gn, 2)[, i][1], sep = ""))
                }
                rm(i)
                eq2 <- unique(eq2)
                if (length(eq2) != 0) {
                  dbl <- data.frame(matrix(ncol = (dim(w)[1] * 
                    dim(w)[2]), nrow = 0))
                  for (i in 1:length(eq2)) {
                    dbl[(nrow(dbl) + 1), ] <- as.vector(dichot(x[, 
                      , which(dimnames(x)[[3]] == strsplit(eq2[i], 
                        "")[[1]][1])] %*% x[, , which(dimnames(x)[[3]] == 
                      strsplit(eq2[i], "")[[1]][2])]))
                  }
                  rm(i)
                  rownames(dbl) <- eq2
                  for (i in 1:nrow(dbl)) {
                    if (isTRUE(eq2[i] %in% rownames(unq)) == 
                      FALSE) {
                      luq[[which(duplicated(rbind(dbl[i, ], unq))) - 
                        1]] <- append(luq[[which(duplicated(rbind(dbl[i, 
                        ], unq))) - 1]], eq2[i])
                    }
                  }
                  rm(i)
                }
            }
        }
        if (k > 2) {
            eq3 <- vector()
            for (i in 1:length(gn)) {
                eq3 <- append(eq3, paste(db, gn[i], sep = ""))
                eq3 <- append(eq3, paste(gn[i], db, sep = ""))
            }
            rm(i)
            for (i in 1:length(gn)) {
                for (j in 1:length(gn)) {
                  eq3 <- append(eq3, paste(strsplit(db[j], "")[[1]][1], 
                    gn[i], strsplit(db[j], "")[[1]][2], sep = ""))
                }
                rm(j)
            }
            rm(i)
            if (length(gn) > 2) {
                for (i in 1:ncol(combn(gn, 3))) {
                  if (!(paste(combn(gn, 3)[, i][1], combn(gn, 
                    3)[, i][2], combn(gn, 3)[, i][3], sep = "") %in% 
                    lbl)) 
                    eq3 <- append(eq3, paste(combn(gn, 3)[, i][1], 
                      combn(gn, 3)[, i][2], combn(gn, 3)[, i][3], 
                      sep = ""))
                  if (!(paste(combn(gn, 3)[, i][1], combn(gn, 
                    3)[, i][3], combn(gn, 3)[, i][2], sep = "") %in% 
                    lbl)) 
                    eq3 <- append(eq3, paste(combn(gn, 3)[, i][1], 
                      combn(gn, 3)[, i][3], combn(gn, 3)[, i][2], 
                      sep = ""))
                  if (!(paste(combn(gn, 3)[, i][2], combn(gn, 
                    3)[, i][1], combn(gn, 3)[, i][3], sep = "") %in% 
                    lbl)) 
                    eq3 <- append(eq3, paste(combn(gn, 3)[, i][2], 
                      combn(gn, 3)[, i][1], combn(gn, 3)[, i][3], 
                      sep = ""))
                  if (!(paste(combn(gn, 3)[, i][2], combn(gn, 
                    3)[, i][3], combn(gn, 3)[, i][1], sep = "") %in% 
                    lbl)) 
                    eq3 <- append(eq3, paste(combn(gn, 3)[, i][2], 
                      combn(gn, 3)[, i][3], combn(gn, 3)[, i][1], 
                      sep = ""))
                  if (!(paste(combn(gn, 3)[, i][3], combn(gn, 
                    3)[, i][1], combn(gn, 3)[, i][2], sep = "") %in% 
                    lbl)) 
                    eq3 <- append(eq3, paste(combn(gn, 3)[, i][3], 
                      combn(gn, 3)[, i][1], combn(gn, 3)[, i][2], 
                      sep = ""))
                  if (!(paste(combn(gn, 3)[, i][3], combn(gn, 
                    3)[, i][2], combn(gn, 3)[, i][1], sep = "") %in% 
                    lbl)) 
                    eq3 <- append(eq3, paste(combn(gn, 3)[, i][3], 
                      combn(gn, 3)[, i][2], combn(gn, 3)[, i][1], 
                      sep = ""))
                }
                rm(i)
            }
            eq3 <- unique(eq3)
            if (length(eq3) != 0) {
                tpl <- data.frame(matrix(ncol = (dim(w)[1] * 
                  dim(w)[2]), nrow = 0))
                for (i in 1:length(eq3)) {
                  tpl[(nrow(tpl) + 1), ] <- as.vector(dichot(x[, 
                    , which(dimnames(x)[[3]] == strsplit(eq3[i], 
                      "")[[1]][1])] %*% x[, , which(dimnames(x)[[3]] == 
                    strsplit(eq3[i], "")[[1]][2])] %*% x[, , 
                    which(dimnames(x)[[3]] == strsplit(eq3[i], 
                      "")[[1]][3])]))
                }
                rm(i)
                rownames(tpl) <- eq3
                for (i in 1:nrow(tpl)) {
                  if (isTRUE(eq3[i] %in% rownames(unq)) == FALSE) {
                    luq[[which(duplicated(rbind(tpl[i, ], unq))) - 
                      1]] <- append(luq[[which(duplicated(rbind(tpl[i, 
                      ], unq))) - 1]], eq3[i])
                  }
                }
                rm(i)
            }
        }
        rm(unq, gn, w)
        lqu <- list()
        lqlb <- vector()
        for (i in 1:length(luq)) {
            if (isTRUE(is.null(luq[[i]])) == FALSE) {
                lqu[[length(lqu) + 1]] <- luq[[i]]
                lqlb[length(lqlb) + 1] <- attr(luq, "names")[i]
            }
        }
        rm(i)
        names(lqu) <- lqlb
        if (length(lqu) != 0) {
            for (i in 1:length(lqu)) {
                lqu[[i]] <- c(attr(lqu, "names")[i], lqu[[i]])
                lqu[[i]] <- unique(lqu[[i]])
            }
            rm(i)
        }
        else if (length(lqu) == 0) {
            lqu <- paste("No equations were produced in words with length", 
                k, "or less", sep = " ")
        }
    }
    if (equat == TRUE) {
        ifelse(isTRUE(length(note) > 0) == TRUE, lst <- list(wt = Bx, 
            ord = dim(Bx)[3], st = lbl, equat = lqu, Note = note), 
            lst <- list(wt = Bx, ord = dim(Bx)[3], st = lbl, 
                equat = lqu))
    }
    else if (equat != TRUE) {
        lst <- list(wt = Bx, ord = dim(Bx)[3], st = lbl)
    }
    class(lst) <- "Strings"
    return(lst)
}
