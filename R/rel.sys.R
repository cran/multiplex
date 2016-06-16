rel.sys <-
function (x, type = c("matlist", "listmat"), bonds = c("entire", 
    "strong", "weak", "asym", "recp", "txch", "tent", "mixd", 
    "full"), sel = NULL, prsep = ", ", loops = FALSE, att = NULL) 
{
    if (isTRUE(att == 0L) == TRUE) {
        att <- NULL
    }
    else {
        NA
    }
    if (match.arg(type) == "matlist") {
        if (!inherits(x, "array")) 
            stop("\"x\" should be an array.")
        if (is.null(att) == FALSE) {
            if (is.na(dim(x)[3]) == FALSE) {
                if (isTRUE(max(att) > dim(x)[3]) == TRUE) 
                  stop("Value of 'attr' greater than dim(x)[3]")
            }
            else if (is.na(dim(x)[3]) == TRUE) {
                if (isTRUE(max(att) > 1L) == TRUE) 
                  stop("Value of 'attr' greater than dim(x)[3]")
            }
            ats <- bundles(x, collapse = FALSE, loops = TRUE, 
                prsep = prsep)[[7]][att]
        }
        else if (is.null(att) == TRUE) {
            ats <- logical(0)
        }
        if (is.na(dim(x)[3]) == FALSE) {
            if (isTRUE(all(seq(dim(x)[3]) %in% att)) == FALSE) {
                bd <- bundles(x[, , which(!(seq(dim(x)[3]) %in% 
                  att))], collapse = FALSE, loops = loops, prsep = prsep)
            }
            else if (isTRUE(all(seq(dim(x)[3]) %in% att)) == 
                TRUE) {
                bd <- NULL
            }
        }
        else {
            bd <- bundles(x, collapse = FALSE, loops = loops, 
                prsep = prsep)
        }
        switch(match.arg(bonds), entire = lbd <- bd, strong = lbd <- list(bd$recp, 
            bd$txch, bd$mixd, bd$full), weak = lbd <- list(bd$asym, 
            bd$tent), asym = lbd <- list(bd$asym), recp = lbd <- list(bd$recp), 
            txch = lbd <- list(bd$txch), tent = lbd <- list(bd$tent), 
            mixd = lbd <- list(bd$mixd), full = lbd <- list(bd$full))
        if (is.null(lbd) == FALSE) {
            if (is.na(dim(x)[3]) == FALSE && isTRUE((dim(x)[3] - 
                length(att)) == 0L) == FALSE) {
                stb <- list()
                for (k in 1:(dim(x)[3] - length(att))) {
                  tmp <- vector()
                  for (i in 1:length(lbd)) {
                    if (isTRUE(length(lbd[[i]]) > 0L) == TRUE) {
                      ifelse(is.na(dim(x[, , which(!(seq(dim(x)[3]) %in% 
                        att))])[3]) == TRUE, tmp <- append(tmp, 
                        lbd[[i]]), tmp <- append(tmp, lbd[[i]][k]))
                    }
                  }
                  rm(i)
                  stb[[k]] <- as.vector(unlist(tmp))
                }
                rm(k)
            }
            else {
                stb <- vector()
                for (i in 1:length(lbd)) {
                  stb <- append(stb, lbd[[i]])
                }
                rm(i)
            }
        }
        else {
            stb <- lbd
        }
        if (is.null(sel) == FALSE) {
            if (is.array(sel) == TRUE) {
                ifelse(is.na(dim(sel)[3]) == TRUE | isTRUE(dim(sel)[3] == 
                  1L) == TRUE, sel <- diag(sel), sel <- diag(mnplx(sel)))
                sel <- as.vector(attr(which(!(sel == 0)), "names"))
            }
            ntsel <- list()
            length(ntsel) <- length(stb)
            for (k in 1:length(stb)) {
                tss <- which(dhc(stb[[k]]) %in% sel)
                if (isTRUE(length(tss) > 0) == TRUE) {
                  tmpsel <- vector()
                  for (i in 1:length(tss)) {
                    if (isTRUE((tss[i]%%2L) == 1L) == TRUE) {
                      tmpsel <- append(tmpsel, stb[[k]][ceiling(tss[i]/2L)])
                    }
                    else if (isTRUE((tss[i]%%2L) == 1L) == FALSE) {
                      tmpsel <- append(tmpsel, stb[[k]][floor(tss[i]/2L)])
                    }
                  }
                  rm(i)
                  ntsel[[k]] <- unique(as.vector(unlist(tmpsel)))
                }
            }
            rm(k)
            rm(tmpsel, tss)
            stb <- ntsel
        }
        else {
            NA
        }
        if (length(stb) > 0L) {
            ties <- vector()
            for (k in 1:length(stb)) {
                for (i in 1:length(stb[[k]])) {
                  if (isTRUE(length(stb[[k]]) > 0L) == TRUE) {
                    ties <- append(ties, dhc(stb[[k]][i], prsep = prsep))
                  }
                }
                rm(i)
            }
            rm(k)
        }
        else {
            ties <- stb <- character(0)
        }
        if (is.null(att) == TRUE) {
            if (is.na(dim(x)[3]) == FALSE) {
                ifelse(is.null(dimnames(x)[[3]]) == TRUE, attr(stb, 
                  "names") <- 1:(dim(x)[3] - length(att)), attr(stb, 
                  "names") <- dimnames(x)[[3]])
            }
        }
        else {
            ifelse(is.null(dimnames(x)[[3]]) == TRUE, attr(stb, 
                "names") <- which(!(seq(dim(x)[3]) %in% att)), 
                attr(stb, "names") <- dimnames(x)[[3]][which(!(seq(dim(x)[3]) %in% 
                  att))])
        }
        if (is.null(dimnames(x)[[1]]) == TRUE) {
            note <- "Input labels in 'x' are NULL."
            lbs <- 1:dim(x)[1]
        }
        else {
            note <- NULL
            lbs <- dimnames(x)[[1]]
        }
        if (isTRUE(length(ats) > 0L) == TRUE) {
            ifelse(length(note) > 0L, RS <- (list(ord = dim(x)[1], 
                nodes = lbs, sel = sel, sys.ord = nlevels(factor(ties)), 
                incl = lbs[which(lbs %in% levels(factor(ties)))], 
                excl = lbs[which(!(lbs %in% levels(factor(ties))))], 
                bond.type = bonds, size = length(unlist(stb)), 
                Note = note, prsep = prsep, Ties = stb, Attrs.ord = length(unlist(ats)), 
                Attrs = jnt(dhc(ats, prsep = prsep), prsep = prsep))), 
                RS <- (list(ord = dim(x)[1], nodes = lbs, sel = sel, 
                  sys.ord = nlevels(factor(ties)), incl = lbs[which(lbs %in% 
                    levels(factor(ties)))], excl = lbs[which(!(lbs %in% 
                    levels(factor(ties))))], bond.type = bonds, 
                  size = length(unlist(stb)), prsep = prsep, 
                  Ties = stb, Attrs.ord = length(unlist(ats)), 
                  Attrs = jnt(dhc(ats, prsep = prsep), prsep = prsep))))
        }
        else {
            ifelse(isTRUE(length(note) > 0L) == TRUE, RS <- (list(ord = dim(x)[1], 
                nodes = lbs, sel = sel, sys.ord = nlevels(factor(ties)), 
                incl = lbs[which(lbs %in% levels(factor(ties)))], 
                excl = lbs[which(!(lbs %in% levels(factor(ties))))], 
                bond.type = bonds, size = length(unlist(stb)), 
                Note = note, prsep = prsep, Ties = stb)), RS <- (list(ord = dim(x)[1], 
                nodes = lbs, sel = sel, sys.ord = nlevels(factor(ties)), 
                incl = lbs[which(lbs %in% levels(factor(ties)))], 
                excl = lbs[which(!(lbs %in% levels(factor(ties))))], 
                bond.type = bonds, size = length(unlist(stb)), 
                prsep = prsep, Ties = stb)))
        }
        class(RS) <- "Rel.System"
        return(RS)
    }
    else if (match.arg(type) == "listmat") {
        if (isTRUE(attr(x, "class") == "Rel.System") == FALSE) 
            stop("Relational system must be a \"Rel.System\" class.")
        if (isTRUE(x$sys.ord == 0L) == TRUE) 
            stop("Relational system chosen is empty!")
        if (isTRUE(bonds != "entire") == TRUE) 
            warning("'bonds' is only for the \"matlist\" option.")
        lbst <- attr(x$Ties, "names")
        if (is.null(sel) == FALSE) {
            if (isTRUE(sel == 1L) == TRUE) {
                sel <- x$nodes[which(x$nodes %in% unlist(dhc(x$Attrs)))]
            }
            else if (isTRUE(sel == 0L) == TRUE) {
                sel <- x$nodes[which(!(x$nodes %in% unlist(dhc(x$Attrs))))]
            }
            else if (isTRUE(any(sel %in% x$nodes)) == FALSE) {
                return(sel)
            }
            else {
                NA
            }
            ntsel <- list()
            for (k in 1:length(x$Ties)) {
                tss <- which(dhc(x$Ties[[k]]) %in% sel)
                if (isTRUE(length(tss) > 0) == TRUE) {
                  tmpsel <- vector()
                  for (i in 1:length(tss)) {
                    if (isTRUE((tss[i]%%2L) == 1L) == TRUE) {
                      tmpsel <- append(tmpsel, x$Ties[[k]][ceiling(tss[i]/2L)])
                    }
                    else {
                      tmpsel <- append(tmpsel, x$Ties[[k]][floor(tss[i]/2L)])
                    }
                  }
                  rm(i)
                  ntsel[[k]] <- as.vector(unlist(tmpsel))
                }
                else {
                  lbst <- lbst[which(!(lbst %in% lbst[k]))]
                }
            }
            rm(k)
            rm(tss)
            ntsel <- ntsel[unlist(lapply(ntsel, length) != 0)]
            x$Ties <- ntsel
            lbs <- unique(dhc(unlist(ntsel)))
            n <- length(lbs)
            r <- length(lbst)
        }
        else if (is.null(sel) == TRUE) {
            n <- x$sys.ord
            r <- length(x$Ties)
            lbs <- x$incl
        }
        arr <- array(0, dim = c(n, n, r))
        for (i in 1:r) {
            if (isTRUE(length(x$Ties[[i]]) > 0) == TRUE && isTRUE(n > 
                0) == TRUE) {
                arr[, , i] <- transf(x$Ties[[i]], type = "listmat", 
                  ord = n, labels = lbs)
            }
            else {
                NA
            }
        }
        rm(i)
        dimnames(arr)[[1]] <- dimnames(arr)[[2]] <- lbs
        if (isTRUE(n > 0) == TRUE) 
            dimnames(arr)[[3]] <- lbst
        if (is.null(x$Attrs) == FALSE) {
            arra <- array(0, dim = c(n, n, length(x$Attrs)))
            dimnames(arra)[[1]] <- dimnames(arra)[[2]] <- lbs
            if (isTRUE(n > 0) == TRUE) 
                dimnames(arra)[[3]] <- attr(x$Attrs, "names")
            for (i in 1:length(x$Attrs)) {
                act <- dhc(x$Attrs[[i]], prsep = prsep)
                if (isTRUE(length(act) > 0) == TRUE) {
                  diag(arra[, , i])[which(lbs %in% dhc(x$Attrs[[i]]))] <- 1L
                }
            }
            rm(i)
            attrs <- dim(arr)[3]
            arr <- zbind(arr, arra)
            if (isTRUE(dim(arra)[3] > 1) == TRUE) {
                class(arr) <- c("array", paste("Attrs.", paste(attrs + 
                  1L, dim(arr)[3], sep = ","), sep = " : "))
            }
            else {
                class(arr) <- c("array", paste("Attrs.", dim(arr)[3], 
                  sep = " : "))
            }
        }
        return(arr)
    }
}
