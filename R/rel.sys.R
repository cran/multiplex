rel.sys <-
function (x, bonds = c("entire", "strong", "weak"), prsep = ", ", 
    loops = FALSE, att = NULL) 
{
    if (isTRUE(att == 0L) == TRUE) {
        att <- NULL
    }
    else {
        NA
    }
    if (is.na(dim(x)[3]) == FALSE) {
        if (isTRUE(all(seq(dim(x)[3]) %in% att)) == FALSE) {
            bd <- bundles(x[, , which(!(seq(dim(x)[3]) %in% att))], 
                collapse = FALSE, loops = loops, prsep = prsep)
        }
        else if (isTRUE(all(seq(dim(x)[3]) %in% att)) == TRUE) {
            bd <- NULL
        }
    }
    else {
        bd <- bundles(x, collapse = FALSE, loops = loops, prsep = prsep)
    }
    if (is.null(att) == FALSE) {
        if (is.na(dim(x)[3]) == FALSE) {
            if (isTRUE(max(att) > dim(x)[3]) == TRUE) 
                stop("Value of 'att' greater than dim(x)[3]")
        }
        else if (is.na(dim(x)[3]) == TRUE) {
            if (isTRUE(max(att) > 1L) == TRUE) 
                stop("Value of 'att' greater than dim(x)[3]")
        }
        ats <- bundles(x, collapse = FALSE, loops = TRUE, prsep = prsep)[[7]][att]
    }
    else if (is.null(att) == TRUE) {
        ats <- logical(0)
    }
    switch(match.arg(bonds), entire = lbd <- bd, strong = lbd <- list(bd$recp, 
        bd$txch, bd$mixed, bd$full), weak = lbd <- list(bd$asym, 
        bd$tent))
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
    if (length(stb) > 0L) {
        tmp <- vector()
        for (k in 1:length(stb)) {
            for (i in 1:length(stb[[k]])) {
                if (isTRUE(length(stb[[k]]) > 0L) == TRUE) {
                  tmp <- append(tmp, dhc(stb[[k]][i], prsep = prsep))
                }
            }
            rm(i)
        }
        rm(k)
    }
    else {
        tmp <- stb <- character(0)
    }
    if (is.null(att) == TRUE) {
        if (is.na(dim(x)[3]) == FALSE) {
            ifelse(is.null(dimnames(x)[[3]]) == TRUE, attr(stb, 
                "names") <- 1:(dim(x)[3] - length(att)), attr(stb, 
                "names") <- dimnames(x)[[3]])
        }
    }
    else {
        ifelse(is.null(dimnames(x)[[3]]) == TRUE, attr(stb, "names") <- which(!(seq(dim(x)[3]) %in% 
            att)), attr(stb, "names") <- dimnames(x)[[3]][which(!(seq(dim(x)[3]) %in% 
            att))])
    }
    if (is.null(dimnames(x)[[1]]) == TRUE) {
        note <- "Input labels in x are NULL."
        lbs <- 1:dim(x)[1]
    }
    else {
        note <- NULL
        lbs <- dimnames(x)[[1]]
    }
    if (isTRUE(length(ats) > 0L) == TRUE) {
        ifelse(length(note) > 0L, RS <- (list(ord = dim(x)[1], 
            nodes = lbs, sys.ord = nlevels(factor(tmp)), incl = lbs[which(lbs %in% 
                levels(factor(tmp)))], excl = lbs[which(!(lbs %in% 
                levels(factor(tmp))))], bond.type = bonds, size = length(unlist(stb)), 
            Note = note, prsep = prsep, Ties = stb, Attrs.ord = length(unlist(ats)), 
            Attrs = jnt(dhc(ats, prsep = prsep), prsep = prsep))), 
            RS <- (list(ord = dim(x)[1], nodes = lbs, sys.ord = nlevels(factor(tmp)), 
                incl = lbs[which(lbs %in% levels(factor(tmp)))], 
                excl = lbs[which(!(lbs %in% levels(factor(tmp))))], 
                bond.type = bonds, size = length(unlist(stb)), 
                prsep = prsep, Ties = stb, Attrs.ord = length(unlist(ats)), 
                Attrs = jnt(dhc(ats, prsep = prsep), prsep = prsep))))
    }
    else {
        ifelse(isTRUE(length(note) > 0L) == TRUE, RS <- (list(ord = dim(x)[1], 
            nodes = lbs, sys.ord = nlevels(factor(tmp)), incl = lbs[which(lbs %in% 
                levels(factor(tmp)))], excl = lbs[which(!(lbs %in% 
                levels(factor(tmp))))], bond.type = bonds, size = length(unlist(stb)), 
            Note = note, prsep = prsep, Ties = stb)), RS <- (list(ord = dim(x)[1], 
            nodes = lbs, sys.ord = nlevels(factor(tmp)), incl = lbs[which(lbs %in% 
                levels(factor(tmp)))], excl = lbs[which(!(lbs %in% 
                levels(factor(tmp))))], bond.type = bonds, size = length(unlist(stb)), 
            prsep = prsep, Ties = stb)))
    }
    class(RS) <- "Rel.System"
    return(RS)
}
