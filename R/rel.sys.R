rel.sys <-
function (x, bonds = c("entire", "strong", "weak"), prsep = ", ") 
{
    bd <- bundles(x, collapse = FALSE, loops = FALSE, prsep = prsep)
    switch(match.arg(bonds), entire = lbd <- bd, strong = lbd <- list(bd$recp, 
        bd$txch, bd$mixed, bd$full), weak = lbd <- list(bd$asym, 
        bd$tent))
    if (is.na(dim(x)[3]) == FALSE) {
        stb <- list()
        for (k in 1:dim(x)[3]) {
            tmp <- vector()
            for (i in 1:length(lbd)) {
                tmp <- append(tmp, lbd[[i]][k])
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
    if (length(stb) > 0) {
        tmp <- vector()
        for (k in 1:length(stb)) {
            for (i in 1:length(stb[[k]])) {
                if (isTRUE(length(stb[[k]]) > 0) == TRUE) {
                  tmp <- append(tmp, dhc(stb[[k]][i], prsep = prsep))
                }
            }
            rm(i)
        }
        rm(k)
    }
    else {
        tmp <- stb <- logical(0)
    }
    if (is.na(dim(x)[3]) == FALSE) {
        ifelse(is.null(dimnames(x)[[3]]) == FALSE, attr(stb, 
            "names") <- dimnames(x)[[3]], attr(stb, "names") <- 1:dim(x)[3])
    }
    if (is.null(dimnames(x)[[1]]) == TRUE) {
        note <- "Input labels in x are NULL."
        lbs <- 1:dim(x)[1]
    }
    else {
        note <- NULL
        lbs <- dimnames(x)[[1]]
    }
    class(stb) <- "Rel.System"
    ifelse(isTRUE(length(note) > 0) == TRUE, return(list(ord = dim(x)[1], 
        nodes = lbs, sys.ord = nlevels(factor(tmp)), incl = lbs[which(lbs %in% 
            levels(factor(tmp)))], excl = lbs[which(!(lbs %in% 
            levels(factor(tmp))))], bond.type = bonds, size = length(unlist(stb)), 
        Note = note, ties = stb)), return(list(ord = dim(x)[1], 
        nodes = lbs, sys.ord = nlevels(factor(tmp)), incl = lbs[which(lbs %in% 
            levels(factor(tmp)))], excl = lbs[which(!(lbs %in% 
            levels(factor(tmp))))], bond.type = bonds, size = length(unlist(stb)), 
        ties = stb)))
}
