diagram <-
function(x, unord = TRUE, attrs = NULL, main = NULL, cex.main = par()$cex.main)
{
    if (require("Rgraphviz", quietly = TRUE, warn.conflicts = FALSE)) {
        if (is.null(dimnames(x)[[1]]) == TRUE) 
            rownames(x) <- colnames(x) <- as.character(as.roman(c(1:dim(x)[1])))
        if (is.null(attrs) == TRUE) 
            attrs = list(graph = list(rankdir = "BT"), edge = list(arrowsize = "0", 
                minlen = "1"), node = list(shape = "rectangle", 
                color = "white", fixedsize = FALSE))
        po <- x & (1 - t(x))
        diag(po) <- 0
        for (i in seq_len(ncol(po))) {
            tmp <- outer(po[, i], po[i, ], pmin.int)
            po <- pmin(po, (1 - tmp))
        }
        rm(tmp)
        if (unord == FALSE) {
            px <- po
            out <- vector()
            k <- 1
            for (i in 1:nrow(px)) {
                if (sum(px[i, ] + px[, i]) == 0) {
                  out[k] <- i
                  k <- k + 1
                }
            }
            rm(i)
            d <- nrow(px)
            for (j in out) {
                for (k in 1:d) {
                  px[, j] <- px[j, ] <- NA
                }
            }
            rm(j)
            lb <- dimnames(px)[[1]]
            for (l in out) {
                lb[l] <- NA
            }
            rm(l)
            npx <- data.frame(matrix(0, ncol = (nrow(px) - length(out)), 
                nrow = 0))
            colnames(npx) <- as.vector(na.exclude(lb))
            for (i in 1:d) {
                ifelse(isTRUE(all(is.na(px[i, ])) == FALSE) == 
                  TRUE, npx[i, ] <- as.vector(na.exclude(px[i, 
                  ])), NA)
            }
            rm(i)
            po <- as.matrix(na.exclude(npx))
            dimnames(po)[[1]] <- dimnames(po)[[2]] <- as.vector(na.exclude(lb))
        }
        plot(as(po, "graphNEL"), attrs = attrs, main = main, 
            cex.main = cex.main)
    }
    else stop("Package 'Rgraphviz' needs to be properly installed.")
}
