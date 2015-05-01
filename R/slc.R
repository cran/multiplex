slc <-
function (w, slct) 
{
    if (isTRUE(is.data.frame(w) == TRUE) == TRUE) {
        sw <- w[, which(colnames(w) %in% slct)]
    }
    else if (isTRUE(is.vector(w) == TRUE) == TRUE) {
        sw <- w[which(attr(w, "names") %in% slct)]
    }
    else {
        sw <- list()
        for (k in 1:length(w)) {
            tmp <- vector()
            if (length(w[[k]]) > 0) {
                for (n in 1:length(slct)) {
                  for (j in 1:length(w[[k]])) {
                    if (slct[n] %in% c(c(strsplit(w[[k]][j], 
                      ", ")[[1]][1], strsplit(w[[k]][j], ", ")[[1]][2]))) {
                      tmp <- append(tmp, w[[k]][j])
                    }
                  }
                  rm(j)
                }
                rm(n)
            }
            sw[[k]] <- as.vector(unlist(tmp))
        }
        rm(k)
        attr(sw, "names") <- attr(w, "names")
    }
    return(sw)
}
