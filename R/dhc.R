dhc <-
function (x, prsep = ", ") 
{
    if (isTRUE(length(x) != 0) == TRUE && isTRUE(is.na(x)) == 
        FALSE) {
        if (isTRUE(is.list(x)) == TRUE) {
            for (i in 1:length(x)) ifelse(isTRUE(length(x[[i]]) != 
                0) == TRUE, x[[i]] <- strsplit(x[[i]], prsep)[[1]], 
                NA)
            return(x)
        }
        else if (isTRUE(is.list(x)) == FALSE) {
            Lt <- list()
            for (i in 1:length(x)) Lt[[length(Lt) + 1]] <- strsplit(levels(factor(x[i])), 
                prsep)[[1]]
            return(Lt[[1]])
        }
    }
    else {
        x
    }
}
