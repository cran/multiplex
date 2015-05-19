dhc <-
function (x, prsep = ", ") 
{
    if (isTRUE(length(x) != 0) == TRUE && isTRUE(is.na(x)) == 
        FALSE) {
        Lt <- FALSE
        ifelse(isTRUE(is.list(x)) == TRUE, Lt <- TRUE, x <- as.list(x))
        for (i in 1:length(x)) ifelse(isTRUE(length(x[[i]]) != 
            0) == TRUE, x[[i]] <- strsplit(x[[i]], prsep)[[1]], 
            NA)
        ifelse(isTRUE(Lt) == TRUE, return(x), return(unlist(x)))
    }
    else {
        x
    }
}
