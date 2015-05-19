fltr <-
function (e, PO, rclos = TRUE) 
{
    if (isTRUE(e > nrow(PO)) == TRUE) 
        stop("'e' is greater than the size of the partial order")
    pfltr <- vector()
    for (i in 1:nrow(PO)) {
        if (isTRUE(PO[e, i] == 1) == TRUE && isTRUE(PO[i, e] == 
            0) == TRUE) {
            pfltr <- append(pfltr, i)
        }
        else {
            NA
        }
        if (rclos) {
            if (isTRUE(e == i) == TRUE) {
                pfltr <- append(pfltr, i)
            }
        }
    }
    rm(i)
    return(pfltr)
}
