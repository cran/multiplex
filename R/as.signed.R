as.signed <-
function (x) 
{
    if (is.array(x) == FALSE) 
        stop("Data must be an array")
    lst <- list(val = levels(factor(x)), s = x)
    class(lst) <- "Signed"
    return(lst)
}
