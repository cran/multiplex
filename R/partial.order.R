partial.order <-
function(x, labels = NULL)
{
    if(isTRUE(attr(x, "class")=="Strings")==FALSE) {
        warning("\"x\" should be an object of a \"Strings\" class.")
    } else { x<-x$wt }
    if (is.array(x) == FALSE) 
        stop("Data must be a stacked array of square matrices, like a product of 'strings'.")

    if (is.na(dim(x)[3]) == FALSE) {
        tmp <- data.frame(matrix(ncol = (dim(x)[1] * dim(x)[2]), 
            nrow = 0))
        for (i in 1:dim(x)[3]) {
            tmp[i, ] <- as.vector(x[, , i])
        }
        rm(i)
        po <- as.matrix(array(0, dim = c(dim(x)[3], dim(x)[3])))
        for (j in 1:dim(x)[3]) {
            for (i in 1:dim(x)[3]) {
                if ((as.numeric(any(tmp[i, ] < tmp[j, ])) == 
                  1 && as.numeric(any(tmp[j, ] < tmp[i, ])) == 
                  0) | as.numeric(all(tmp[i, ] == tmp[j, ])) == 
                  1) 
                  po[i, j] <- 1
            }
        }
        rm(i, j)
        rownames(po) <- colnames(po) <- dimnames(x)[[3]]
    }
    else if (is.na(dim(x)[3]) == TRUE) {
        po <- 1
    }
#    if (is.null(dimnames(x)[[3]]) == FALSE) 
    if (is.null(labels) == FALSE) 
        dimnames(po)[[2]] <- dimnames(po)[[1]] <- labels
    return(POT = po)
}
