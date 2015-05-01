ngbs <-
function (x, type = c("und", "dir")) 
{
    x <- as.list(unlist(x))
    nx <- vector()
    for (i in 1:length(x)) {
        if (isTRUE(length(x[[i]]) > 0) == TRUE) {
            for (j in 1:length(x[[i]])) {
                nx <- append(nx, strsplit(x[[i]][j], ", ")[[1]][1])
                switch(match.arg(type), und = nx <- append(nx, 
                  strsplit(x[[i]][j], ", ")[[1]][2]), dir = NA)
            }
            rm(j)
        }
    }
    rm(i)
    return(levels(factor(nx)))
}
