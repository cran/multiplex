read.dl <-
function (file) 
{
    arx <- scan(file, what = "character", nlines = -1, quiet = TRUE)
    if (isTRUE(arx[1] == "DL") == FALSE) 
        stop("Input file must have a DL format.")
    tip <- dhc(arx[2:3], prsep = "=")
    if (isTRUE(tip[1] == "N") == TRUE) {
        if (any(tip == "NM") == TRUE) {
            arr <- array(NA, dim = c(as.numeric(rep(tip[2], 2)), 
                as.numeric(tip[4])))
            dimnames(arr)[[1]] <- arx[(which(arx == "LABELS:")[1] + 
                1L):(which(arx == "LABELS:")[1] + dim(arr)[1])]
            dimnames(arr)[[2]] <- arx[(which(arx == "LABELS:")[2] + 
                1L):(which(arx == "LABELS:")[2] + dim(arr)[2])]
            dimnames(arr)[[3]] <- arx[(which(arx == "LABELS:")[3] + 
                1L):(which(arx == "LABELS:")[3] + dim(arr)[3])]
            lnch <- which(arx == "DATA:")
            lgtv <- dim(arr)[1]^2L
            arrt <- arr
            for (k in 1:dim(arr)[3]) {
                arrt[, , k] <- as.numeric(arx[(lnch + 1):(lgtv + 
                  lnch)])
                arr[, , k] <- t(arrt[, , k])
                lnch <- (lgtv + lnch)
            }
            rm(k, arrt, lnch, lgtv)
        }
        else if (any(tip == "NM") == FALSE) {
            arrt <- array(as.numeric(arx[(which(arx == "DATA:") + 
                1L):length(arx)]), dim = c(as.numeric(rep(tip[2], 
                2))))
            arr <- t(arrt)
            dimnames(arr)[[1]] <- dimnames(arr)[[2]] <- arx[(which(arx == 
                "LABELS:")[1] + 1L):(which(arx == "LABELS:")[1] + 
                as.numeric(tip[2]))]
        }
        else {
            NA
        }
    }
    else if (isTRUE(tip[1] == "NR") == TRUE) {
        arrt <- data.frame(matrix(as.numeric(arx[(which(arx == 
            "DATA:") + 1L):length(arx)]), ncol = as.numeric(tip[which(tip == 
            "NR") + 1]), nrow = as.numeric(tip[which(tip == "NC") + 
            1])))
        arr <- t(arrt)
        colnames(arr) <- arx[(which(arx == "COLUMN") + 2L):((which(arx == 
            "COLUMN") + 1L + ncol(arr)))]
        rownames(arr) <- arx[(which(arx == "ROW") + 2L):((which(arx == 
            "ROW") + 1L + nrow(arr)))]
    }
    else {
        stop("Format not supported.")
    }
    arr
}
