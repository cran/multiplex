transf <-
function(x, type = c("matlist", "listmat"), lb2lb = FALSE, labels = NULL, prsep = ", ", ord = NULL) #
{
    if(isTRUE(is.character(x) == TRUE) == TRUE) type <- "listmat"
    #
    if(match.arg(type) == "matlist") {
        if(isTRUE(is.matrix(x) == TRUE) == FALSE) stop("Input data has to have a matrix format.")
	    if(isTRUE(lb2lb == TRUE)== TRUE) {
		if(is.null(labels)==TRUE) {
			if(is.null(dimnames(x)[[1]])==TRUE) stop("To use the \"lb2lb\" option you need to specify the labels.")
				labels <- dimnames(x)[[1]]
		}
             } #else { if(is.null(labels)==TRUE) labels<-1:dim(x)[1] }
        ## LA MATRIZ TIENE QUE SER DICÓTOMA
#        x<-dichot(x)
        #
        if(isTRUE(sum(x) > 0) == TRUE) {
            inc <- list()
            rws <- vector()
            cls <- vector()
	    for (k in 1:max(x)) {
	    X <- dichot(x, c = k)
            for (i in 1:length(which((X) == 1))) {
                cls[i] <- (ceiling(which((X) == 1)/dim(x)[1]))[i]
                ifelse((which((X) == 1)%%dim(x)[1])[i] == 0, 
                  rws[i] <- (which((X) == 1)%%dim(x)[1])[i] + 
                    dim(x)[1], rws[i] <- (which((X) == 1)%%dim(x)[1])[i])
                ifelse(isTRUE(lb2lb == TRUE)== TRUE, inc[[length(inc)+1]] <- paste(labels[rws[i]], labels[cls[i]], sep = prsep), 
                                                     inc[[length(inc)+1]] <- paste(rws[i], cls[i], sep = prsep))
###                inc[[i]] <- paste(rws[i], cls[i], sep = prsep)
            }; rm(i)
	    }; rm(k)
            return(sort(unlist(inc)))
        }
        else {
            return(paste(0, 0, sep = prsep))
        }
    }
    #
    if(match.arg(type) == "listmat") {
        if(is.character(x) == FALSE) stop("Input data has to be a list with character format.")
	    if(isTRUE(lb2lb == TRUE)== TRUE) {
#                if(is.null(labels)==TRUE) stop("To use the \"lb2lb\" option you need to specify the labels.")
                }
#
#	lb <- vector()
#	for (i in 1:length(x)) lb[length(lb)+1] <- strsplit(x[i], ", ")[[1]][1]; lb[length(lb)+1] <- strsplit(x[i], ", ")[[1]][2] 
#
        if(isTRUE(is.null(labels)==TRUE)==TRUE) {
        if(isTRUE(is.null(ord) == FALSE)==TRUE) { mat <- matrix(0, ncol = ord, nrow = ord) } else if(isTRUE(is.null(ord) == TRUE)==TRUE) {
            suppressWarnings(
	    ifelse(isTRUE(is.na(as.numeric(unlist(strsplit(levels(factor(unlist(x))),prsep))))==TRUE)==FALSE,
	    dm <- length(levels(factor(unlist(strsplit(levels(factor(unlist(x))),prsep))))),
	    dm <- max(as.numeric(unlist(strsplit(levels(factor(unlist(x))), prsep)))) ) #nlevels(factor(lb)) )
	    )
	    mat <- matrix(0, nrow = dm, ncol = dm)
	}
        #
        tmp<-as.list(unlist(x))
        nx<-vector()
        for(i in 1:length(tmp)) {
                if(isTRUE(length(tmp[[i]])>0)==TRUE) {
                for(j in 1:length(tmp[[i]])) { 
                        nx<-append(nx,strsplit(tmp[[i]][j],prsep)[[1]][1]) 
                        nx<-append(nx,strsplit(tmp[[i]][j],prsep)[[1]][2])  
                }
                }
        }; rm(i); rm(tmp)
        lbs<-levels(factor(nx))
        #
        } else {
                mat <- matrix(0, ncol = length(labels), nrow = length(labels)) 
        lbs<-labels
        }
###
for(i in 1:length(lbs)) if(isTRUE(is.numeric(lbs[i])==FALSE)==TRUE) { lb2lb <- TRUE; break } else { NA }
###
        for (i in 1:length(x)) {
           ifelse(isTRUE(lb2lb == TRUE)== TRUE, 
            mat[which(strsplit(x[i], prsep)[[1]][1]==lbs),which(strsplit(x[i], prsep)[[1]][2]==lbs)] <- (mat[which(strsplit(x[i], prsep)[[1]][1]==lbs),which(strsplit(x[i], prsep)[[1]][2]==lbs)]+1), #<<===----Aqui tambien
            mat[as.numeric(strsplit(x[i], prsep)[[1]])[1], as.numeric(strsplit(x[i], prsep)[[1]])[2]] <- (mat[as.numeric(strsplit(x[i], prsep)[[1]])[1], as.numeric(strsplit(x[i], prsep)[[1]])[2]]+1))#<<===----Aqui
#            mat[which(strsplit(x[i], prsep)[[1]][1]==lbs),which(strsplit(x[i], prsep)[[1]][2]==lbs)] <- 1
        }
        rm(i)

#        if (is.null(ord) == FALSE) {
#  #          ifelse(is.null(labels) == FALSE, rownames(mat) <- colnames(mat) <- lbs, 
#                rownames(mat) <- colnames(mat) <- 1:ord#)
#        }
#        else {
#            ifelse(is.null(labels) == FALSE, rownames(mat) <- colnames(mat) <- lbs, 
#                rownames(mat) <- colnames(mat) <- 1:max(as.numeric(unlist(strsplit(levels(factor(unlist(x))), 
#                  prsep)))))
#        }

        if(isTRUE(length(lbs)==nrow(mat))==TRUE) rownames(mat) <- colnames(mat) <- lbs

        return(mat)
    }

}
