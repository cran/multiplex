expos <-
function (rs, bh, adopters = FALSE) 
{
    if (isTRUE(attr(rs$ties, "class") == "Rel.System") == FALSE) 
        stop("Relational system must be a \"Rel.System\" class.")
    if (isTRUE(rs$sys.ord == 0) == TRUE) 
        stop("Relational system chosen is empty!")
    At <- data.frame(matrix(0, ncol = rs$ord, nrow = 0))
    if (isTRUE(is.array(bh) == TRUE) == TRUE) {
        if (dim(bh)[1] != rs$ord) 
            stop("'rs' and 'bh' have a different order")
        if (is.na(dim(bh)[3]) == FALSE) {
            for (i in 1:dim(bh)[3]) At[i, ] <- diag(bh[, , i])
        }
        else if (is.na(dim(bh)[3]) == TRUE) {
            At <- diag(bh)
        }
    }
    else if (isTRUE(is.data.frame(bh) == TRUE) == TRUE) {
        if (ncol(as.vector(bh)) != rs$ord) 
            stop("'rs' and 'bh' have a different order")
        ifelse(isTRUE(all(as.character(bh[, 1]) %in% rs$nodes)) == 
            TRUE, At <- t(bh[, 2:ncol(bh)]), At <- t(bh))
        if (is.null(attr(bh, "names")) == TRUE) 
            attr(At, "names") <- rs$nodes
    }
    else {
        stop("'bh' must be either data frame, vector, or array")
    }
    if (isTRUE(is.null(nrow(At)) == FALSE) == TRUE) {
        ifelse(is.null(rs$nodes) == TRUE, colnames(At) <- 1:rs$ord, 
            colnames(At) <- rs$nodes)
        rownames(At) <- LETTERS[1:nrow(At)]
    }
    ifelse(isTRUE(is.vector(At) == TRUE) == TRUE, at <- dichot(At), 
        at <- dichot(At[, 1:ncol(At)]))
    if (is.null(rs$incl) == FALSE) {
        ifelse(isTRUE(is.null(nrow(at)) == TRUE) == TRUE, at <- at[which(attr(at, 
            "names") %in% rs$incl)], at <- as.data.frame(at)[, 
            which(colnames(at) %in% rs$incl)])
    }
    adpt <- list()
    if (isTRUE(is.vector(at) == FALSE) == TRUE) {
        adpt[[1]] <- colnames(at)[setdiff(1:length(colnames(at)), 
            union(which(at[1, ] == 1), which(at[2, ] == 1)))]
        adpt[[2]] <- colnames(at)[setdiff(which(at[1, ] == 1), 
            which(at[2, ] == 1))]
        adpt[[3]] <- colnames(at)[setdiff(which(at[2, ] == 1), 
            which(at[1, ] == 1))]
        adpt[[4]] <- colnames(at)[intersect(which(at[1, ] == 
            1), which(at[2, ] == 1))]
        attr(adpt, "names") <- c("none", "only_A", "only_B", 
            "both_A_B")
    }
    else if (isTRUE(is.vector(at) == TRUE) == TRUE) {
        adpt[[1]] <- attr(which(at == 0), "names")
        adpt[[2]] <- attr(which(at != 0), "names")
        attr(adpt, "names") <- c("Non-adopters", "Adopters")
    }
    expA <- list()
    if (isTRUE(is.vector(at) == FALSE) == TRUE) {
        expB <- list()
        for (l in 1:(length(adpt) - 1)) {
            if (isTRUE(length(adpt[[l]]) > 0) == TRUE) {
                for (i in 1:length(adpt[[l]])) {
                  if (isTRUE(adpt[[l]][i] %in% ngbs(rs$ties, 
                    type = "und")) == TRUE) {
                    if (sum(slc(at, ngbs(slc(rs$ties, adpt[[l]][i]), 
                      type = "und"))) > 0) {
                      if (isTRUE(rs$bond.type == "weak") == TRUE) {
                        if (length(setdiff(ngbs(slc(rs$ties, 
                          adpt[[l]][i]), type = "dir"), adpt[[l]][i])) > 
                          0) {
                          tmp <- setdiff(ngbs(slc(rs$ties, adpt[[l]][i]), 
                            type = "dir"), adpt[[l]][i])
                          if (slc(at, adpt[[l]][i])[1] != 1) {
                            if (is.null(nrow(slc(at, tmp)))) {
                              if (slc(at, tmp)[1] != 0) 
                                expA[[(length(expA) + 1)]] <- paste(adpt[[l]][i], 
                                  slc(at, tmp)[1], sep = " - ")
                            }
                            else {
                              if ((round(sum(slc(at, tmp)[1, 
                                ])/ncol(slc(at, tmp)), 2)) != 
                                0) 
                                expA[[(length(expA) + 1)]] <- paste(adpt[[l]][i], 
                                  round(sum(slc(at, tmp)[1, ])/ncol(slc(at, 
                                    tmp)), 2), sep = " - ")
                            }
                          }
                        }
                      }
                      else {
                        if (slc(at, adpt[[l]][i])[1] != 1) {
                          tmp <- ngbs(slc(rs$ties, adpt[[l]][i]), 
                            type = "und")
                          if (sum(slc(at, tmp)[1, ])/(ncol(slc(at, 
                            tmp)) - 1) > 0) 
                            expA[[(length(expA) + 1)]] <- paste(adpt[[l]][i], 
                              round(sum(slc(at, tmp)[1, ])/(ncol(slc(at, 
                                tmp)) - 1), 2), sep = " - ")
                        }
                      }
                      if (isTRUE(rs$bond.type == "weak") == TRUE) {
                        if (length(setdiff(ngbs(slc(rs$ties, 
                          adpt[[l]][i]), type = "dir"), adpt[[l]][i])) > 
                          0) {
                          tmp <- setdiff(ngbs(slc(rs$ties, adpt[[l]][i]), 
                            type = "dir"), adpt[[l]][i])
                          if (slc(at, adpt[[l]][i])[2] != 1) {
                            if (is.null(nrow(slc(at, tmp)))) {
                              if (slc(at, tmp)[2] != 0) 
                                expB[[(length(expB) + 1)]] <- paste(adpt[[l]][i], 
                                  slc(at, tmp)[2], sep = " - ")
                            }
                            else {
                              if ((round(sum(slc(at, tmp)[2, 
                                ])/ncol(slc(at, tmp)), 2)) != 
                                0) 
                                expB[[(length(expB) + 1)]] <- paste(adpt[[l]][i], 
                                  round(sum(slc(at, tmp)[2, ])/ncol(slc(at, 
                                    tmp)), 2), sep = " - ")
                            }
                          }
                        }
                      }
                      else {
                        if (slc(at, adpt[[l]][i])[2] != 1) {
                          tmp <- ngbs(slc(rs$ties, adpt[[l]][i]), 
                            type = "und")
                          if (sum(slc(at, tmp)[2, ])/(ncol(slc(at, 
                            tmp)) - 1) > 0) 
                            expB[[(length(expB) + 1)]] <- paste(adpt[[l]][i], 
                              round(sum(slc(at, tmp)[2, ])/(ncol(slc(at, 
                                tmp)) - 1), 2), sep = " - ")
                        }
                      }
                    }
                  }
                }
                rm(i)
            }
        }
        rm(l)
    }
    else if (isTRUE(is.vector(at) == TRUE) == TRUE) {
        for (i in 1:length(adpt[[1]])) {
            if (isTRUE(adpt[[1]][i] %in% ngbs(rs$ties, type = "und")) == 
                TRUE) {
                if (sum(slc(at, ngbs(slc(rs$ties, adpt[[1]][i]), 
                  type = "und"))) > 0) {
                  if (isTRUE(rs$bond.type == "weak") == TRUE) {
                    if (length(setdiff(ngbs(slc(rs$ties, adpt[[1]][i]), 
                      type = "dir"), adpt[[1]][i])) > 0) {
                      tmp <- setdiff(ngbs(slc(rs$ties, adpt[[1]][i]), 
                        type = "dir"), adpt[[1]][i])
                      if (slc(at, adpt[[1]][i])[1] != 1) {
                        if (is.null(nrow(slc(at, tmp)))) {
                          if (slc(at, tmp)[1] != 0) 
                            expA[[(length(expA) + 1)]] <- paste(adpt[[l]][i], 
                              slc(at, tmp)[1], sep = " - ")
                        }
                        else {
                          if ((round(sum(slc(at, tmp))/length(slc(at, 
                            tmp)), 2)) != 0) 
                            expA[[(length(expA) + 1)]] <- paste(adpt[[l]][i], 
                              round(sum(slc(at, tmp))/length(slc(at, 
                                tmp)), 2), sep = " - ")
                        }
                      }
                    }
                  }
                  else {
                    if (slc(at, adpt[[1]][i])[1] != 1) {
                      tmp <- ngbs(slc(rs$ties, adpt[[1]][i]), 
                        type = "und")
                      if (sum(slc(at, tmp))/(length(slc(at, tmp)) - 
                        1) > 0) 
                        expA[[(length(expA) + 1)]] <- paste(adpt[[1]][i], 
                          round(sum(slc(at, tmp))/(length(slc(at, 
                            tmp)) - 1), 2), sep = " - ")
                    }
                  }
                }
            }
        }
    }
    if (isTRUE(is.vector(at) == FALSE) == TRUE) {
        exp <- list()
        length(exp) <- 2
        if (is.null(unlist(expA)) == FALSE) 
            exp[[1]] <- unlist(expA)
        if (is.null(unlist(expB)) == FALSE) 
            exp[[2]] <- unlist(expB)
        attr(exp, "names") <- c("to_A", "to_B")
        if (adopters) {
            return(list(Adoption = noquote(adpt), Exposure = noquote(exp)))
        }
        else {
            return(list(Exposure = noquote(exp)))
        }
    }
    else if (isTRUE(is.vector(at) == TRUE) == TRUE) {
        return(list(Adoption = noquote(adpt), Exposure = noquote(unlist(expA))))
    }
}
