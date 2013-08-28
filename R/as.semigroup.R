as.semigroup <-
function(x, labels=NULL) {

if(is.array(x) == FALSE && is.data.frame(x) == FALSE) stop("Data must be a square matrix or data frame")

S <- as.data.frame(x)

if(suppressWarnings(NA%in%(as.numeric(attr(x,"names"))))==FALSE) {
	lbs <- 1:nrow(S)
} else {
	lbs <- rownames(S)
}
#, class(lst)<-c("Semigroup", "symbolic"), class(lst)<-c("Semigroup", "numerical") )
#ifelse(isTRUE(is.null(rownames(S)==TRUE)==TRUE), ,  )
#if(is.numeric(x)==FALSE) { lbs <- colnames(x) } else 

ifelse(isTRUE(is.null(labels)==TRUE)==FALSE,  lbs <- labels, NA )

lst<-list(ord=nrow(S),st=lbs, S=S)
#ifelse(is.null(labels)==FALSE, lst<-list(ord=nrow(S),st=lbs, S=S), lst<-list(ord=nrow(S),st<-labels,S=S) )

ifelse(suppressWarnings(NA%in%(as.numeric(attr(x,"names"))))==TRUE, class(lst)<-c("Semigroup", "symbolic"), class(lst)<-c("Semigroup", "numerical") )

    return(lst)

}
