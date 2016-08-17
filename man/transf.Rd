\name{transf}
\alias{transf}
\title{
Transform Data from/to Array to Pairwise List Formats
}
\description{
Function to transform data from/to array to pairwise list formats to represent the network.
}
\usage{
transf(x, type = c("tolist", "toarray"), lb2lb = FALSE, 
       labels = NULL, ord, prsep)
}
\arguments{
  \item{x}{
   a matrix or a list of pair relations
  }
  \item{type}{
   whether the transformation is from a matrix to a list of pair relations, or from a list of pair relations to an array format
  }
  \item{lb2lb}{
   (logical) whether the transformation is label-to-label
  }
  \item{labels}{
   (optional) the labels in the transformation
  }
  \item{ord}{
   (optional) the order of the resulted structure (`\code{toarray}' option, otherwise ignored)
  }
  \item{prsep}{
   (optional) the pair separator for the pairwise relations
  }
}
\details{
This is a function to transform the data representing the network from an array, matrix, or data frame into a `pairwise list' of relations and vice versa. Use argument `\code{tolist}' to transform a matrix to a list of pair elements, and `\code{toarray}' for the converse. In the former option there is the possibility to enable \code{lb2lb} to preserve the label names of the network members; otherwise their placement in the array will be given. For the latter option it is advisable to specify the order of the structure in case there are isolated actors.

Note that this function is designed for simple networks, and in order to perform a transformation of a multiple network that are represented by high dimensional arrays we use instead for the \code{\link{rel.sys}} function, which allows many more options.
}
\value{
Depending on the input data, the result is either a list of pair relations or a matrix of relations.
}
%\references{
%
%}
\author{
Antonio Rivero Ostoic
}
\note{
Data frames are also accepted for the `\code{tolist}' option, but in case that this information is given as a list of pair relations the output will be a square matrix.
}
\seealso{
\code{\link{read.srt}}, \code{\link{bundles}}, \code{\link{reduc}}, \code{\link{rel.sys}}
}
\examples{
## scan the multiplication table data
s <- matrix(data=c(1, 1, 1, 3, 3, 3, 3, 3, 3), nrow=3, ncol=3, byrow=TRUE)

## transform the matrix to a list format
transf(s, lb2lb = TRUE, labels = c('n','m','ñ'))
}
\keyword{ manip }
\keyword{ array }

