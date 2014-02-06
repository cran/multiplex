\name{transf}
\alias{transf}
\title{
Transform Data from/to Matrix/List Formats
}
\description{
Function to transform data from/to matrix/list formats representing a network.
}
\usage{
transf(x, type = c("matlist", "listmat"), lb2lb = FALSE, 
       labels = NULL, prsep = ", ", ord = NULL)
}
\arguments{
  \item{x}{
   a matrix or a list of pair relations
  }
  \item{type}{
   whether the transformation is from a matrix to a list of pair relations, or from a list of pair relations to a matrix form
  }
  \item{lb2lb}{
   (logical) whether the transformation is label-to-label
  }
  \item{labels}{
   the labels in the transformation
  }
  \item{prsep}{
   the pair separator
  }
  \item{ord}{
   the order of the resulted structure
  }
}
\details{
`\code{matlist}' is the option to transform a matrix to a list of pair elements. In case that the \code{lb2lb} is enabled in this type of transformation, then \code{labels} must be provided, whereas the pair separator is optional. On the other hand `\code{listmat}' will produce a matrix from a list of pair elements, and in this case is advisable to specify the order of the structure.
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
High dimensional arrays are not accepted as input data. Use \code{\link{rel.sys}} for that.
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

