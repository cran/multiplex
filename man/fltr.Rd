\name{fltr}
\alias{fltr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Principal filter
}
\description{
A function to find principal filter in a partial order
}
\usage{
fltr(e, PO, rclos = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{e}{
the reference element in the partial order
}
  \item{PO}{
the partial order
}
  \item{rclos}{
(logical) apply reflexive closure?
}
}
\details{
This function helps to find principal filters in a partial order structure, or else principal ideals in case we use the transpose of the partial order.
}
\value{
A vector with elements belonging to the principal filter (or ideal) of the reference node in the partial order.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Antonio Rivero Ostoic
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{galois}}, \code{\link{partial.order}}, \code{\link{diagram}}
}
\examples{
## Create a data frame
dfr <- data.frame(x=1:3, y=5:7)

## Partial ordering of concepts
PO <- partial.order(galois(dfr),"galois")

## Filter for the first element
fltr(1, PO, rclos=TRUE)

}
\keyword{ algebra }
\keyword{ math }
