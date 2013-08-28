\name{partial.order}
\alias{partial.order}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The Partial Order of Strings of Relations
}
\description{
Construct the partial order table of unique relations of the semigroup.
}
\usage{
partial.order(x, labels = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
   an object of a `\code{Strings}' class
}
  \item{labels}{
   (optional) the labels of the unique relations
}
}
\details{
To get the partial order of an entire semigroup, both generators and compound relations must be considered. This information and the labels of the unique relations are given by the \code{\link{strings}} function. cf. \code{\link{semigroup}} to see how the \code{x} should be specified properly.
}
\value{
The partial order table in a matrix form.
}
\references{
Pattison, P.E. \emph{Algebraic Models for Social Networks}. Cambridge University Press. 1993.
}
\author{
Antonio Rivero Ostoic
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{as.strings}}, \code{\link{strings}}, \code{\link{perm}}, \code{\link{diagram}}.
}
\examples{
## Load the data, and obtain the partial order
data("incubA")

## the strings in the structure
st <- strings(incubA) 

## Get the partial order
partial.order(st)

}
%% Add one or more standard keywords, see file 'KEYWORDS' in the
%% R documentation directory.
\keyword{ algebra }
%
%