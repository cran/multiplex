\name{dichot}
\alias{dichot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Dichotomize data with a cutoff
}
\description{
Function to dichotomize the input data for the semigroup construction with a cutoff value.
}
\usage{
dichot(x, c = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
   some data in a numeric form (usually arrays).
}
  \item{c}{
   the cutoff value to perform the dichotomization.
}
}
\details{
This is basically the \code{\link[base:replace]{replace}} function here aimed to specify the cutoff value for the dichotomization of the data. The values equal or higher to the cutoff values are converted to one, otherwise they are set to zero. The default value of the cutoff is 1.
}
\value{
Binary values of the input data.
}
%\references{
%No hay...
%}
\author{
Antonio Rivero Ostoic
}
\note{
The labels are preserved after the dichotomization.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
\code{\link[base:replace]{replace}}, \code{\link{prev}}, \code{\link{semigroup}}.
}
\examples{
## Create the data: 2 binary relations among 3 elements
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
       c(3,3,2))>.5, 3 ) )

## dichotomize it with a cutoff value of 2
dichot(arr, c = 2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
%
