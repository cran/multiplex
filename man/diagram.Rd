\name{diagram}
\alias{diagram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot the Hasse Diagram of ordered relations
}
\description{
A function to plot the Hasse Diagram of partially ordered relations.
}
\usage{
diagram(x, unord = TRUE, attrs = NULL, main = NULL, 
        cex.main = par()$cex.main)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
   a matrix representing ordered relations
}
  \item{unord}{
   (logical) whether or not the elements not belonging in the order should be included in the diagram
}
  \item{attrs}{
   (optional) attributes of the diagram
}
  \item{main}{
   (optional) diagram's title
}
  \item{cex.main}{
   (optional) diagram's title size
}

}
\details{
An example of ordered relations is found in the partial order table of relations, which is product of the \code{\link{partial.order}} function. This function plot either the partial order or a linear order diagram, depending on the results as Hasse diagrams. Note that if the elements of the partial order are not labelled, Roman numerals will be given to each element.
}
\value{
A Hasse diagram of the partial order relation.
}
%\references{
%No hay...
%}
\author{
Antonio Rivero Ostoic
}
\note{
This function requires that the \emph{Rgraphviz} package is available.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{partial.order}}, \code{\link{as.strings}}, \code{\link{strings}}.
}
\examples{
## load the data
data("incubA")

## given e.g. a partial order table in the object 'po'
po <- partial.order(as.strings(incubA))


## plot the order relation as a Hasse diagram.
if(require(Rgraphviz)) {
plot(diagram(po))
}
}

\keyword{graphs}
%\keyword{dplot}
%
