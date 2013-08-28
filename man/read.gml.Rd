\name{read.gml}
\alias{read.gml}
\title{
Read \code{gml} Files
}
\description{
A function to read files with the \code{gml} format.
}
\usage{
read.gml(file, directed = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{  character vector containing a file name or path
}
  \item{directed}{  (logical) whether the graph is directed or undirected.
}
}
\details{
The \code{gml} format, an acronym for \emph{graph modelling language}, provides capabilities to represent multiple networks and add arguments both to the nodes and the edges for visualization purposes.
}
\value{
An array representing the multigraph.
}
\references{
\code{visone} Software for the analysis and visualization of social networks. \emph{http://visone.info}
}
\author{
Antonio Rivero Ostoic
}
%\note{
%
%}
\seealso{
\code{\link{write.gml}}
}
%\examples{
%
%}
\keyword{ IO }
\keyword{ data }
\keyword{ file }
