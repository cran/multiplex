\name{expos}
\alias{expos}
\title{
Network Exposure for Multiple Networks
}
\description{
Function to measure the network exposure of the nodes accroding to a chosen relational system representing a multiple network.
}
\usage{
expos(rs, bh, adopters = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rs}{
   an object of `\code{Rel.System}'
}
  \item{bh}{
   a data frame, or a diagonal matrix of binary data with the behaviour of the nodes
}
  \item{adopters}{
   (optional) whether the adopters should be included or not in the output
}
}
\details{
This is a generalization of the network exposure measure for multiple networks with the characteristics chosen for the representative relational system. It is possible to specify up to two different behaviours for the nodes, from which the measure is calculated.
}
\value{
%%
\item{Adoption}{the adoption membership for the type of relational system chosen}
\item{Exposure}{the exposure to the behaviour}
}
\references{
Ostoic, J.A.R. Algebraic Methods for the Analysis of Multiple Social Networks and Actors Attributes. PhD thesis. University of Southern Denmark. 2013.

Valente, T. W. \emph{Social network and health}. Oxford University Press. 2010.

Rogers, E. \emph{The Diffusion of Innovations}. 5th ed. (1st ed. 1964) The Free Press. 2003.
}
\author{
Antonio Rivero Ostoic
}
\note{
In case of specifying more than two types of behaviour, only the first two are considered in the computation, where both the data frame and the diagonal matrix are dichotomized.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{rel.sys}}
}
\examples{
## Create the data: two binary relations among three elements
arr <- round( replace( array( runif(18), c(3 ,3, 2) ), array( runif(18),
c(3, 3, 2) ) > .9, 3 ) )

## Calculate the exposure measure for two behaviours
expos(rel.sys(arr), arr)

}
\keyword{ attribute }
\keyword{ models }

