\name{rel.sys}
\alias{rel.sys}
\title{
Relational System
}
\description{
Create the Relation System of a multiple network.
}
\usage{
rel.sys(x, bonds = c("entire", "strong", "weak"), prsep = ", ")
}
\arguments{
  \item{x}{
   an array; usually with three dimensions of stacked matrices where the multiple relations are placed.
}
  \item{bonds}{
   the type of bonds to be used in the creation of the relational system
}
  \item{prsep}{
   (optional) the pair separator for the pairwise relations
  }
}
\details{
When the type of bonds chosen is \code{entire} then the nodes with ties are considered in the relational system without isolated nodes. \code{strong} bonds are relational bundles with a mutual character, whereas \code{weak} bonds are those pattern exclusively without mutual character.

}
\value{
An object of `\code{Rel.System}' class.
The items are:
   \item{ord}{order of the network relational system}
   \item{nodes}{the nodes in the relational system}
   \item{sys.ord}{the order of the relational system with the chosen bond type}
   \item{incl}{the nodes included the relational system with the chosen bond type}
   \item{excl}{the nodes excluded the relational system with the chosen bond type}
   \item{bond.type}{the type of bonds used in the relational system creation}
   \item{size}{number of ties in the relational system}
   \item{ties}{the ties in the relational system}
%% ...
}
%\references{
%
%}
\author{
Antonio Rivero Ostoic
}
%\note{
%
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{expos}}, \code{\link{bundles}}
}
\examples{
## Create the data: two binary relations among three elements
arr <- round( replace( array( runif(18), c(3 ,3, 2) ), array( runif(18),
       c(3, 3, 2) ) > .9, 3 ) )

## Determine the system of strong bonds
rel.sys(arr, bonds="strong")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }

