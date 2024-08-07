\name{rel.sys}
\alias{rel.sys}
\title{
Relational System
}
\description{
Create the Relation System of a multiplex network.
}
\usage{
rel.sys(x, type = c("tolist", "toarray"), bonds = c("entire", "strong", "weak", 
        "asym", "recp", "txch", "tent", "mixd", "full"), loops = FALSE, 
        sel = NULL, att = NULL, sep)
}
\arguments{
  \item{x}{
   an array; usually with three dimensions of stacked matrices where the multiple relations are placed.
}
  \item{type}{
   if the transformation is from 
  \itemize{ 
    \item \code{tolist} for (array of) matrices into lists of pairwise relations
    \item \code{toarray} for lists of pairwise relations into (array of) matrices
  }
 }
  \item{bonds}{
   the type of bonds to be used in the creation of the relational system 
  \itemize{ 
    \item \code{entire} for whole network (default, same as \code{full})
    \item \code{strong} for strong bonds
    \item \code{weak} for weak bonds
    \item \code{asym} for asymmetric ties
    \item \code{recp} for reciprocal ties
    \item \code{txch} for tie exchange bundles
    \item \code{tent} for tie entrainment bundles
    \item \code{mixd} for mixed bundles
    \item \code{full} for the `entire' network (same as \code{entire})
  }
}
  \item{loops}{
   (logical) whether or not the loops should be considered in the relational system
}
  \item{sel}{
   (optional) the set of actors to be selected
}
  \item{att}{
   array(s) in \code{x} that correspond(s) to node attributes
}
  \item{sep}{
   (optional) the pair separator for the pairwise relations
  }
}
\details{
When the type of bonds chosen is \code{entire} then the nodes with ties are considered in the relational system without isolated nodes. \code{strong} bonds are relational bundles with a mutual character, whereas \code{weak} bonds are those patterns exclusively without mutual character.

When choosing from a list with actor attributes, it is also possible to select the network members having or \emph{not} having the attribute that is specified in the \code{Attrs} output by using in argument \code{sel}.
}
\value{
An object of `\code{Rel.System}' class for the \code{type = "tolist"} (default) option.
The items are:
   \item{ord}{order of the network relational system}
   \item{nodes}{the nodes in the relational system}
   \item{sel}{the selected set of actors}
   \item{sys.ord}{the order of the relational system with the chosen bond type}
   \item{incl}{the nodes included the relational system with the chosen bond type}
   \item{excl}{the nodes excluded the relational system with the chosen bond type}
   \item{bond.type}{the type of bonds used in the relational system creation}
   \item{size}{number of ties in the relational system}
   \item{Note}{(optional) note}
   \item{sep}{the pairwise separator of the relational system}
   \item{Ties}{the ties in the relational system}
   \item{Attrs.ord}{if \code{att} is not \code{NULL}, the number of nodes with the chosen attribute(s) }
   \item{Attrs}{if \code{att} is not \code{NULL}, the actors with the chosen attribute(s)}

For \code{type = "toarray"} the output is a dichotomous 2D or 3D array recording the relations among the actors in the network.
}
\references{
Ostoic, J.A.R. ``Creating context for social influence processes in multiplex networks.'' \emph{Network Science}, 5(1), 1-29.
}
\author{
Antonio Rivero Ostoic
}
%\note{
%}

%

\seealso{
\code{\link{expos}}, \code{\link{bundles}}, \code{\link{neighb}}
}
\examples{
## create the data: two binary relations among three elements
arr <- round( replace( array( runif(18), c(3 ,3, 2) ), array( runif(18),
       c(3, 3, 2) ) > .9, 3 ) )

## system of strong bonds
rel.sys(arr, bonds = "strong")

## first array is for attributes
rel.sys(arr, att = 1)

}

\keyword{models}
\keyword{attribute}

