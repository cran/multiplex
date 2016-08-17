\name{multiplex-package}
\alias{multiplex-package}
%\alias{multiplex}
\docType{package}
\title{
Algebraic Tools for the Analysis of Multiple Social Networks
}
\description{
One of the aims of the `\code{multiplex}' package is to meet the necessity to count with an analytic tool specially designed for social networks with relations at different levels. In this sense, `\code{multiplex}' counts with functions that models the local role algebras of the network based on the simple and compound relations existing in the system, and also a procedure for the construction and analysis of signed networks through the semiring structure. The different relational patterns at the dyadic level in the network can be obtained as well, which can serve for a further analysis with different types of structural theories.

It is also possible to take the attributes of the actors in the analysis of multiple networks with different forms to incorporate this kind of information to the existing relational structures. In this case for example the network exposure of the actors can be taken in the context of multiple networks, or else the attributes can be embedded in the resulted algebraic structures.
}
\details{
\tabular{ll}{
Package: \tab multiplex\cr
Type: \tab Package\cr
Version: \tab 2.0\cr
Date: \tab 16 August 2016\cr
License: \tab GPL-3\cr
LazyLoad: \tab yes\cr
}
To work with this package we typically start with a specific algebraic system. A semigroup is a closed system made of a set of elements and an associative operation on it. This algebraic structure is constructed by the \code{semigroup} function, and it takes an array of (usually but not necessarily) multiple binary relations, which are the generator relations. The Word Table and the Edge Table serve to describe completely the semigroup, and they are constructed with the functions \code{wordT} and \code{edgeT} respectively. Unique relations of the complete semigroup are given by the \code{strings} function. The \code{partial.order} function specifies --among other things-- the ordering of the string elements in the semigroup. The function \code{diagram} produces the lattice of inclusions of a structure having ordered relations.

Semigroups can be analysed further by \code{ltlw} function, and they also can be reduced by a decomposition process. The decomposition is based on congruence or \eqn{\pi}-relations of the unique strings imported from Pacnet. In this case \code{pi.rels}, \code{cngr}, and \code{decomp} will make this job for you either for an abstract or a partially ordered semigroup.

In addition, it is possible to analyse structural balance in signed networks, which are built either by \code{signed} or \code{as.signed}, through the algebraic structure of the semiring. A semiring is an algebraic structure that combines an abstract semigroup with identity under multiplication and a commutative monoid under addition. The \code{semiring} function is capable to perform both balance and cluster semiring either with paths or with semipaths.

There are other capabilities in the package that are not strictly algebraic. For instance, the \code{dichot} serves to dichotomize the input data with a specified cut-off value, \code{rm.isol} removes isolated nodes, \code{comps} identifies components and isolates in the network, and the \code{perm} function performs an automorphism of the elements in the representative array. All these functions are built for multiple networks represented by high dimensional structures that can be constructed by the function \code{zbind}.

The `\code{multiplex}' package creates a Relation-Box with the \code{rbox} function, and it implements the Partial Structural Equivalence expressed in the cumulated person hierarchy of the network calculated via the \code{cph} function.

Relational bundles are identified through the \code{bundles} function, which provides lists of pair relations, whereas \code{bundle.census} and \code{summaryBundles} provide the enumeration of the different bundle classes in the system. An advantage of counting with the bundle patterns is that the different types of bundles serve to establish a system inside the network, which can be extracted through \code{rel.sys}, and it constitutes the basis for measuring the network exposure in multivariate relational systems via \code{expos}.

Actor attributes can be analysed through Galois derivation that is derived by \code{galois}, and which provides an algebraic approach for two-mode networks. The output of this function allows constructing the Concept Lattice again with \code{diagram} provided that \code{partial.order} constructs the partial order set of this type of structure.

Besides, multivariate network data can be created through the (s)end (r)eceive (t)ies format that can be loaded and transformed via the \code{read.srt} function. Other formats for multiple network data like Ucinet \code{dl} or Visone \code{gml} can be imported and exported as well with the `\code{multiplex}' package.
}
%
\author{
J. Antonio Rivero Ostoic

Maintainer: Antonio Rivero Ostoic <multiplex@post.com>
}
\references{
Lorrain, François and Harrison C. White, `Structural equivalence of individuals in social networks.' \emph{Journal of Mathematical Sociology}, 1, 49-80. 1971.

Boorman, Scott A. and Harrison C. White, `Social structure from multiple networks. II. Role structures.' \emph{American Journal of Sociology}, 81 (6), 1384-1446. 1976.

Boyd, John P. \emph{Social Semigroups. A unified theory of scaling and blockmodelling as applied to social networks}. George Mason University Press. 1991.

Pattison, Philippa E. \emph{Algebraic Models for Social Networks}. Cambridge University Press. 1993.

Ganter, B. and R. Wille  \emph{Formal Concept Analysis -- Mathematical Foundations}. Springer. 1996.
}

%\seealso{
%
%}

\examples{
## Create the data: two binary relations among three elements
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
       c(3,3,2))>.5, 3 ) )

## Dichotomize it with customized cutoff value
dichot(arr, c = 3)

## preview
prev(arr)

## create the semigroup (elay...)
semigroup(arr)

## and look at the strings
strings(arr)

}
\keyword{ models }
\keyword{ attribute }
\keyword{ manip }
\keyword{ IO }
\keyword{ data }
\keyword{ file }
