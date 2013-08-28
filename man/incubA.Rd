\name{incubA}
\docType{data}
\alias{incubA}
\alias{incubB}
\alias{incubC}
\alias{incubD}
\title{
Business Incubator Data Sets
}
\description{
Each of this data set is a blockmodel having multiple relations between firms working in a business incubator in Denmark. 

The data comprises three types of relationships and one type of attribute; the relations are cooperation (\code{C}), friendship (\code{F}), and perceived competition (\code{K}), their transposes with the following letter in the Latin alphabet, whereas the attribute is the adoption of a Web innovations by the firms that is coded either as (\code{A}) or (\code{B}).

For each type of relation, its is also given thier transposes, which are marked by the following letter of their codes in the alphabet. The data was collected in year 2010; see `source' for the details.
}
\usage{
data("incubA")
data("incubB")
data("incubC")
data("incubD")
}
\format{
\code{incubA} is an array \eqn{4 \times 4 \times 8} representing eight types of relationships between four clusters of actors.

\code{incubB} is an array \eqn{4 \times 4 \times 8} representing eight types of relationships between four clusters of actors.

\code{incubC} is an array \eqn{3 \times 3 \times 8} representing eight types of relationships between three clusters of actors.

\code{incubD} is an array \eqn{4 \times 4 \times 6} representing six types of relationships between four clusters of actors.
}
\source{
Ostoic, J.A.R.  `Algebraic methods for the analysis of multiple social networks and actors attributes'' PhD Thesis. University of Southern Denmark. 2013.
}
%\references{
%}
\keyword{datasets}
