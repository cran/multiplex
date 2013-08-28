\name{orden}
\alias{orden}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
internal function
}
\description{
internal function
}
%\usage{
%orden(Pr, prsep = ", ")
%}
%%- maybe also 'usage' for other objects documented here.
%\arguments{
%  \item{Pr}{
%%%     ~~Describe \code{Pr} here~~
%}
%  \item{prsep}{
%%%     ~~Describe \code{prsep} here~~
%}
%}
%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%%  ~Describe the value returned
%%%  If it is a LIST, use
%%%  \item{comp1 }{Description of 'comp1'}
%%%  \item{comp2 }{Description of 'comp2'}
%%% ...
%}
%\references{
%%% ~put references to the literature/web site here ~
%}
%\author{
%%%  ~~who you are~~
%}
%\note{
%%%  ~~further notes~~
%}
%
%%% ~Make other sections like Warning with \section{Warning }{....} ~
%
%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%
%## The function is currently defined as
%function (Pr, prsep = ", ") 
%{
%    for (i in 1:length(Pr)) {
%        if (isTRUE(as.numeric(strsplit(Pr[i], prsep)[[1]][1]) > 
%            as.numeric(strsplit(Pr[i], prsep)[[1]][2])) == TRUE) 
%            Pr[i] <- swp(Pr[i])
%    }
%    rm(i)
%    return(Pr)
%  }
%}
%% Add one or more standard keywords, see file 'KEYWORDS' in the
%% R documentation directory.
\keyword{internal}
