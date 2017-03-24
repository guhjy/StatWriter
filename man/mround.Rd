\name{mround}
\alias{mround}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
shortcut for round
}
\description{
shortcut for round with some nice advantage for writing. It does not loose the trailing zeros and add digits when necessary, or return the number in scientific notation when too many (12) digits are necessary to represent the first significant digit.
}
\usage{
mround(x, digits=2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a object of class numeric
}
  \item{digits}{
number of digits to print
}

}
\details{
This function is a modification of Karl Broman myround function https://github.com/kbroman/broman/blob/master/R/myround.R
Returns the value rounded at the indicated number of digits, preserving extra zeros and adding digists when necessary 
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
a numeric value 
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Marcello Gallucci
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

a<-.0809474782344
mround(a)
mround(0.8,digits=3)
mround(0.00008)
mround(0.000000000000000000000000000008)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Writer }
\keyword{ round }% __ONLY ONE__ keyword per line
