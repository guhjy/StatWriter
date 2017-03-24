\name{pvalue}
\alias{pvalue}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Writes nicely formatted p-values
}
\description{
Prints out APA style p-values

}
\usage{
pvalue(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a object of class numeric
}

}
\details{
Returns the p-value round at 3 digits or <.001 if necessary

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
a string
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

d<-data.frame(cbind(y=rnorm(100,0,1),x=rnorm(100,0,1)))
p<-summary(lm(y~x,data=d))$coefficients[1,4]
pvalue(p)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Writer }
\keyword{ pvalue }% __ONLY ONE__ keyword per line
