\name{ruminate.factor}
\alias{ruminate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Writer for numeric.
}
\description{
Prints out info about numeric variable printing out mean and standard deviation in english, feasable for be inserted in a paper

}
\usage{
ruminate(x,names=c("M","SD"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an object of class  \code{\link[base]{numeric}}
}
  \item{names}{
a character vector with the letters to be put before the mean and the sd.
}

}
\details{
Returns the mean and stardard deviation of a numeric variable

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

a<-rnorm(100,0,1)
ruminate(a)
ruminate(a,names=c("mean","s.d."))


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Writer }
\keyword{ ruminate }% __ONLY ONE__ keyword per line
