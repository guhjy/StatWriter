\name{ruminate.factor}
\alias{ruminate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrtier for factors
}
\description{
Prints out info about factor by computing an one-way frequency and prints out proportions or frequencies in english feasable for be inserted in a paper

}
\usage{
ruminate(x,plural=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an object of class  \code{\link[base]{factor}}
}
  \item{plural}{
adds an "s" to each name
}

}
\details{
Returns each level in a factor with its frequency or proportion

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

a<-factor(rep(1:2,15))
levels(a)<-c("Black","Yellow")
ruminate(a)
levels(a)<-c("Male","Female")
ruminate(a,plural=T)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Writer }
\keyword{ ruminate }% __ONLY ONE__ keyword per line
