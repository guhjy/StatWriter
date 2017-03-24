\name{ruminate}
\alias{ruminate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Writer for simple objects.
}
\description{
Prints out info about an object in english feasable for be inserted in a paper.  It is a generic function which means that new printing methods can be easily added for new classes.

}
\usage{
ruminate(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a object 
}

}
\details{
Returns the object in a reasonable way

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
# a numeric variable 
a<-rnorm(100,0,1)
ruminate(a)
ruminate(a,names=c("mean","s.d."))

# a factor
a<-factor(rep(1:2,15))
levels(a)<-c("Black","Yellow")
ruminate(a)
levels(a)<-c("Male","Female")
ruminate(a,plural=T)

# a table
a<-table(rep(1:2,15))
names(a)<-c("Black","Yellow")
ruminate(a)
names(a)<-c("Male","Female")
ruminate(a,plural=T)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Writer }
\keyword{ ruminate }% __ONLY ONE__ keyword per line
