\name{tvalue}
\alias{tvalue}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract t-value for writing
}
\description{
Extract t-value from glm and lmer model and  writes it in APA style
}
\usage{
tvalue(x,row)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a object of class glm, lm, or merModLmerTest
}
  \item{row}{
which row of the estimates table should be returned. Default is 2, which means the first effect after the intercept. Row can be numeric, indicating the position of the effect in the summary table, or a string, indicating the name of the effect.
}

}
\details{
Returns the t value with degrees of freedom and p-value. Models can be fromclass \code{\link[stats]{glm}}, \code{\link[stats]{lm}}, or \code{\link[lmerTest]{merModLmerTest-class}}

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

dat<-as.data.frame(cbind(x=rnorm(100,0,1),y=rnorm(100,0,1)))
mod<-lm(y~x,dat)
tvalue(mod) #for intercept and b
tvalue(mod,2) #only for b
tvalue(mod,"x") #passing the variable name


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Writer }
\keyword{ round }% __ONLY ONE__ keyword per line
